##############################################################################
# OPAL – Shiny App  |  Author view & Institution view (map)
##############################################################################

library(shiny)
library(visNetwork)
library(leaflet)
library(this.path)
library(dplyr)
library(scales)
library(tidygeocoder)

setwd(this.dir())
`%nin%` <- Negate(`%in%`)

# ----------------------------------------------------------------------------
# 0) Settings
# ----------------------------------------------------------------------------
cache_file  <- "inst_coords_cache.rds"
vienna_name <- "Vienna University of Economics and Business"

# ----------------------------------------------------------------------------
# 1) Load & prepare data
# ----------------------------------------------------------------------------
nodes        <- read.csv2("author_nodes_new.csv", stringsAsFactors = FALSE)
edges        <- read.csv2("author_edges_new.csv",    stringsAsFactors = FALSE)
institutions <- readRDS("downloaded_institutions_combined.rds")

nodes$author_id <- as.character(nodes$author_id)
colnames(edges)  <- c("from","to","freq")
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)

# attach external EXT_ IDs to institution records
external_ids    <- nodes[grepl("^EXT_", nodes$id) & nodes$label!="NA NA", ]$id
institutions$id <- external_ids

# build internal “Vienna U.” entries
internal_names <- nodes[grepl("^A_", nodes$id), ]$label
internal_ids   <- nodes[grepl("^A_", nodes$id), ]$author_id
internal_institutions <- data.frame(
  label        = internal_names,
  countries    = "AT",
  institutions = vienna_name,
  id           = internal_ids,
  stringsAsFactors = FALSE
)

# dedupe legacy institutions by (id,label)
institutions <- institutions %>%
  rename(label = name) %>%
  distinct(id, label, .keep_all = TRUE)

# combine and re-dedupe
final_institutions <- bind_rows(internal_institutions, institutions) %>%
  distinct(id, label, .keep_all = TRUE)

# fix Unknown fields
final_institutions$countries    [final_institutions$countries    == 0] <- "Unknown"
final_institutions$institutions [final_institutions$institutions == 0] <- "Unknown"
colnames(final_institutions) <- c("label","countries","institutions","id")

# merge nodes → authors + institution info
nodes <- nodes %>%
  filter(label!="NA NA") %>%
  inner_join(final_institutions,
             by = c("id","label"),
             relationship = "many-to-many") %>%
  distinct(author_id, .keep_all = TRUE)

# collapse duplicate edges
edges <- edges %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(from,to)), collapse=":")) %>%
  ungroup() %>%
  group_by(pair) %>%
  summarise(
    from = strsplit(pair,":")[[1]][1],
    to   = strsplit(pair,":")[[1]][2],
    freq = sum(freq),
    .groups = "drop"
  )

# build author tooltip
nodes$title <- paste0(
  "Name: ", nodes$label,
  "<br>Publications: ", nodes$pubs,
  "<br>Country: ", nodes$countries,
  "<br>Institution: ", nodes$institutions
)

# ----------------------------------------------------------------------------
# 2) Build institution-to-institution network + geocoding cache
# ----------------------------------------------------------------------------
inst_edges <- edges %>%
  left_join(nodes[,c("id","institutions")],
            by = c("from"="id")) %>%
  rename(inst_from = institutions) %>%
  left_join(nodes[,c("id","institutions")],
            by = c("to"="id")) %>%
  rename(inst_to   = institutions) %>%
  filter(inst_from != inst_to) %>%
  group_by(inst_from, inst_to) %>%
  summarise(freq = sum(freq), .groups="drop")

# load or initialize geocode cache
inst_coord <- if (file.exists(cache_file)) {
  readRDS(cache_file)
} else {
  data.frame(
    address   = character(),
    latitude  = double(),
    longitude = double(),
    stringsAsFactors = FALSE
  )
}

# geocode any missing
inst_tbl <- nodes %>% distinct(institutions) %>% rename(address=institutions)
pending  <- inst_tbl %>% filter(address %nin% inst_coord$address)
if (nrow(pending)>0) {
  message("Initial geocoding of ",nrow(pending)," institutes…")
  new_coords <- geocode(
    pending, address,
    method="osm", lat="latitude", long="longitude"
  )
  inst_coord <- bind_rows(inst_coord, new_coords) %>%
    distinct(address, .keep_all=TRUE)
  saveRDS(inst_coord, cache_file)
  message("→ Cache now has ",nrow(inst_coord)," entries.")
}

# ----------------------------------------------------------------------------
# 3) UI
# ----------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(div(img(src="logo.png"), "Welcome to OPAL")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("view_mode","Top-level view:",
                   choices = c("Author view","Institution view"),
                   inline  = TRUE),
      
      uiOutput("dynamic_node_picker"),
      uiOutput("dynamic_net_switch"),
      uiOutput("inst_selector"),
      uiOutput("dynamic_slider")
    ),
    mainPanel(
      conditionalPanel("input.view_mode=='Author view'",
                       visNetworkOutput("filteredGraph",height="550px")),
      conditionalPanel("input.view_mode=='Institution view'",
                       leafletOutput("inst_map",height="550px"))
    )
  )
)

# ----------------------------------------------------------------------------
# 4) Server
# ----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # -- dynamic author vs. institution selector --
  output$dynamic_node_picker <- renderUI({
    if (input$view_mode=="Author view") {
      selectInput("selected_node","Select an author:",
                  choices = nodes$label, selected = nodes$label[1])
    } else {
      # only show institutions with coords
      avail <- intersect(
        unique(nodes$institutions),
        inst_coord %>% filter(!is.na(latitude),!is.na(longitude)) %>% pull(address)
      )
      selectizeInput("selected_inst","Select an institution:",
                     choices = sort(avail),
                     selected = sort(avail)[1],
                     options = list(server=TRUE))
    }
  })
  
  # -- show‐institutes filter (author view only) --
  output$inst_selector <- renderUI({
    req(input$view_mode=="Author view", input$selected_node)
    ego_id <- nodes$author_id[nodes$label==input$selected_node]
    ce     <- subset(edges, from==ego_id|to==ego_id)
    ids    <- unique(c(ce$from,ce$to,ego_id))
    insts  <- nodes %>% filter(id%in%ids) %>% pull(institutions) %>% unique()
    checkboxGroupInput("inst_filter","Show institutes:",
                       choices=insts, selected=insts)
  })
  
  # -- author‐centric network switch --
  output$dynamic_net_switch <- renderUI({
    req(input$view_mode=="Author view")
    radioButtons("net_mode","Author-centric network:",
                 choices=c("People network","Institution network"),
                 selected="People network")
  })
  
  # -- slider: pubs vs. collabs --
  output$dynamic_slider <- renderUI({
    if (input$view_mode=="Author view") {
      sliderInput("pubs","Max Publications:",
                  min=min(nodes$pubs),
                  max=max(nodes$pubs),
                  value=max(nodes$pubs),
                  step=1)
    } else {
      req(input$selected_inst)
      maxc <- inst_edges %>%
        filter(inst_from==input$selected_inst|inst_to==input$selected_inst) %>%
        pull(freq) %>% max(na.rm=TRUE)
      sliderInput("collab","Max Collaborations:",
                  min=1, max=maxc, value=maxc, step=1)
    }
  })
  
  # --- render author view network ---
  output$filteredGraph <- renderVisNetwork({
    req(input$view_mode=="Author view", input$selected_node)
    
    # extract ego‐network for the selected author
    ego_id <- nodes$author_id[nodes$label==input$selected_node]
    ce  <- subset(edges, from==ego_id|to==ego_id)
    ids <- unique(c(ce$from,ce$to,ego_id))
    fn  <- subset(nodes, id%in%ids & (pubs<=input$pubs | id==ego_id))
    fn  <- fn %>% filter(institutions %in% input$inst_filter)
    fe  <- subset(ce, from%in%fn$id & to%in%fn$id)
    
    if (input$net_mode=="Institution network") {
      # keep one central node + one per partner institution
      main_node <- fn[fn$id==ego_id,]
      inst_nodes <- fn[fn$id!=ego_id,] %>%
        distinct(institutions,.keep_all=TRUE) %>%
        mutate(
          id    = paste0("inst__",institutions),
          label = institutions,
          title = paste0("Institution: ",institutions)
        )
      fn <- bind_rows(main_node,inst_nodes)
      fe <- inst_nodes %>% transmute(from=ego_id,to=id,freq=1)
    }
    
    if (nrow(fe)>0) {
      fe <- fe %>%
        mutate(
          width = scales::rescale(freq,to=c(1,10)),
          title = paste("Collaborations:",freq)
        )
    }
    
    visNetwork(fn,fe) %>%
      visLayout(randomSeed=1) %>%
      visEdges(smooth=TRUE) %>%
      visPhysics(
        solver="forceAtlas2Based",
        forceAtlas2Based=list(
          gravitationalConstant=-60,
          centralGravity=0.015,
          springLength=230,
          springConstant=0.08
        ),
        stabilization=FALSE
      ) %>%
      visEvents(stabilizationIterationsDone=
                  "function(){ this.fit({animation:false}); }")
  })
  
  # --- render institution view map ---
  output$inst_map <- renderLeaflet({
    req(input$view_mode=="Institution view", input$selected_inst)
    centre <- input$selected_inst
    
    link_tbl <- inst_edges %>%
      filter((inst_from==centre|inst_to==centre) & freq<=input$collab) %>%
      mutate(partner=ifelse(inst_from==centre,inst_to,inst_from))
    
    # on‐the‐fly geocode any new partners
    all_needed <- unique(c(centre,link_tbl$partner))
    to_geo    <- setdiff(all_needed,
                         inst_coord %>% filter(!is.na(latitude)) %>% pull(address))
    if (length(to_geo)>0) {
      newc <- geocode(
        data.frame(address=to_geo,stringsAsFactors=FALSE),
        address,method="osm",lat="latitude",long="longitude"
      )
      inst_coord <<- bind_rows(inst_coord,newc) %>%
        distinct(address,.keep_all=TRUE)
      saveRDS(inst_coord,cache_file)
    }
    
    pts         <- inst_coord %>%
      filter(address%in%all_needed & !is.na(latitude))
    centre_pt   <- pts %>% filter(address==centre)
    partner_pts <- pts %>% filter(address!=centre)
    vienna_pt   <- partner_pts %>% filter(address==vienna_name)
    other_pts   <- partner_pts %>% filter(address!=vienna_name)
    
    m <- leaflet() %>% addTiles()
    
    # center marker
    if (nrow(centre_pt)>0) {
      m <- m %>% addCircleMarkers(
        lng=centre_pt$longitude,lat=centre_pt$latitude,
        popup=centre,radius=10,color="red",fillOpacity=0.9
      )
    }
    
    # other partners
    if (nrow(other_pts)>0) {
      m <- m %>% addCircleMarkers(
        lng=other_pts$longitude,lat=other_pts$latitude,
        popup=paste0(
          other_pts$address,"<br>Collaborations: ",
          link_tbl$freq[match(other_pts$address,link_tbl$partner)]
        ),
        radius=6,color="steelblue",fillOpacity=0.7
      )
    }
    
    # Vienna highlight
    if (nrow(vienna_pt)>0) {
      m <- m %>% addCircleMarkers(
        lng=vienna_pt$longitude,lat=vienna_pt$latitude,
        popup=paste0(
          vienna_name,"<br>Collaborations: ",
          link_tbl$freq[match(vienna_name,link_tbl$partner)]
        ),
        radius=8,color="orange",fillOpacity=0.9
      )
    }
    
    # draw lines
    if (nrow(centre_pt)>0 && nrow(partner_pts)>0) {
      for (i in seq_len(nrow(partner_pts))) {
        p <- partner_pts$address[i]
        col <- if (p==vienna_name) "orange" else "gray"
        w   <- if (p==vienna_name) 3 else 1
        op  <- if (p==vienna_name) 0.8 else 0.6
        m <- m %>% addPolylines(
          lng=c(centre_pt$longitude,partner_pts$longitude[i]),
          lat=c(centre_pt$latitude,partner_pts$latitude[i]),
          color=col,weight=w,opacity=op
        )
      }
    }
    
    # auto-zoom
    if (nrow(centre_pt)>0 && nrow(partner_pts)>0) {
      all_lng <- c(centre_pt$longitude,partner_pts$longitude)
      all_lat <- c(centre_pt$latitude, partner_pts$latitude)
      m <- m %>% fitBounds(
        lng1=min(all_lng),lat1=min(all_lat),
        lng2=max(all_lng),lat2=max(all_lat)
      )
    } else if (nrow(centre_pt)>0) {
      m <- m %>% setView(
        lng=centre_pt$longitude,lat=centre_pt$latitude,zoom=4
      )
    } else if (nrow(partner_pts)>0) {
      m <- m %>% fitBounds(
        lng1=min(partner_pts$longitude),
        lat1=min(partner_pts$latitude),
        lng2=max(partner_pts$longitude),
        lat2=max(partner_pts$latitude)
      )
    }
    
    m
  })
  
}

# launch app
shinyApp(ui, server)
