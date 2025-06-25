##############################################################################
# OPAL – Shiny App  |  Merged App

# !!! WARNING !!!
# This app is in the process of merging and most functions might not work
##############################################################################

library(shiny)
library(visNetwork)
library(leaflet)
library(this.path)
library(dplyr)
library(scales)
library(tidygeocoder)
library(countrycode)
library(fuzzyjoin)

setwd(this.dir())
`%nin%` <- Negate(`%in%`)

clean_name <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9\\s-]", "", x)
  x <- gsub("\\s+", " ", x)
  x <- trimws(x)               
  return(x)
}

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

qsranking <- read.csv("qsranking.csv")
qsranking <- subset(qsranking, select = c(index, rank_2025, rank_2024, institution_name, location))
qsranking$location <- countrycode(qsranking$location, origin = "country.name.en", destination = "iso2c")
colnames(qsranking) <- c('index', 'rank_2025', 'rank_2024', 'institutions', 'location')

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

nodes$continent <- countrycode(sourcevar = nodes[, "countries"],
                               origin = "iso2c",
                               destination = "continent")

nodes$continent[which(is.na(nodes$continent))] <- "Unknown"


#qs implementation
nodes_qsranking <- subset(nodes, select = c(id, institutions, countries))

nodes_qsranking$institutions <- clean_name(nodes_qsranking$institutions)
qsranking$institutions <- clean_name(qsranking$institutions)

result <- stringdist_left_join(nodes_qsranking, qsranking, by = "institutions", method = "jw", max_dist = 0.2, distance_col='dist')
result <- result %>% group_by(id) %>% filter(countries == location | countries == "Unknown") %>% slice(which.min(dist))
rm(nodes_qsranking)

#cleaning qsranking results
result$rank_2025 <- gsub('[+]', '', result$rank_2025)
result$rank_2024 <- gsub('[+]', '', result$rank_2024)
result$rank_2025_num <- as.numeric(sapply(strsplit(result$rank_2025, c('-')), "[[", 1))
result$rank_2024_num <- as.numeric(sapply(result$rank_2024, function(x) {
  strsplit(x, '-')[[1]][1]
}))

#merging the cleaned results with nodes
result$rank <- (result$rank_2025_num + result$rank_2024_num)/2
result <- result[,c(1,12)]
nodes <- left_join(nodes, result, by = 'id')

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
  "<br>Continent: ", nodes$continent,
  "<br>Institution: ", nodes$institutions,
  "<br>Rank: ", nodes$rank
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
      uiOutput("edge_buttons"),
      uiOutput("dynamic_node_picker"),
      uiOutput("dynamic_net_switch"),
      uiOutput("inst_selector"),
      uiOutput("dynamic_slider"),
      uiOutput("rank_slider"),
      #sliderInput("rank", "Max Rank:",
      #            min = min(na.omit(nodes$rank)), max = max(na.omit(nodes$rank)),
      #            value = max(na.omit(nodes$rank))),
      uiOutput("cont_checkbox")
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
  
  # -- edge drawing logic selection (connected vs. further) --
  output$edge_buttons <- renderUI({
    req(input$view_mode=="Author view", input$selected_node)
    radioButtons("edge_mode","Edge-drawing logic:",
                 choices=c("Simple connections","Further connections"),
                 selected="Simple connections")
  })
  
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
  
  output$cont_checkbox <- renderUI({
    req(input$view_mode=="Author view", input$selected_node)
    checkboxGroupInput(
      "cont_filter", "Show continents:",
      choices  = sort(unique(nodes$continent)),
      selected = sort(unique(nodes$continent))
    )
  })
  

  
  # -- institution rank filter (author view only) --
  output$rank_slider <- renderUI({
    req(input$view_mode=="Author view", input$selected_node)
    sliderInput("rank_filter", "Max Rank:",
                min = min(na.omit(nodes$rank)), max = max(na.omit(nodes$rank)),
                value = max(na.omit(nodes$rank)))
  })
  
  # -- show‐institutes filter (author view only) --
  output$inst_selector <- renderUI({
    req(input$view_mode=="Author view", input$selected_node)
    ego_id <- nodes$author_id[nodes$label==input$selected_node]
    ce     <- subset(edges, from==ego_id|to==ego_id)
    ids    <- unique(c(ce$from,ce$to,ego_id))
    insts  <- nodes %>% filter(id%in%ids) %>% pull(institutions) %>% unique()
    checkboxGroupInput("inst_filter","Show institutions:",
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
      sliderInput("pubs_filter","Max Publications:",
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
    cn <- subset(nodes, id %in% ids)
    #fn  <- subset(nodes, id%in%ids)
    
    pn <- subset(cn, id == ego_id)
    fn <- subset(cn, id != ego_id)
    
    fur_e <- subset(edges,
                    (from %in% fn$id | to %in% fn$id) &
                    !(from == ego_id | to == ego_id)
    )
    
    if(input$edge_mode =="Simple connections") {
      ae <- ce
    } else if (input$edge_mode =="Further connections") {
      ae <- (rbind(ce, fur_e)) 
    }
    
    
    fn  <- fn %>% filter(institutions %in% input$inst_filter)
    fe  <- subset(ae, from%in%fn$id & to%in%fn$id)
    
    if (input$net_mode=="Institution network") {
      # keep one central node + one per partner institution
      main_node <- pn
      
      fn <- fn %>% filter(pubs <= input$pubs_filter)
      fn <- fn %>% filter(continent %in% input$cont_filter)
      fn <- fn %>% filter(rank <= input$rank_filter | is.na(rank))
      
      inst_nodes <- fn[fn$id!=ego_id,] %>%
        distinct(institutions,.keep_all=TRUE) %>%
        mutate(
          id    = paste0("inst__",institutions),
          label = institutions,
          title = paste0("Institution: ",institutions)
        )
      fn <- bind_rows(main_node,inst_nodes)
      fe <- inst_nodes %>% transmute(from=ego_id,to=id,freq=1)
    } else if (input$net_mode=="People network") {
      main_node <- pn
      
      fn <- fn %>% filter(pubs <= input$pubs_filter)
      fn <- fn %>% filter(continent %in% input$cont_filter)
      fn <- fn %>% filter(rank <= input$rank_filter | is.na(rank))
      
      fn <- bind_rows(main_node,fn)
      fe <- ae
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

shinyApp(ui = ui, server = server)