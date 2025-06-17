library(shiny)
library(visNetwork)
library(this.path)
library(dplyr)
library(scales)
setwd(this.dir())

`%nin%` <- Negate(`%in%`)   # convenience operator


# 1) Load data and merge institution info

nodes <- read.csv2("author_nodes_new.csv")
edges <- read.csv2("author_edges_new.csv")
institutions_raw <- readRDS("downloaded_institutions_combined.rds")

nodes$author_id <- as.character(nodes$author_id)
colnames(edges) <- c("from", "to", "freq")
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)

# match external IDs (EXT_)
ext_ids <- nodes[grepl("^EXT_", nodes$id) & nodes$label != "NA NA", ]$id
institutions_raw$id <- ext_ids
institutions_raw <- institutions_raw %>%
  rename(label = name) %>%                     # rename column to match nodes
  select(label, countries, institutions, id)   # keep same order

# internal (WU) institution rows
int_labels <- nodes[grepl("^A_", nodes$id), ]$label
int_inst <- data.frame(
  label        = int_labels,
  countries    = "AT",
  institutions = "Vienna University of Economics and Business",
  id           = nodes[grepl("^A_", nodes$id), ]$author_id,
  stringsAsFactors = FALSE
)

inst_all <- rbind(int_inst, institutions_raw)
inst_all$countries   [inst_all$countries    == 0] <- "Unknown"
inst_all$institutions[inst_all$institutions == 0] <- "Unknown"

# attach institution data to nodes
nodes <- nodes[nodes$label != "NA NA", ]
nodes <- merge(nodes, inst_all, by = c("id", "label"))
nodes <- nodes %>% distinct(author_id, .keep_all = TRUE)

# undirected edge aggregation
edges <- edges %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(from, to)), collapse = ":")) %>%
  ungroup() %>%
  group_by(pair) %>%
  summarise(
    from = strsplit(pair, ":")[[1]][1],
    to   = strsplit(pair, ":")[[1]][2],
    freq = sum(freq),
    .groups = "drop"
  )

# tool-tips
nodes$title <- paste0(
  "Name: ", nodes$label,
  "<br>Publications: ", nodes$pubs,
  "<br>Country: ", nodes$countries,
  "<br>Institution: ", nodes$institutions
)


# 2) User interface

ui <- fluidPage(
  titlePanel(div(img(src = "logo.png"), "Welcome to OPAL")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("view_mode", "View:",
                   choices = c("Autoren", "Institute"), inline = TRUE),
      selectInput("selected_node", "Select a node:", choices = nodes$label),
      sliderInput("pubs", "Max Publications:",
                  min = min(nodes$pubs), max = max(nodes$pubs),
                  value = max(nodes$pubs)),
      uiOutput("inst_checkbox")   # dynamic check-box list
    ),
    mainPanel(visNetworkOutput("filteredGraph", height = "550px"))
  )
)


# 3) Server

server <- function(input, output, session) {
  
  # return raw ego-network (before filters)
  raw_ego <- reactive({
    sel_id <- nodes[nodes$label == input$selected_node, ]$id
    e  <- subset(edges, from == sel_id | to == sel_id)
    ids <- unique(c(e$from, e$to, sel_id))
    list(nodes = subset(nodes, id %in% ids), edges = e)
  })
  
  # check-boxes – always reset to “all selected” on node change
  output$inst_checkbox <- renderUI(
    checkboxGroupInput(
      "inst_filter", "Show institutes:",
      choices  = sort(unique(nodes$institutions)),
      selected = sort(unique(nodes$institutions))
    )
  )
  observe({
    cur_choices <- sort(unique(raw_ego()$nodes$institutions))
    updateCheckboxGroupInput(
      session, "inst_filter",
      choices  = cur_choices,
      selected = cur_choices      # reset selection to ALL
    )
  })
  
  # draw network
  output$filteredGraph <- renderVisNetwork({
    eg   <- raw_ego()
    ego_id <- nodes[nodes$label == input$selected_node, ]$id
    
    # publication filter
    kn <- eg$nodes %>% filter(pubs <= input$pubs | id == ego_id)
    ka <- eg$edges %>% filter(from %in% kn$id & to %in% kn$id)
    
    # institute filter
    kn <- kn %>% filter(institutions %in% input$inst_filter)
    ka <- ka %>% filter(from %in% kn$id & to %in% kn$id)
    
    # switch labels for institute view
    if (input$view_mode == "Institute")
      kn$label <- kn$institutions
    
    # edge style
    ka$width <- rescale(ka$freq, to = c(1, 10))
    ka$title <- paste("Collaborations:", ka$freq)
    
    visNetwork(nodes = kn, edges = ka) %>%
      visLayout(randomSeed = 1) %>%
      visEdges(smooth = TRUE) %>%
      visPhysics(
        solver = "forceAtlas2Based",
        forceAtlas2Based = list(
          gravitationalConstant = -60,
          centralGravity        = 0.015,
          springLength          = 230,
          springConstant        = 0.08
        ),
        stabilization = FALSE
      ) %>%
      visEvents(
        stabilizationIterationsDone =
          "function () { this.fit({animation:false}); }"
      ) %>%
      addFontAwesome(name = "font-awesome",
                     version = c("4.7.0", "5.13.0")) %>%
      visGroups(groupname = "pro", shape = "icon",
                icon = list(code = "f007", size = 50, color = "steelblue")) %>%
      visGroups(groupname = "doc", shape = "icon",
                icon = list(code = "f007", size = 50, color = "gold"))
  })
}

shinyApp(ui = ui, server = server)
