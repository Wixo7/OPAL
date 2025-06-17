library(shiny)
library(visNetwork)
library(this.path)
library(dplyr)
library(scales)
setwd(this.dir())

`%nin%` <- Negate(`%in%`)   # Convenience operator


### 1) Load data and harmonise institution information                


nodes        <- read.csv2("author_nodes_new.csv")
edges        <- read.csv2("author_edges_new.csv")
institutions <- readRDS("downloaded_institutions_combined.rds")

nodes$author_id <- as.character(nodes$author_id)
colnames(edges) <- c("from", "to", "freq")
edges$from <- as.character(edges$from)
edges$to   <- as.character(edges$to)

external_ids  <- nodes[grepl("^EXT_", nodes$id) & nodes$label != "NA NA", ]$id
institutions$id <- external_ids                      # add missing id column
institutions <- institutions |>
  rename(label = name) |>
  select(label, countries, institutions, id)         # match column order

internal_names <- nodes[grepl("^A_", nodes$id), ]$label
internal_institutions <- data.frame(
  label        = internal_names,
  countries    = "AT",
  institutions = "Vienna University of Economics and Business",
  id           = nodes[grepl("^A_", nodes$id), ]$author_id,
  stringsAsFactors = FALSE
)

final_institutions <- rbind(internal_institutions, institutions)
final_institutions$countries    [final_institutions$countries     == 0] <- "Unknown"
final_institutions$institutions [final_institutions$institutions  == 0] <- "Unknown"

nodes <- nodes[nodes$label != "NA NA", ]
nodes <- merge(nodes, final_institutions, by = c("id", "label"))
nodes <- nodes %>% distinct(author_id, .keep_all = TRUE)

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

nodes$title <- paste0(
  "Name: ", nodes$label,
  "<br>Publications: ", nodes$pubs,
  "<br>Country: ", nodes$countries,
  "<br>Institution: ", nodes$institutions
)


### 2) User interface                                                  


ui <- fluidPage(
  titlePanel(div(img(src = "logo.png"), "Welcome to OPAL")),
  sidebarLayout(
    sidebarPanel(
      radioButtons("view_mode", "Ansicht:",
                   choices = c("Autoren", "Institute"), inline = TRUE),
      selectInput("selected_node", "Select a node:", choices = nodes$label),
      sliderInput("pubs", "Max Publications:",
                  min = min(nodes$pubs), max = max(nodes$pubs),
                  value = max(nodes$pubs)),
      uiOutput("inst_selector")        # populated dynamically
    ),
    mainPanel(visNetworkOutput("filteredGraph", height = "500px"))
  )
)


### 3) Server logic                                                    


server <- function(input, output, session) {
  
  # Helper: returns raw ego-network (without any filters)
  get_raw_ego <- reactive({
    sel_id <- nodes[nodes$label == input$selected_node, ]$id
    ego_edges <- subset(edges, from == sel_id | to == sel_id)
    ego_ids   <- unique(c(ego_edges$from, ego_edges$to, sel_id))
    list(
      nodes = subset(nodes, id %in% ego_ids),
      edges = ego_edges
    )
  })
  
  # Dynamic institute checkbox list – always pre-selected
  output$inst_selector <- renderUI({
    checkboxGroupInput("inst_filter", "Institute anzeigen:", choices = NULL)
  })
  
  observe({
    inst_choices <- sort(unique(get_raw_ego()$nodes$institutions))
    updateCheckboxGroupInput(
      session, "inst_filter",
      choices  = inst_choices,
      selected = inst_choices        # pre-select everything
    )
  })
  
  # Draw the (filtered) network
  output$filteredGraph <- renderVisNetwork({
    
    # Apply publication filter
    raw <- get_raw_ego()
    sel_id <- nodes[nodes$label == input$selected_node, ]$id
    ndf <- raw$nodes %>% filter(pubs <= input$pubs | id == sel_id)
    edf <- raw$edges %>% filter(from %in% ndf$id & to %in% ndf$id)
    
    # Apply institute checkbox filter
    ndf <- ndf %>% filter(institutions %in% input$inst_filter)
    edf <- edf %>% filter(from %in% ndf$id & to %in% ndf$id)
    
    # Switch to institute labels if requested
    if (input$view_mode == "Institute") {
      ndf$label <- ndf$institutions
    }
    
    # Edge styling
    edf$width <- rescale(edf$freq, to = c(1, 10))
    edf$title <- paste("Collaborations:", edf$freq)
    
    visNetwork(ndf, edf) %>%
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
      # Auto-fit once the network is stabilised
      visEvents(
        stabilizationIterationsDone =
          "function () { this.fit({animation:false}); }"
      ) %>%
      addFontAwesome(name = "font-awesome", version = c("4.7.0", "5.13.0")) %>%
      visGroups(groupname = "pro", shape = "icon",
                icon = list(code = "f007", size = 50, color = "steelblue")) %>%
      visGroups(groupname = "doc", shape = "icon",
                icon = list(code = "f007", size = 50, color = "gold"))
  })
}

shinyApp(ui = ui, server = server)
