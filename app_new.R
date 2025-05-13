library(shiny)
library(visNetwork)
library(this.path)
library(dplyr)
library(scales)
setwd(this.dir())

# loading the data
nodes <- read.csv2('author_nodes.csv')
edges <- read.csv2('author_edges.csv')
nodes <- na.omit(nodes)
edges <- na.omit(edges)
nodes <- nodes %>% distinct(author_id, .keep_all=TRUE)

colnames(edges) <- c('from', 'to', 'freq')

nodes$author_id <- as.character(nodes$author_id)
edges$from <- as.character(edges$from)
edges$to <- as.character(edges$to)

colnames(nodes)[colnames(nodes) == "author_id"] <- "id"

edges <- edges %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(from, to)), collapse = "_")) %>%
  ungroup() %>%
  group_by(pair) %>%
  summarise(
    from = strsplit(pair, "_")[[1]][1],
    to = strsplit(pair, "_")[[1]][2],
    freq = sum(freq),
    .groups = "drop"
  )

# creating a tooltip
newTitle = paste0("Name: ", nodes$label,
                  "<br>Publications: ", nodes$publications)
                  #"<br>Salary: €", nodes$salary)
nodes$title <- newTitle

# UI
ui <- fluidPage(
  # creating logo and title
  titlePanel(title=div(img(src="logo.png"), "Welcome to OPAL")),
  
  #layout elements - input and output
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_node", "Select a node:", choices = nodes$label),
      #radioButtons("aca_title", "Academic Title:",
      #             c("All" = "all",
      #               "Professor" = "pro",
      #               "PhD" = "doc")),
      sliderInput("publications", "Max Publications:",
                  min = min(nodes$publications), max = max(nodes$publications),
                  value = max(nodes$publications)),
    ),
    mainPanel(
      visNetworkOutput("filteredGraph", height = "500px")
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # creating the main logic of the graph display
  output$filteredGraph <- renderVisNetwork({
    req(input$selected_node)
    
    # get selected node ID
    selected <- (nodes[nodes$label == input$selected_node,]$id)
    
    # detect all nodes and edges connected to primary node
    connected_edges <- subset(edges, from == selected | to == selected)
    connected_node_ids <- unique(c(connected_edges$from, connected_edges$to, selected))
    connected_nodes <- subset(nodes, id %in% connected_node_ids)
    
    # splitting the network so only the connected nodes and not the primary one get filtered
    primary_node <- subset(connected_nodes, id == selected)
    filtered_nodes <- subset(connected_nodes, id != selected)
    
    further_edges <- subset(edges,
                            (from %in% filtered_nodes$id | to %in% filtered_nodes$id) &
                              !(from == selected | to == selected)
    )
    
    all_edges <- (rbind(connected_edges, further_edges))
    
    # FILTERS
    
    # academic titles filtering
    #if (input$aca_title != "all"){
    #filtered_nodes <- subset(filtered_nodes, type == input$aca_title)
    #}
    
    # salary filtering
    filtered_nodes <- subset(filtered_nodes, publications <= input$publications)
  
    # post-filter network binding
    filtered_nodes <- rbind(filtered_nodes, primary_node)
    
    # grouping
    #filtered_nodes$group <- filtered_nodes$type
    
    # labeling
    #filtered_nodes$label <- filtered_nodes$name
    all_edges$width <- scales::rescale(all_edges$freq, to = c(1, 10))
    all_edges$title <- paste("Collaborations:", all_edges$freq)
    
    # drawing the graph
    visNetwork(nodes = filtered_nodes, edges = all_edges) %>%
      addFontAwesome(name = "font-awesome", version = c("4.7.0", "5.13.0")) %>%
      visGroups(groupname = "pro", shape = "icon", 
                icon = list(code = "f007", size = 50, color = "steelblue")) %>%
      visGroups(groupname = "doc", shape = "icon", 
                icon = list(code = "f007", size = 50, color = "gold"))
  })
}


shinyApp(ui = ui, server = server)

#Andrea Potz
