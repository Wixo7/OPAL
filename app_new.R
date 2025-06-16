library(shiny)
library(visNetwork)
library(this.path)
library(dplyr)
library(scales)
#library(operators)
setwd(this.dir())

`%nin%` = Negate(`%in%`)

# loading the data
nodes <- read.csv2('author_nodes_new.csv')
edges <- read.csv2('author_edges_new.csv')
institutions <- readRDS("downloaded_institutions_combined.rds")
#nodes <- na.omit(nodes)
#edges <- na.omit(edges)
nodes <- nodes %>% distinct(author_id, .keep_all=TRUE)

colnames(edges) <- c('from', 'to', 'freq')

nodes$author_id <- as.character(nodes$author_id)
edges$from <- as.character(edges$from)
edges$to <- as.character(edges$to)

n <- nrow(institutions)
new_institutions <- institutions[!c(FALSE, rowMeans(institutions[-1, ] == institutions[-n, ]) == 1), ]
fixed_institutions <- new_institutions[which(new_institutions$name %in% nodes$label),]
fixed_institutions <- fixed_institutions[!duplicated(fixed_institutions[c('name')]), ]

external_names <- nodes[grepl("^EXT_", nodes$id),]$label
external_institutions <- data.frame(name = external_names)
external_institutions <- merge(external_institutions, fixed_institutions, by = "name")
external_names_rest <- nodes[grepl("^EXT_", nodes$id),]$label[external_names %nin% external_institutions$name]
external_rest_c <- numeric(length(external_names_rest))
external_rest_i <- numeric(length(external_names_rest))
external_institutions_rest <- data.frame(name = external_names_rest, countries = external_rest_c, institutions = external_rest_i)
external_institutions_final <- rbind(external_institutions, external_institutions_rest)

internal_names <- nodes[grepl("^A_", nodes$id),]$label
institutions_var <- numeric(length(internal_names))
countries_var <- numeric(length(internal_names))
internal_institutions <- data.frame(name = internal_names, countries = countries_var, institutions = institutions_var)
internal_institutions$countries <- "AT"
internal_institutions$institutions <- "Vienna University of Economics and Business"

final_institutions <- rbind(internal_institutions, external_institutions_final)
final_institutions$countries[which(final_institutions$countries == 0)] <- "Unknown"
final_institutions$institutions[which(final_institutions$institutions == 0)] <- "Unknown"

colnames(final_institutions) <- c('label', 'countries', 'institutions')

nodes <- merge(nodes, final_institutions, by = "label")
#nodes$institutions[which(nodes$university == "Vienna University of Economics and Business")] <- "Vienna University of Economics and Business"
#nodes$countries[which(nodes$university == "Vienna University of Economics and Business")] <- "AT"

#nodes <- nodes[!duplicated(nodes[c('id')]), ]

nodes <- nodes %>% distinct(author_id, .keep_all=TRUE)
nodes <- arrange(nodes, author_id)

#edges <- edges %>%
#  rowwise() %>%
#  mutate(pair = paste(sort(c(from, to)), collapse = "_")) %>%
#  ungroup() %>%
#  group_by(pair) %>%
#  summarise(
#    from = strsplit(pair, "_")[[1]][1],
#    to = strsplit(pair, "_")[[1]][2],
#    freq = sum(freq),
#    .groups = "drop"
# )

# creating a tooltip
newTitle = paste0("Name: ", nodes$label,
                  "<br>Publications: ", nodes$pubs,
                  "<br>Country: ", nodes$countries,
                  "<br>Institution: ", nodes$institutions)
nodes$title <- newTitle

# UI
ui <- fluidPage(
  # creating logo and title
  titlePanel(title=div(img(src="logo.png"), "Welcome to OPAL")),
  
  #layout elements - input and output
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_node", "Select a node:", choices = nodes$label),
      sliderInput("pubs", "Max Publications:",
                  min = min(nodes$pubs), max = max(nodes$pubs),
                  value = max(nodes$pubs)),
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
    filtered_nodes <- subset(filtered_nodes, pubs <= input$pubs)
  
    # post-filter network binding
    filtered_nodes <- rbind(filtered_nodes, primary_node)
    
    # labeling
    all_edges$width <- scales::rescale(all_edges$freq, to = c(1, 10))
    all_edges$title <- paste("Collaborations:", all_edges $freq)
    
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