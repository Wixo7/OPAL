library(shiny)
library(visNetwork)
library(this.path)
library(dplyr)
library(scales)
library(fuzzyjoin)
library(countrycode)
setwd(this.dir())

`%nin%` = Negate(`%in%`)

# loading the data
nodes <- read.csv2('author_nodes_new.csv')
edges <- read.csv2('author_edges_new.csv')
institutions <- readRDS("downloaded_institutions_combined.rds")

nodes$author_id <- as.character(nodes$author_id)
colnames(edges) <- c('from', 'to', 'freq')
edges$from <- as.character(edges$from)
edges$to <- as.character(edges$to)

external_ids <- nodes[grepl("^EXT_", nodes$id),]$id[which(nodes[grepl("^EXT_", nodes$id),]$label != "NA NA")]
institutions$id <- external_ids

internal_names <- nodes[grepl("^A_", nodes$id),]$label
institutions_var <- numeric(length(internal_names))
countries_var <- numeric(length(internal_names))
internal_ids <- nodes[grepl("^A_", nodes$id),]$author_id
internal_institutions <- data.frame(name = internal_names, countries = countries_var, institutions = institutions_var, id = internal_ids)
internal_institutions$countries <- "AT"
internal_institutions$institutions <- "Vienna University of Economics and Business"

final_institutions <- rbind(internal_institutions, institutions)
final_institutions$countries[which(final_institutions$countries == 0)] <- "Unknown"
final_institutions$institutions[which(final_institutions$institutions == 0)] <- "Unknown"

colnames(final_institutions) <- c('label', 'countries', 'institutions', 'id')

nodes <- nodes[nodes$label != "NA NA",]

nodes <- merge(nodes, final_institutions, by = c("id", "label"))

nodes <- nodes %>% distinct(author_id, .keep_all=TRUE)

edges <- edges %>%
  rowwise() %>%
  mutate(pair = paste(sort(c(from, to)), collapse = ":")) %>%
  ungroup() %>%
  group_by(pair) %>%
  summarise(
    from = strsplit(pair, ":")[[1]][1],
    to = strsplit(pair, ":")[[1]][2],
    freq = sum(freq),   
    .groups = "drop"
  )

qsranking <- read.csv("qsranking.csv")
qsranking <- subset(qsranking, select = c(index, rank_2025, rank_2024, institution_name, location))
qsranking$location <- countrycode(qsranking$location, origin = "country.name.en", destination = "iso2c")

colnames(qsranking) <- c('index', 'rank_2025', 'rank_2024', 'institutions', 'location')

nodes <- nodes[6000:7000,]

nodes <- subset(nodes, select = c(id, institutions, countries))

result <- stringdist_left_join(nodes, qsranking, by = "institutions", method = "jw", max_dist = 0.12, distance_col='dist')

result_new <- result %>% group_by(id) %>% filter(countries == location | countries == "Unknown") %>% slice(which.min(dist))
