library(httr)
library(jsonlite)

author_name <- 'Axel Polleres'
search_term <- "Enabling Semantic Web Services: The Web Service Modeling Ontology"

search_name <- gsub(" ", "+", author_name)
search_url <- paste0("https://api.openalex.org/authors?search=", search_name)
response <- GET(search_url)

search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

res <- search_results$results

results_for_name <- list()

for (i in 1:nrow(res)) {
  results_for_name[[length(results_for_name)+1]] <- fromJSON(content(GET(res$works_api_url[i]), as = "text", encoding = "UTF-8"))$results
}

correct <- 0

for (i in 1:length(results_for_name)) {
  if(search_term %in% results_for_name[[i]]$title) {
    correct <- i
  }
  if(correct != 0) {
    break
  }
}