library(httr)
library(jsonlite)

output <- fromJSON("pure_research_output.json", flatten=TRUE)
output_new <- output[["items"]]
papers <- output_new[c("title.value", "contributors")]

author_name <- nodes$label[1:100]
new_name <- "Andrea Wagner"

#search_name <- URLencode(author_name, reserved = TRUE)
search_name <- URLencode(new_name, reserved = TRUE)
search_url <- paste0("https://api.openalex.org/authors?search=", search_name)

rm(researchers)

results_for_all_names <- list()
researchers <- list()

countries <- c()

for (i in 1:length(search_url)) {
  response <- GET(search_url[i])
  search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  res <- search_results$results
  
  researchers[[length(researchers)+1]] <- res
  
}

for (i in 1:length(search_url)) {
  response <- GET(search_url[i])
  search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  res <- search_results$results
  
  results_for_name <- list()
  
  if (is.data.frame(res) && nrow(res) != 0) {
    for (j in 1:nrow(res)) {
      results_for_name[[length(results_for_name)+1]] <- fromJSON(content(GET(res$works_api_url[j]), as = "text", encoding = "UTF-8"))$results
    }
  } else {
    results_for_name[[length(results_for_name)+1]] <- NA
  }

  results_for_all_names[[length(results_for_all_names)+1]] <- results_for_name
  
  rm(results_for_name)
}

for (i in 1:length(results_for_all_names[[1]])) {
  if(length(which(results_for_all_names[[1]][[i]]$title %in% papers$title.value)) != 0) {
    print(researchers[[1]]$last_known_institutions[[i]]$display_name)
    print(researchers[[1]]$last_known_institutions[[i]]$country_code)
    break
  }
}

#search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

#res <- search_results$results

#results_for_name <- list()

#for (i in 1:nrow(res)) {
#  results_for_name[[length(results_for_name)+1]] <- fromJSON(content(GET(res$works_api_url[i]), as = "text", encoding = "UTF-8"))$results
#}

##----

search_term <- "Enabling Semantic Web Services: The Web Service Modeling Ontology"

correct <- 0

for (i in 1:length(results_for_name)) {
  if(search_term %in% results_for_name[[i]]$title) {
    correct <- i
  }
  if(correct != 0) {
    break
  }
}