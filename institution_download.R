library(httr)
library(jsonlite)

papers <- readRDS("papers.rds")
author_name <- readRDS("author_name.rds")

author_name <- author_name[1:100]

#external_names <- nodes[grepl("^EXT_", nodes$id),]

#author_name <- external_names$label

#author_name <- author_name[author_name != "NA NA"]

#saveRDS(author_name, file = "author_name.rds")

search_name <- URLencode(author_name, reserved = TRUE)
search_url <- paste0("https://api.openalex.org/authors?search=", search_name)

countries <- numeric(length(author_name))
uni <- numeric(length(author_name))

for (i in 1:length(search_url)) {
  response <- GET(search_url[i])
  search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  res <- search_results$results
  
  #researchers[[length(researchers)+1]] <- res
  
  results_for_name <- list()
  
  if (is.data.frame(res) && nrow(res) != 0) {
    for (j in 1:nrow(res)) {
      results_for_name[[length(results_for_name)+1]] <- fromJSON(content(GET(res$works_api_url[j]), as = "text", encoding = "UTF-8"))$results
    }
  } else {
    results_for_name[[length(results_for_name)+1]] <- NA
  }
  
  for (h in 1:length(results_for_name)) {
    if(length(res) != 0) {
      if(length(which(results_for_name[[h]]$title %in% papers$title.value)) != 0) {
        if(length(res$last_known_institutions[[h]]$display_name) != 0) {
          uni[i] <- (res$last_known_institutions[[h]]$display_name[1])
          countries[i] <- (res$last_known_institutions[[h]]$country_code[1])
        }
        else{
          uni[i] <- 0
          countries[i] <- 0
        }
        break
      }
    }
  }
  
  rm(results_for_name)
  
}

downloaded_institutions <- data.frame(name = author_name, countries = countries, institutions = uni)

saveRDS(downloaded_institutions, file = "downloaded_institutions_1_100_test.rds")

#for (i in 1:length(search_url)) {
#  response <- GET(search_url[i])
#  search_results <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
#  res <- search_results$results
#  
#}