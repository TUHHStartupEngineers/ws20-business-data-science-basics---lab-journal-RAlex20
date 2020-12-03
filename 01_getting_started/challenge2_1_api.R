library(httr)
user_data <- GET("https://api.nytimes.com/svc/mostpopular/v2/viewed/7.json?api-key=YWHPIcHZhaVdsCWE1uAAcHBJPJAOGOta")
user_data

rawToChar(user_data$content)

library(jsonlite)
library(tidyverse)

mostPopularArticle_url <-user_data %>%
  .$content %>%
  rawToChar() %>%
  fromJSON %>%
  as_tibble() %>%
  head(n=10)

#mostPopularArticle_url[[1]]

view(mostPopularArticle_url)