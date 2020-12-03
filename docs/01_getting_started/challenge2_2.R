# WEBSCRAPING ----

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing

# 1.1 Collect Models and its Prices under Reise ----

url_reise <- "https://www.rosebikes.de/fahrräder/reise"

xopen(url_reise)

html_reise <- read_html(url_reise)

# Web scrape the nodes for models

bike_reise_models <- html_reise %>%
  
  html_nodes(css = ".catalog-category-bikes__title .catalog-category-bikes__title-text") %>%
  html_text() %>%
  str_remove_all(pattern = "\n")
  #enframe(name = "position", value = "model_names")

bike_reise_models

bike_reise_price <- html_reise %>%
  html_nodes(css = ".catalog-category-bikes__price-title") %>%
  html_text() %>%
  stringr::str_extract("(?<=\\nab ).*(?=€\\n)") %>%
  readr::parse_number() %>%
  str_remove_all(pattern = "\\.") %>%
  as.numeric()
  #enframe(name = "posiion", value = "model_price")
  
bike_reise_price

tibble(bike_reise_models, bike_reise_price)