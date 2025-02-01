library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)

unique_facets <- function(data_index_table){
  data_index_table %>% pull(facets) %>% unlist() %>% unique()
}

facet_desc_map <- tibble::tribble(
  ~table, ~facet, ~desc_name,
  "petroleum", "series", "series-description"
)
