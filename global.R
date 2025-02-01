library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)
library(rlang)

unique_facets <- function(data_index_table){
  data_index_table %>% pull(facets) %>% unlist() %>% unique()
}

facet_desc_map <- tibble::tribble(
  ~table, ~facet, ~desc,
  "petroleum", "series", "series-description",
  "petroleum", "duoarea", "area-name",
  "petroleum", "process", "process-name",
  "petroleum", "product", "product-name",
)
