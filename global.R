library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)
library(rlang)

unique_facets <- function(data_index_table){
  data_index_table %>% pull(facets) %>% unlist() %>% unique()
}

# Why? Since EIA doesn't actually tell us the mapping of ID to the more descriptive term in the metadata request, 
# manually mapping is required. Fortunately we only need to maintain about 1-30 columns per table, which isn't ideal, 
# but certainly isn't into the range of "tasks which are too big for humans"

facet_desc_map <- tibble::tribble(
  ~table, ~facet, ~desc,
  
  "petroleum", "series", "series-description",
  "petroleum", "duoarea", "area-name",
  "petroleum", "process", "process-name",
  "petroleum", "product", "product-name",
  
  "steo", "seriesId", "seriesDescription"
)
