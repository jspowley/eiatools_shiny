library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)

unique_facets <- function(data_index_table){
  data_index_table %>% pull(facets) %>% unlist() %>% unique()
}
