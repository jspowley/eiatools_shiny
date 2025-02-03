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
  
  "steo", "seriesId", "seriesDescription",
  
  "international", "productId", "productName",
  "international", "activityId", "activityName",
  "international", "countryRegionId", "countryRegionName",
  "international", "countryRegionTypeId", "countryRegionTypeName",
  "international", "dataFlagId", "dataFlagDescription",
  "international", "unit", "unitName",
  
  "coal", "mineStateId", "mineStateDescription",
  "coal", "coalRankId", "coalRankDescription",
  "coal", "plantStateId", "plantStateDescription",
  "coal", "mineTypeId", "mineTypeDescription",
  "coal", "mineMSHAId", "mineName",
  "coal", "mineBasinId", "mineBasinDescription",
  "coal", "mineCountyId", "mineCountyName",
  "coal", "contractType", "contractType",
  "coal", "transportationMode", "transportationMode",
  "coal", "coalSupplier", "coalSupplier",
  "coal", "plantId", "plantName",
  "coal", "location", "location",
  "coal", "rank", "rank",
  "coal", "coalType", "coalType",
  "coal", "sector", "sectorDescription",
  "coal", "stateRegionId", "stateRegionDescription",
  "coal", "exportImportType", "exportImportType",
  "coal", "countryId", "countryDescription",
  "coal", "customsDistrictId", "customsDistrictDescription",
  "coal", "marketTypeId", "marketTypeDescription",
  "coal", "stateId", "stateDescription",
  "coal", "regionId", "regionDescription",
  "coal", "supplyRegionId", "supplyRegionDescription",
  "coal", "censusRegionId", "censusRegionDescription",
  "coal", "mississippiRegionId", "mississippiRegionDescription",
  "coal", "mineStatusId", "mineStatusDescription"
)
