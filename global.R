library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)
library(rlang)
library(rhandsontable)

unique_facets <- function(data_index_table){
  data_index_table %>% pull(facets) %>% unlist() %>% unique()
}

# Why? Since EIA doesn't actually tell us the mapping of ID to the more descriptive term in the metadata request, 
# manually mapping is required. Fortunately we only need to maintain about 1-30 columns per table, which isn't ideal, 
# but certainly isn't into the range of "tasks which are too big for humans"

# For a good example of proof manual mapping is 100% required, see eiatools::data_index$coal...
# We have id's mapped to descriptions, id's without descriptions which require 
# they are mapped to themselves, and descriptions and id's which aren't adjacent 
# within the table.

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
  "coal", "mineStatusId", "mineStatusDescription",
  
  "crude-oil-imports", "originId", "originName",
  "crude-oil-imports", "originType", "originTypeName",
  "crude-oil-imports", "destinationId", "destinationName",
  "crude-oil-imports", "destinationType", "destinationTypeName",
  "crude-oil-imports", "gradeId", "gradeName",
  
  "electricity", "stateid", "stateDescription",
  "electricity", "sectorid", "sector_general_description",
  "electricity", "sector", "sector_general_description",
  "electricity", "fueltypeid", "fuelTypeDescription", 
  "electricity", "respondent", "respondent-name", 
  "electricity", "type", "type-name", 
  "electricity", "fueltype", "type-name", 
  "electricity", "fuel2002", "fuelTypeDescription", 
  "electricity", "fuelid", "fuelDescription", 
  "electricity", "subba", "subba-name", 
  "electricity", "parent", "parent-name", 
  "electricity", "fromba", "fromba-name", 
  "electricity", "toba", "toba-name", 
  "electricity", "timezone", "timezone-description", 
  "electricity", "state", "stateId", 
  "electricity", "producertypeid", "producerTypeDescription", 
  "electricity", "energysourceid", "energySourceDescription", 
  "electricity", "", "", 
  "electricity", "", "", 
  "electricity", "", "", 
  
)
