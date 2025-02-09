library(shiny)
library(bslib)
library(tidyverse)
library(eiatools)
library(DT)
library(rlang)
library(rhandsontable)
library(htmltools)
library(rvest)
library(plotly)
library(shinyalert)


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
  
  # Petroleum
  
  "petroleum", "series", "series-description",
  "petroleum", "duoarea", "area-name",
  "petroleum", "process", "process-name",
  "petroleum", "product", "product-name",
  
  # Short Term Energy Outlook
  
  "steo", "seriesId", "seriesDescription",
  
  # Internation
  
  "international", "productId", "productName",
  "international", "activityId", "activityName",
  "international", "countryRegionId", "countryRegionName",
  "international", "countryRegionTypeId", "countryRegionTypeName",
  "international", "dataFlagId", "dataFlagDescription",
  "international", "unit", "unitName",
  
  # Coal
  
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
  
  # Crude Oil Imports
  
  "crude-oil-imports", "originId", "originName",
  "crude-oil-imports", "originType", "originTypeName",
  "crude-oil-imports", "destinationId", "destinationName",
  "crude-oil-imports", "destinationType", "destinationTypeName",
  "crude-oil-imports", "gradeId", "gradeName",
  
  # Electricity
  
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
  
  "electricity", "timezone", "timezone", 
  "electricity", "timezone-description", "timezone-description", 
  "electricity", "timePeriod", "timePeriod",
  
  "electricity", "producertypeid", "producerTypeDescription", 
  "electricity", "energysourceid", "energySourceDescription", 
  "electricity", "energy_source_code", "energy-source-desc",
  
  "electricity", "stateid", "state_general_description", 
  "electricity", "location", "state_general_description", 
  "electricity", "state", "state_general_description", 
  "electricity", "stateId", "state_general_description", 
  "electricity", "stateID", "state_general_description", 
  
  "electricity", "technology", "technology", 
  "electricity", "entityid", "entityName",
  "electricity", "plantid", "plantName",
  "electricity", "plantCode", "plantName",
  "electricity", "generatorid", "generatorid",
  "electricity", "prime_mover_code", "prime_mover_code",
  "electricity", "primeMover", "primeMover",
  "electricity", "balancing_authority_code", "balancing-authority-name",
  "electricity", "status", "statusDescription",
  "electricity", "unit", "unit",
  
  # Natural Gas
  
  "natural-gas", "duoarea", "area-name",
  "natural-gas", "product", "product-name",
  "natural-gas", "process", "process-name",
  "natural-gas", "series", "series-description",
  
  # Nuclear Outages
  
  "nuclear-outages", "facility", "facilityName",
  "nuclear-outages", "generator", "generator",
  
  # State Energy Data System
  
  "seds", "seriesId", "seriesDescription",
  "seds", "stateId", "stateDescription",
  
  # Biomass
  
  "densified-biomass", "region", "region",
  "densified-biomass", "fuelTypeId", "fuelTypeDescription",
  "densified-biomass", "respondent", "respondent",
  "densified-biomass", "stateId", "stateName",
  "densified-biomass", "status", "status",
  
  # Total Energy
  
  "total-energy", "msn", "seriesDescription",
  
  # Annual Energy Outlook
  
  "aeo", "history", "history",
  "aeo", "scenario", "scenarioDescription",
  "aeo", "tableId", "tableName",
  "aeo", "seriesId", "seriesName",
  "aeo", "regionId", "regionName",
  
  # International Energy Outlook
  
  "ieo", "history", "history",
  "ieo", "scenario", "scenarioDescription",
  "ieo", "tableId", "tableName",
  "ieo", "seriesId", "seriesName",
  "ieo", "regionId", "regionName",
  
  # CO2 Emissions
  
  "co2-emissions", "sectorId", "sector-name",
  "co2-emissions", "fuelId", "fuel-name",
  "co2-emissions", "stateId", "state-name",
  "co2-emissions", "seriesId", "series-name"
  
)

eia_docs <- function(){
  url <- "https://www.eia.gov/opendata/documentation.php"
  page <- rvest::read_html(url)
  content <- rvest::html_nodes(page, "body")
  html_content <- as.character(content)
  
  return(html_content)
}


