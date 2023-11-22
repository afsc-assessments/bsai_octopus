##=================================================##
#     Obtaining the Octopus Catch and Survey Data   #
#      First Created: 09/26/23                      #
#      Last Updated: 09/26/23                       #
##=================================================##

library(here)
library(keyring)  # accessing my username and passwords for database
library(RODBC)    # used to access database in R
# library(readr)
library(tidyverse)
library(dplyr)


##-----------------------##
#   Accessing Databases   #
##-----------------------##
# AKFIN database #
database_akfin <- 'akfin'
channel_akfin <- RODBC::odbcConnect(database_akfin, uid = key_list(database_akfin)$username,
                                    pwd = key_get(database_akfin, keyring::key_list(database_akfin)$username),
                                    believeNRows=FALSE)  # gains access to database

# AFSC database #
database_afsc <- 'afsc'
channel_afsc <- RODBC::odbcConnect(database_afsc, uid = key_list(database_afsc)$username,
                                   pwd = key_get(database_afsc, keyring::key_list(database_afsc)$username),
                                   believeNRows=FALSE)  # gains access to database


##----------------------##
#   Collect Catch Data   #
##----------------------##

## Collect Catch Data from AKFIN ##
query_cat <- "select   year, week_end_date, ves_akr_name, FMP_Area, fmp_subarea, reporting_area_code, trip_target_group, agency_species_code, species_group_code, species_group_name, species_name, weight_posted 
          from     council.comprehensive_blend_ca
          where    agency_species_code = 870 and
                   fmp_area = 'BSAI' and
                   year <= 2023"


catch <- RODBC::sqlQuery(channel_akfin, query_cat)

catch <- catch %>%
  rename_all(tolower) %>%
  mutate(target_species = case_when(trip_target_group == "Pacific Cod" ~ "P.cod",
                                    trip_target_group == "Flatfish" ~ "Flatfish",
                                    .default = "Other"))

catch_total <- catch %>%
  group_by(year) %>%
  summarise(catch_wght = sum(weight_posted)) %>%
  ungroup()


catch_tg <- catch %>%
  group_by(year,target_species) %>%
  summarise(catch_wght = sum(weight_posted)) %>%
  ungroup()

AAA
##-----------------------##
#   Collect Survey Data   #
##-----------------------##
## Setting up query for AKFIN ##  (You can use either AKFIN or AFSC, they should give the same data)
# query_sur <- "select  survey, year, regulatory_area_name, species_code, area_biomass, biomass_var
#              from     afsc.race_biomassareaaigoa
#              where    species_code in (400,420,435,440,455,460,471,472,480,485) and
#                       survey = 'GOA' and
#                       year <= 2023"
# temp_channel <- channel_akfin

## Setting up query for AFSC ##
query_sur <- "select  survey_definition_id, year, species_code, biomass_mt, biomass_var
             from     gap_products.biomass
             where    species_code in (78010, 78011, 78012, 78013, 78020, 78021, 78022, 78023,78030, 78040, 78210, 78300, 78301, 78352, 78353, 78403, 78404, 78452, 78454, 78455) and
                      year <= 2023"
temp_channel <- channel_afsc


## Get Survey Data ## 
tot_sur_biomass <- RODBC::sqlQuery(temp_channel, query = query_sur)

test <- tot_sur_biomass %>%
  rename_all(tolower) %>%
  add_column(species = "Octopus") %>%
  group_by(year, survey_definition_id, species) %>%
  summarise(biomass = sum(biomass_mt)) %>%
  ungroup()
  


