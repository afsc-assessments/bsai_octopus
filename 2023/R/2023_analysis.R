# notes ----
# code for 2023 bsai octopus data queries and such 
# lee.cronin-fine@noaa.gov
# ben.williams@noaa.gov

# load ----

library(afscdata)
library(afscassess)

# globals ----
year = 2023
species = 870 # can't use species group code 'OTHR' as multiple species in there

# setup folder structure - only run this once
# setup_folders(year)

# query data ----
bsai_octopus(year)
