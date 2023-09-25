# notes ----
# code for 2023 bsai octopus data queries and such 
# lee.cronin-fine@noaa.gov
# ben.williams@noaa.gov

# load ----
# devtools::unload("afscdata")
library(afscdata)
library(afscassess)

# globals ----
year = 2023
species = 870 # can't use species group code 'OTHR' as multiple species in there

# setup folder structure - only run this once
# setup_folders(year)

# query data ----
bsai_octopus(year)


old_catch <- data.frame(year = 1997:2002,
                       catch = c(160,168,310,359,211,334,
                                 86,13,14,57,9,21,
                                 3, 9, 2, 3, 7, 19),
                       target = c(rep("pcod", 6), rep("flatfish", 6), rep("other", 6))) %>% 
  tidytable::pivot_wider(names_from=target, values_from = catch) %>% 
  tidytable::mutate(total = sum(pcod, flatfish, other), .by=year)

vroom::vroom(here::here(year, "data", "raw", "fsh_catch_data.csv")) %>%  
  tidytable::filter(year >= 2003) %>%
  tidytable::mutate(target = case_when(trip_target_code=="C" ~ "pcod",
                            !(trip_target_code %in% c("A", "O", "C", "I", "B", "P", "K", "S")) ~ "flatfish",
                            TRUE ~ "other")) %>% 
  tidytable::summarise(catch = sum(weight_posted, na.rm = T),
                       .by = c(year, target, retained_or_discarded)) %>% 
  tidytable::mutate(total = sum(catch, na.rm=T), .by = year) %>% 
  tidytable::pivot_wider(names_from = retained_or_discarded, values_from = catch) %>% 
  tidytable::mutate(catch = sum(D, R, na.rm=T),  .by = c(year, target)) %>% 
  tidytable::mutate(retained = sum(R, na.rm=T) / total * 100, .by=year) %>% 
  tidytable::select(-c(D, R)) %>% 
  tidytable::pivot_wider(names_from = target, values_from = catch) %>% 
  tidytable::relocate(year, pcod, flatfish, other, total, retained) %>% 
  tidytable::bind_rows(old_catch,.) -> catch

vroom::vroom_write(catch, here::here(year, "data", "output", "catch.csv"))

  
db = connect()

table = dplyr::tbl(db, dplyr::sql("afsc.race_biomass_ebsshelf_plusnw"))
dplyr::collect(table)


afsc_species = c(78010, 78011, 78012, 78013, 78020, 78021, 78022, 78023, 
                 78030, 78040, 78210, 78300, 78301, 78352, 78353, 78403, 
                 78404, 78452, 78454, 78455)
afsc_species = c(78010, 78012, 78020, 78403, 78454, 78455)

db2 = connect("afsc")
dplyr::tbl(db2, dplyr::sql("gap_products.biomass")) %>% 
  dplyr::rename_with(tolower) %>% 
  dplyr::filter(area_id==99900, species_code %in% afsc_species) %>% 
  dplyr::group_by(year, species_code) %>% 
  dplyr::summarise(biomass = sum(biomass_mt)) %>% 
  dplyr::collect() %>% 
  dplyr::arrange(species_code, year) %>% 
  View()


table %>% 
  dplyr::rename_with(tolower) %>% 
  dplyr::filter(species_code %in% afsc_species, species_name=="Enteroctopus dofleini") %>% 
  dplyr::group_by(year, species_name) %>% 
  dplyr::summarise(biomass = sum(biomass)) %>% dplyr::arrange(year) %>% View()



