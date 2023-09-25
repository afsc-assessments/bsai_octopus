# notes ----
# code for 2023 bsai octopus data queries and such 
# lee.cronin-fine@noaa.gov
# ben.williams@noaa.gov

# load ----
# remotes::install_github("BenWilliams-NOAA/afscassess")
# remotes::install_github("afsc-assessments/afscdata")
library(afscdata)
library(afscassess)
library(ggplot2)
theme_set(theme_report())

# globals ----
year = 2023

# setup folder structure - only run this once
# setup_folders(year)

# query data ----
bsai_octopus(year)

# cleanup catch data 
# afscdata is not currently setup for bsai octopus catch data so add in archived data here (it came from SAFE doc, no idea on origin...)

old_catch <- data.frame(year = 1997:2002,
                       catch = c(160,168,310,359,211,334,
                                 86,13,14,57,9,21,
                                 3, 9, 2, 3, 7, 19),
                       target = c(rep("pcod", 6), rep("flatfish", 6), rep("other", 6))) %>% 
  tidytable::pivot_wider(names_from=target, values_from = catch) %>% 
  tidytable::mutate(total = sum(pcod, flatfish, other), .by=year)

vroom::vroom(here::here(year, "data", "raw", "fsh_catch_data.csv")) %>%  
  tidytable::filter(year >= 2003) %>%
  tidytable::mutate(target = tidytable::case_when(trip_target_code=="C" ~ "pcod",
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

# cleanup survey data   
vroom::vroom(here::here(year, "data", "raw", "survey_biomass.csv")) %>% 
  tidytable::summarise(biomass = sum(biomass_mt),
                       var = sum(biomass_var, na.rm=T),
                       .by = c(year, area_id )) -> srv
# survey table
srv %>% 
  tidytable::mutate(cv = sqrt(var) / biomass * 100) %>% 
  tidytable::mutate(area = tidytable::case_when(area_id==99900 ~ "EBS_shelf",
                                                area_id==99905 ~ "EBS_slope",
                                                area_id==99904 ~ "AI")) %>% 
  tidytable::pivot_wider(names_from = area, values_from = c(biomass, cv)) %>% 
  tidytable::select(-area_id, -var) -> survey

vroom::vroom_write(survey, here::here(year, "data", "output", "bts_biomass.csv"))

# plots 
dir.create(here::here(year, "figs"))


png(filename=here::here(year, "figs", "catch.png"), width = 6.5, height = 6.5,
    units = "in", type ="cairo", res = 200)

catch %>% 
  ggplot(aes(year, total)) + 
  geom_point() +
  geom_line() +
  expand_limits(y=0) +
  xlab("Year") +
  ylab("Catch (t)") +
  afscassess::scale_x_tickr(data = catch, var=year)

dev.off()



png(filename=here::here(year, "figs", "survey.png"), width = 6.5, height = 6.5,
    units = "in", type ="cairo", res = 200)

srv %>% 
  tidytable::filter(year>1986) %>% 
  tidytable::mutate(area = tidytable::case_when(area_id==99900 ~ "EBS_shelf",
                                                area_id==99905 ~ "EBS_slope",
                                                area_id==99904 ~ "AI"),
                    lci = biomass - sqrt(var) * 1.96,
                    uci = biomass + sqrt(var) * 1.96,
                    lci = ifelse(lci<0, 0, lci)) %>% 
  ggplot(aes(year, biomass, color=area)) +
  geom_point(position=position_dodge(width=0.6)) + 
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2, position=position_dodge(width=0.6)) +
  scico::scale_color_scico_d("Area", palette = 'roma', begin = 0.2) +
  afscassess::scale_x_tickr(data = srv, var=year) +
  xlab("Year") +
  ylab("Biomass (t)") +
  theme(legend.position = c(.9, .8))

dev.off()



