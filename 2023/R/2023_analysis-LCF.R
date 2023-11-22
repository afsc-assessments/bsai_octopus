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
library(tidyverse)
library(dplyr)
library(here)
library(scico)
theme_set(theme_report())

##------------------##
#    Manual Inputs   #
##------------------##
query_data <- F
export_dat <- F
pres_pl   <- T         # T = Create plots for presentation, F = don't create plots for presentations 
year = 2023

# setup folder structure - only run this once
# setup_folders(year)

# query data ----
if(query_data == T){
  bsai_octopus(year) # pulls the data
  temp <- as.numeric(unlist(strsplit(as.character(Sys.Date()), split = "-")))
  cur_dat_word <- paste(month.name[temp[2]], " ", temp[3],", ",temp[1],sep = "" )
  cat("Data was last collected on:", cur_dat_word, file = here(year, "data","raw","data_last_called.out"))
} 

# cleanup catch data 
# afscdata is not currently setup for bsai octopus catch data so add in archived data here (it came from SAFE doc, no idea on origin...)

old_catch <- data.frame(year = 1997:2002,
                       catch = c(160,168,310,359,211,334,
                                 86,13,14,57,9,21,
                                 3, 9, 2, 3, 7, 19),
                       target = c(rep("pcod", 6), rep("flatfish", 6), rep("other", 6))) %>% 
  pivot_wider(names_from=target, values_from = catch) %>% 
  mutate(total = sum(pcod, flatfish, other), .by=year)

# the raw data
raw_catch <- read.csv(here(year, "data","raw","fsh_catch_data.csv"))

catch_tot <- raw_catch %>%  
  filter(year >= 2003) %>%
  mutate(target = case_when(trip_target_code=="C" ~ "pcod",
                            !(trip_target_code %in% c("A", "O", "C", "I", "B", "P", "K", "S")) ~ "flatfish",
                            TRUE ~ "other")) %>% 
  summarise(catch = sum(weight_posted, na.rm = T),
                       .by = c(year, target, retained_or_discarded)) %>% 
  mutate(total = sum(catch, na.rm=T), .by = year) %>% 
  pivot_wider(names_from = retained_or_discarded, values_from = catch) %>% 
  mutate(catch = sum(D, R, na.rm=T),  .by = c(year, target)) %>% 
  mutate(retained = sum(R, na.rm=T) / total * 100, .by=year) %>% 
  select(-c(D, R)) %>% 
  pivot_wider(names_from = target, values_from = catch) %>% 
  relocate(year, pcod, flatfish, other, total, retained) %>% 
  bind_rows(old_catch,.) %>%
  mutate(across(c('pcod','flatfish','other','total'), round, 0)) %>%
  mutate(across(c('retained'), round, 0)) %>%
  rename(Year = year,
         P.cod = pcod,
         Flatfish = flatfish,
         Other = other,
         Total = total) 

if(export_dat == T) {
  write.csv(catch_tot, here(year, "data", "output", paste(year,"octopus_total_catch_by_target.csv",sep = "_")),row.names = F)
  temp <- unlist(strsplit(as.character(max(raw_catch$week_end_date)), split = "T"))
  temp <- as.numeric(unlist(strsplit(as.character(temp[1]), split = "-")))
  last_cat_dat_word <- paste(month.name[temp[2]], " ", temp[3],", ",temp[1],sep = "" )
  cat("Data when catch was last recorded:", last_cat_dat_word, file = here(year, "data","raw","data_last_catch_date.out"))
}
  

# Plot Catch with ABC and TAC

# Read in the file with ABC and TAC. This an manually manipulated excel file. To get the 
specs <- read.csv(here::here(year, "data", "user_inputs", 'specs.csv'))  

temp <- catch_tot %>%
  select(Year,Total)

specs2 <- specs %>%
  select(Year,TAC,ABC)  %>%
  left_join(temp) %>%
  pivot_longer(cols = c('TAC', 'ABC'))

# ggplot2::theme_set(cowplot::theme_cowplot(font_size = 13) +
#                      cowplot::background_grid() +
#                      cowplot::panel_border())
# ggplot2::theme_set(afscassess::theme_report())


tac_abc_pl <- ggplot(specs2 %>%
                     filter(Year <= year),
                      aes(x = Year)) +
  geom_bar(aes(y = value, fill = name),
           stat = 'identity', position = 'identity',
           col = 'black', alpha = .25) +
  geom_point(aes(y = Total, shape = 'Catch'))  +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  geom_line(aes(y = Total, lty = 'Catch')) +
  scale_y_continuous(labels = scales::comma)  +
  scale_fill_manual(values = c('white', 'black'))  +
  theme(legend.position = c(.1, .85)) +
  labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
print(tac_abc_pl)

ggsave(here::here(year, "figs", paste(year,"Octopus_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
      height = 4, width = 7,bg = 'white')

if(pres_pl == T){
  tac_abc_pl2 <- ggplot(specs2 %>%
                         filter(Year <= year),
                       aes(x = Year)) +
    geom_bar(aes(y = value, fill = name),
             stat = 'identity', position = 'identity',
             col = 'black', alpha = .25) +
    geom_point(aes(y = Total, shape = 'Catch'))  +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    geom_line(aes(y = Total, lty = 'Catch')) +
    scale_y_continuous(labels = scales::comma)  +
    scale_fill_manual(values = c('white', 'black'))  +
    theme(legend.position = c(.1, .85)) +
    labs(x = NULL, y = 'Biomass (t)', fill = NULL, shape = NULL, lty = NULL)
  print(tac_abc_pl2)
  
  ggsave(here::here(year, "figs","Presentations", paste(year,"Octopus_catch_specs.png",sep = "_")), dpi = 300, units = 'in',
         height = 4, width = 7,bg = 'white')
  
}

## Survey Data

raw_srv <- read.csv(here(year, "data", "raw", "survey_biomass.csv"))

temp_srv <- raw_srv %>%
  summarise(biomass = sum(biomass_mt),
                       var = sum(biomass_var, na.rm=T),
                       .by = c(year, area_id )) %>% 
  mutate(cv = sqrt(var) / biomass * 100) %>% 
  mutate(area = case_when(area_id==99900 ~ "EBS_shelf",
                                                area_id==99905 ~ "EBS_slope",
                                                area_id==99904 ~ "AI")) %>% 
  pivot_wider(names_from = area, values_from = c(biomass, cv)) %>% 
  select(-area_id, -var) %>%
  mutate(across(everything(), round, 0))

if(export_dat == T) write.csv(temp_srv, here(year, "data", "output", paste(year,"octopus_bts_biomass.csv",sep = "_")),row.names = F)

# Plot survey
pl_suryey <- raw_srv %>%
  summarise(biomass = sum(biomass_mt),
            var = sum(biomass_var, na.rm=T),
            .by = c(year, area_id )) %>% 
  filter(year>1986) %>% 
  mutate(area = case_when(area_id==99900 ~ "EBS shelf",
                          area_id==99905 ~ "EBS slope",
                          area_id==99904 ~ "AI"),
                    lci = biomass - sqrt(var) * 1.96,
                    uci = biomass + sqrt(var) * 1.96,
                    lci = ifelse(lci<0, 0, lci))


srv_nam <- unique(pl_suryey$area)
mean_vec <- c()
for(i in 1:length(srv_nam))
{
  temp <- pl_suryey %>%
    filter(area == srv_nam[i])
  mean_vec[i] <- mean(temp$biomass)
}

p_survey <- ggplot(pl_suryey, aes(x = year, y = biomass, color = area)) + 
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2, position=position_dodge(width=0.6)) +
  # scico::scale_color_scico_d(palette = "roma") +
  scale_color_manual(values = c("#7E1700", "#C7B354","#023198")) +
  geom_hline(yintercept = mean_vec[1], lty = 2,  col = "#7E1700") +
  geom_hline(yintercept = mean_vec[2], lty = 2, col = '#C7B354') +
  geom_hline(yintercept = mean_vec[3], lty = 2, col = '#023198') +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  theme(legend.position = c(.68, .85)) +
  guides(color = guide_legend(title = "Survey")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = 'Biomass (t)', title = '') +
  xlim(1987,2024)
print(p_survey)

ggsave(here(year, "figs", paste(year,"Octopus_surveys.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 7, bg = 'white')

if(pres_pl == T){
  p_survey2 <- ggplot(pl_suryey, aes(x = year, y = biomass, color = area)) + 
    geom_point(position=position_dodge(width=0.6)) +
    geom_errorbar(aes(ymin=lci, ymax=uci), width=0.2, position=position_dodge(width=0.6)) +
    # scico::scale_color_scico_d(palette = "roma") +
    scale_color_manual(values = c("#7E1700", "#C7B354","#023198")) +
    geom_hline(yintercept = mean_vec[1], lty = 2,  col = "#7E1700") +
    geom_hline(yintercept = mean_vec[2], lty = 2, col = '#C7B354') +
    geom_hline(yintercept = mean_vec[3], lty = 2, col = '#023198') +
    theme(panel.grid.major = element_line(linewidth = 0.5)) +
    theme(axis.line = element_line()) +
    theme(axis.ticks = element_line(colour = "black")) +
    theme(legend.position = c(.68, .85)) +
    theme(axis.text = element_text(size =12),
          axis.title = element_text(size =12),
          legend.text = element_text(size = 12),
          strip.text = element_text(size = 12)) +
    guides(color = guide_legend(title = "Survey")) +
    scale_y_continuous(labels = scales::comma) +
    labs(x = "Year", y = 'Biomass (t)', title = '') +
    xlim(1987,2024) # +
    # annotate(geom="label", x=1987, y=mean_vec[1], label=format(round(mean_vec[1]), big.mark=","),
    #          color="#7E1700") +
    # annotate(geom="label", x=1998, y=mean_vec[2], label=format(round(mean_vec[2]), big.mark=","),
    #          color='#C7B354') +
    # annotate(geom="label", x=1987, y=mean_vec[3], label=format(round(mean_vec[3]), big.mark=","),
    #          color='#023198')
  print(p_survey2)
  
  ggsave(here(year, "figs", "Presentations", paste(year,"Octopus_surveys.png",sep = "_")), dpi = 300, units = 'in',
         height = 4, width = 7, bg = 'white')
}


## survey specifics
sp_srv <- raw_srv %>%
  mutate(sp_name = case_when(species_code %in% c(78010,78011, 78030, 78013,78020, 70300, 7404) ~ "octopus unid.",
                             species_code== 78012 ~ "B. leioderma",
                             species_code== 78022 ~ "J. diaphana",
                             species_code== 78403 ~ "E. dofleini",
                             species_code== 78452 ~ "Benthoctopus sp",
                             species_code== 78454 ~ "S. salebrosus",
                             species_code== 78455 ~ "B. oregonensis",
                             species_code %in% c(78301)  ~ "O. californiana",
                             species_code %in% c(78352,78353) ~ "G. boreopacifica",
                             .default = "octopus unid."),
         area = case_when(area_id==99900 ~ "EBS shelf",
                          area_id==99905 ~ "EBS slope",
                          area_id==99904 ~ "AI")) %>%
  summarise(biomass = sum(biomass_mt),
            var = sum(biomass_var, na.rm=T),
            .by = c(year, area, sp_name)) %>% 
  filter(biomass > 0) %>%
  mutate(cv = sqrt(var) / biomass * 100)

# Just E>dolfeini defined
sp_srv2 <- raw_srv %>%
  mutate(sp_name = case_when(species_code== 78403 ~ "E. dofleini",
                             .default = "Other Octopus"),
         area = case_when(area_id==99900 ~ "EBS shelf",
                          area_id==99905 ~ "EBS slope",
                          area_id==99904 ~ "AI")) %>%
  summarise(biomass = sum(biomass_mt),
            var = sum(biomass_var, na.rm=T),
            .by = c(year, area, sp_name)) %>% 
  filter(biomass > 0) %>%
  mutate(cv = sqrt(var) / biomass * 100)

al_tot <- sp_srv %>%
  filter(area == "AI") %>%
  arrange(year) %>%
  summarise(total_biomass = sum(biomass),
            var = sum(var, na.rm=T),
            .by = c(year, area )) %>%
  mutate(total_cv = sqrt(var) / total_biomass * 100) %>%
  select(-var, -area)


al_srv <- sp_srv %>%
  filter(area == "AI") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(al_tot)  %>%
  mutate_if(is.numeric, round, 0)

temp <- colnames(al_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
if(temp[temp2[1]] == "biomass_octopus unid."){
  temp2 <- c(temp2[2:length(temp2)],temp2[1])
}
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam <- c(sp_nam, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

al_srv <- al_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')


al_sub_srv <- sp_srv2 %>%
  filter(area == "AI") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(al_tot)  %>%
  mutate_if(is.numeric, round, 0)

temp <- colnames(al_sub_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
temp2 <- c(temp2[2:length(temp2)],temp2[1])
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam_sub <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam_sub <- c(sp_nam_sub, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

al_sub_srv <- al_sub_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')




if(export_dat == T){
  write.csv(al_srv, here(year, "data", "output", paste(year,"octopus_AI_survey.csv",sep = "_")),row.names = F)
  write.csv(al_sub_srv, here(year, "data", "output", paste(year,"octopus_AI_survey_sub.csv",sep = "_")),row.names = F)
  for(i in 1:length(sp_nam)){
    if(i == 1){
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","AI_species.out"))
    }else{
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","AI_species.out"),append = T)
    }
  }
  cat("\n",file = here(year, "data","output","AI_species.out"),append = T)
} 


## Shelf
shelf_tot <- sp_srv %>%
  filter(area == "EBS shelf") %>%
  arrange(year) %>%
  summarise(total_biomass = sum(biomass),
            var = sum(var, na.rm=T),
            .by = c(year, area )) %>%
  mutate(total_cv = sqrt(var) / total_biomass * 100) %>%
  select(-var, -area)


shelf_srv <- sp_srv %>%
  filter(area == "EBS shelf") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(shelf_tot)  %>%
  mutate_if(is.numeric, round, 0)

temp <- colnames(shelf_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
if(temp[temp2[1]] == "biomass_octopus unid."){
  temp2 <- c(temp2[2:length(temp2)],temp2[1])
}
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam <- c(sp_nam, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

shelf_srv <- shelf_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')


shelf_sub_srv <- sp_srv2 %>%
  filter(area == "EBS shelf") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(shelf_tot)  %>%
  mutate_if(is.numeric, round, 0)

temp <- colnames(shelf_sub_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
temp2 <- c(temp2[2:length(temp2)],temp2[1])
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam_sub <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam_sub <- c(sp_nam_sub, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

shelf_sub_srv <- shelf_sub_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')


if(export_dat == T){
  write.csv(shelf_srv, here(year, "data", "output", paste(year,"octopus_EBS_shelf_survey.csv",sep = "_")),row.names = F)
  write.csv(shelf_sub_srv, here(year, "data", "output", paste(year,"octopus_EBS_shelf_survey_sub.csv",sep = "_")),row.names = F)
  for(i in 1:length(sp_nam)){
    if(i == 1){
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","EBS_shelf_species.out"))
    }else{
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","EBS_shelf_species.out"),append = T)
    }
  }
  cat("\n",file = here(year, "data","output","EBS_shelf_species.out"),append = T)
} 

## Slope survey
slope_tot <- sp_srv %>%
  filter(area == "EBS slope") %>%
  arrange(year) %>%
  summarise(total_biomass = sum(biomass),
            var = sum(var, na.rm=T),
            .by = c(year, area )) %>%
  mutate(total_cv = sqrt(var) / total_biomass * 100) %>%
  select(-var, -area)


slope_srv <- sp_srv %>%
  filter(area == "EBS slope") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(slope_tot)  %>%
  mutate_if(is.numeric, round, 0)

temp <- colnames(slope_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
if(temp[temp2[1]] == "biomass_octopus unid."){
  temp2 <- c(temp2[2:length(temp2)],temp2[1])
}
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam <- c(sp_nam, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

slope_srv <- slope_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')

slope_sub_srv <- sp_srv2 %>%
  filter(area == "EBS slope") %>%
  arrange(year) %>%
  select(-var) %>%
  pivot_wider(names_from = sp_name, values_from = c(biomass, cv)) %>%
  left_join(slope_tot)  %>%
  mutate_if(is.numeric, round, 0)


temp <- colnames(slope_sub_srv)
temp2 <- grep("biomass",temp)
temp2 <- temp2[-length(temp2)]
temp2 <- c(temp2[2:length(temp2)],temp2[1])
temp_nam <- temp[temp2]
col_ord <- c()
sp_nam_sub <- c()
for(i in 1:length(temp2))
{
  temp3 <- unlist(strsplit(temp[temp2[i]], split = "_"))[2]
  col_ord <- c(col_ord, grep(temp3, temp))
  sp_nam_sub <- c(sp_nam_sub, unlist(strsplit(temp_nam[i],split = "_"))[2])
}

slope_sub_srv <- slope_sub_srv %>%
  select(1,2,all_of(col_ord),'total_biomass','total_cv')

if(export_dat == T){
  write.csv(slope_srv, here(year, "data", "output", paste(year,"octopus_EBS_slope_survey.csv",sep = "_")),row.names = F)
  write.csv(slope_sub_srv, here(year, "data", "output", paste(year,"octopus_EBS_slope_survey_sub.csv",sep = "_")),row.names = F)
  for(i in 1:length(sp_nam)){
    if(i == 1){
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","EBS_slope_species.out"))
    }else{
      cat(paste(sp_nam[i],"_",sep = ""), file = here(year, "data","output","EBS_slope_species.out"),append = T)
    }
  }
  cat("\n",file = here(year, "data","output","EBS_slope_species.out"),append = T)
} 




