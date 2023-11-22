### Plot the time series of Net Mortality

library(ggplot2)
library(tidyverse)
library(dplyr)
library(here)
library(scico)
library(afscassess)

theme_set(theme_report())

cur_yr <- 2023

# read in mean Net mortality values

dat_mort <- read.csv(here(cur_yr,"data","user_inputs","cons_summary_1million_samples.csv")) %>%
  mutate(ln_g_mean = log(g_mean)) %>%
  mutate(new_old = case_when(year <= 2011 ~ "old",
                             year == 2012 ~ "new",
                             year == 2013 ~ "new",
                             year == 2014 ~ "old",
                             year == 2015 ~ "old",
                             year >= 2016 ~ "new")) %>%
  mutate(across(new_old, factor, levels=c("old","new")))

# the raw data
p_net_mort <- ggplot(dat_mort, aes(x = year, y = g_mean)) + 
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.2, position=position_dodge(width=0.6)) +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  theme(axis.text = element_text(size =12),
        axis.title = element_text(size =12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  theme(legend.position = c(.68, .85)) +
  guides(color = guide_legend(title = "Survey")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = 'Biomass (t)', title = 'Estimated Consumption of Octopus by P.cod') +
  xlim(1987,2024)
print(p_net_mort)

ggsave(here(cur_yr, "figs","Presentations", paste(cur_yr,"Octopus_net_mort.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 9, bg = 'white')


# distinguish new and old
p_net_mort_new_old <- ggplot(dat_mort, aes(x = year, y = g_mean, color = new_old)) + 
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.2, position=position_dodge(width=0.6)) +
  scale_color_manual(values = c("#7E1700", "#023198")) +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  theme(axis.text = element_text(size =12),
        axis.title = element_text(size =12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  theme(legend.position = c(.68, .85)) +
  guides(color = guide_legend(title = "")) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = 'Biomass (t)', title = 'Estimated Consumption of Octopus by P.cod') +
  xlim(1987,2024)
print(p_net_mort_new_old)

ggsave(here(cur_yr, "figs","Presentations", paste(cur_yr,"Octopus_net_mort_new_old.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 9, bg = 'white')


p_net_mort_OFL <- ggplot(dat_mort, aes(x = year, y = g_mean, color = new_old)) + 
  geom_point(position=position_dodge(width=0.6)) +
  geom_errorbar(aes(ymin=X2.5., ymax=X97.5.), width=0.2, position=position_dodge(width=0.6)) +
  scale_color_manual(values = c("#7E1700","#023198")) +
  geom_hline(yintercept = 4769, lty = 2,  col = "#7E1700") +
  geom_hline(yintercept = exp(mean(dat_mort$ln_g_mean)), lty = 2,  col = "#023198") +
  theme(panel.grid.major = element_line(linewidth = 0.5)) +
  theme(axis.line = element_line()) +
  theme(axis.ticks = element_line(colour = "black")) +
  theme(axis.text = element_text(size =12),
        axis.title = element_text(size =12),
        legend.text = element_text(size = 12),
        strip.text = element_text(size = 12),
        plot.title = element_text(size = 20)) +
  theme(legend.position = c(.68, .85)) +
  guides(color = guide_legend(title = "")) +
  scale_y_continuous(labels = scales::comma) +
  # geom_label(label = '4769', x = 2018, y = 4300) +
  annotate(geom="label", x=2020, y=4300, label="4,769",
           color="#7E1700") +
  annotate(geom="label", x=2016, y=6080, label=format(round(exp(mean(dat_mort$ln_g_mean))), big.mark=","),
           color="#023198") +
  labs(x = "Year", y = 'Biomass (t)', title = 'Estimated Consumption of Octopus by P.cod') +
  xlim(1987,2024)
print(p_net_mort_OFL)

ggsave(here(cur_yr, "figs","Presentations", paste(cur_yr,"Octopus_net_mort_OFL.png",sep = "_")), dpi = 300, units = 'in',
       height = 4, width = 9, bg = 'white')



# geom_hline(yintercept = mean_vec[1], lty = 2,  col = "#7E1700") +
#   geom_hline(yintercept = mean_vec[2], lty = 2, col = '#C7B354') +
#   geom_hline(yintercept = mean_vec[3], lty = 2, col = '#023198') +
# scale_color_manual(values = c("#7E1700", "#C7B354","#023198")) +
