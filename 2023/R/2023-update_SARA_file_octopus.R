## Update SARA file ##


library(here)
library(tidyverse)
library(dbplyr)

cur_yr <- 2023


# read in SARA file file
sara <- suppressWarnings(readLines(here(cur_yr,"SARA file","OCTOPUSBSAI2023_HQ.dat")))

sara[grep("#ASMT_YEAR",sara)+1] <- cur_yr

catch <- read.csv(here(cur_yr,"data", "output",paste(cur_yr,"octopus_total_catch_by_target.csv", sep = "_")))

specs <- read.csv(here(cur_yr,"data", "user_inputs","specs.csv")) 

cat_spec <- catch %>%
  left_join(specs) %>%
  mutate(Catch_ABC = round((Total/ABC)*100, 0))


# F_YEAR
sara[grep("#F_YEAR",sara) +1 ] <- cur_yr-1

# BEST_F_ESTIMATE
sara[grep("#BEST_F_ESTIMATE",sara) +1] <- cat_spec$Total[grep(cur_yr-1, cat_spec$Year)]

# F_LIMIT
sara[grep("#F_LIMIT",sara)[1] +1] <- cat_spec$OFL[grep(cur_yr-1, cat_spec$Year)]

# FISHERYYEAR
sara[grep("#FISHERYYEAR",sara) +1] <- paste(cat_spec$Year, collapse = "  ")

# TOTALCATCH
sara[grep("#TOTALCATCH",sara) +1] <- paste(cat_spec$Total, collapse = "  ")

write(sara, file = here(cur_yr, "SARA file","OCTOPUSBSAI2023_HQ.dat"))
