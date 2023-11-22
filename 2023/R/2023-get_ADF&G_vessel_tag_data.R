# Doesn't work


library(here)
library(keyring)  # accessing my username and passwords for database
library(RODBC)    # used to access database in R
library(readr)
library(tidyverse)
library(dplyr)

# AKFIN database #
database_akfin <- 'akfin'
channel_akfin <- RODBC::odbcConnect(database_akfin, uid = key_list(database_akfin)$username,
                                    pwd = key_get(database_akfin, keyring::key_list(database_akfin)$username),
                                    believeNRows=FALSE)  # gains access to database




query_sur <- "select  *
             from     council.COMPREHENSIVE_FT
             where    CFEC_PMT_FSHY in (O 06B,O 09B,O 026B)" 
             
temp_channel <- channel_akfin


## Get Survey Data ## 
test <- RODBC::sqlQuery(temp_channel, query = query_sur)




