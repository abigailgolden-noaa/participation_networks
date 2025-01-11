## Compare Dan Holland's species groupings with the gear-species combos used for the IOPAC model

library(tidyverse)

# read in data
indir = "/Users/abigail.golden/Desktop/Research/24 - CEFI/data/species groupings"


dans_file <- "Holland_species_groups.csv"
allens_file <-  "species_dat.csv"

dans_groups <- read.csv(here::here(indir,dans_file)) %>% 
  dplyr::select(PACFIN_SPECIES_CODE, PACFIN_SPECIES_COMMON_NAME, species_group_2_label)
  
allens_groups <- read.csv(here::here(paste0(indir, "/IOPAC lookup tables"), allens_file)) %>% 
  dplyr::select(SPID, SPECIES_NAME, SP_SUM)

# join the two dataframes

key <- full_join(dans_groups, allens_groups, by = join_by("PACFIN_SPECIES_CODE" =="SPID"))
