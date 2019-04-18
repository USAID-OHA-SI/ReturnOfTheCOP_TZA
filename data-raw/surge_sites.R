library(tidyverse)
library(readxl)
library(usethis)

#import
df_monthy <- read_excel("data-raw/Facility Basic Raw Data.xlsx")

surge_sites <- df_monthy %>% 
  rename_all(tolower) %>% 
  filter(`241 site` == TRUE) %>% 
  pull(facilityuid)

save(surge_sites, file = "data/surge_sites.rda")
