# PROJECT:  COP19 TZA
# AUTHOR:   A.CHAFETZ
# PURPOSE:  Create trend visuals for cascade indicators
# DATE:     2019-04-17

#dependencies
  library(tidyverse)
  library(readxl)
  library(scales)
  library(lubridate)
  library(extrafont)
  library(gridExtra)
  library(grid)


# Data from OLD Monthly Portal: Oct 18 - Feb 19 ---------------------------

  #import monthly data
    df_monthly <- read_excel("data/All Monthly Raw Data in given timeframe.xlsx",
                             col_types = "text")
  
  #tidy dataset
    df_monthly <- df_monthly %>% 
      mutate_all(na_if, 0) %>% 
      gather(ind_monthly, val, -Partner:-Month, na.rm = TRUE) %>% 
      mutate(val = as.double(val)) %>% 
      rename(orgunituid = `DATIM ID`) %>% 
      rename_all(tolower)
  
  #filter to just 241 surge sites
    load("data/surge_sites.rda")
    
    df_monthly <- df_monthly %>% 
      add_column(surge_site = as.character(NA), .after = "orgunituid") %>% 
      mutate(surge_site = ifelse(orgunituid %in% surge_sites, "TRUE", NA))
  
    rm(surge_sites)


# Data from NEW Monthly Portal: Mar 19 ------------------------------------

  #import monthly data
    df_monthly_mar_fac <- read_excel("data/Facility Basic Raw Data_Mar2019.xlsx", 
                                 col_types = "text")
    df_monthly_mar_comm <- read_excel("data/Community Basic Raw Data_Mar2019.xlsx", 
                                      col_types = "text")
    
    df_monthly_mar <- bind_rows(df_monthly_mar_fac, df_monthly_mar_comm)
      rm(df_monthly_mar_fac, df_monthly_mar_comm)
  
  #unify variables with OLD
    df_monthly_mar <- df_monthly_mar %>% 
      rename(region = `SNU1: Region`,
             district = `PSNU`,
             partner = `Partner APR19`,
             agency = Agency,
             site = Facility, 
             orgunituid = FacilityUID,
             surge_site = `241 Site`) %>% 
      select(-c(ID, MoH_ID, SNU1Uid, PSNUuid)) 
    
    df_monthly_mar <- df_monthly_mar %>% 
      mutate_all(na_if, 0) %>% 
      gather(ind_monthly, val, -region:-surge_site, na.rm = TRUE) %>% 
      mutate(val = as.double(val)) %>% 
      add_column(month = "Mar 2019", .after = "orgunituid")
    

# Combine data ------------------------------------------------------------

  #append NEW data for March onto OLD
    df_monthly <- df_monthly %>% 
      bind_rows(df_monthly_mar)
      rm(df_monthly_mar)
    
  #joining mapping to monthly data, dropping all non-essential indicators
    load("data/key_ind.rda")
  
    df_monthly <- inner_join(df_monthly, key_ind, by = "ind_monthly")
    rm(key_ind)

  #filter to just surge sites
    df_monthly <- filter(df_monthly, surge_site == "TRUE") 
  
  #aggregate up to indicator level
    df_monthly <- df_monthly %>% 
      select(-c(ind_monthly, sex, age)) %>% 
      group_by_if(is.character) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup()
  
  #clean up date, conver to R date
    df_monthly <- df_monthly %>% 
      mutate(month = str_replace(month, " ", " 1, "),
             month = mdy(month))


#create positivity, linkage and retention metrics (at partner level)
  df_monthly_pct <- df_monthly %>% 
    group_by(partner, agency, month, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>%
    mutate(Positivity = HTS_TST_POS/HTS_TST,
           Linkage = TX_NEW/HTS_TST_POS,
           Retention = TX_CURR/(TX_CURR_prior + TX_NEW),
           partner = str_replace(partner, "\\/", "-"))
  
#keep HTS_TST as raw values
  df_monthly_hts <- df_monthly_pct %>% 
    select(partner:HTS_TST) %>%
    gather(indicator, val, HTS_TST) %>% 
    mutate(pd = quarter(month, with_year = TRUE, fiscal_start = 10) %>% as.character()) %>% 
    group_by(partner, indicator) %>% 
    mutate(max = max(val)*1.1) %>% #used to keep labels within bounds
    ungroup()  

#munge dataset for pct graphing
  df_monthly_pct <- df_monthly_pct %>% 
    select(-HTS_TST:-TX_NEW) %>% 
    gather(indicator, val, Positivity:Retention) %>% 
    mutate(indicator = factor(indicator, c("Positivity", "Linkage", "Retention")),
           pd = quarter(month, with_year = TRUE, fiscal_start = 10) %>% as.character()) %>% 
    group_by(partner, indicator) %>% 
    mutate(max = max(val) + ifelse(max(val) < .15, .05, .15)) %>% #used to keep labels within bounds
    ungroup() %>% 
    mutate(val = ifelse(val == 0, NA, val))


#plot
  source("R/plot_cascade.R")
  
  plot_cascade("EGPAF")


#export
partners <- c("Deloitte", "EGPAF", "Baylor", "JSI-AIDSFree", #USAID "Jhpiego-Sauti", 
              "AGPAHI", "MDH", #CDC "ICAP",
              "HJF" #DOD
              )
map(.x = partners, 
    .f = ~ plot_cascade(.x, "out/"))


