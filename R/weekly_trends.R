library(tidyverse)
library(readxl)
library(scales)
library(lubridate)
library(extrafont)
library(RColorBrewer)

# function to import and munge weekly template ----------------------------

  import_weekly <- function(path){
    
    #import mapping between indicator names and labels
      ind_names <- read_excel(path, sheet = "Guide-Facility", range = "E3:F47", 
                              col_names = c("indicator", "indicator_label"))
      
    #unwanted tabs (don't have site data)
      remove <- c("Guide-Facility", "Guide-Comm", "Facility Data", "Community Data")
    
    #import all site sheets
      df <- path %>% 
        excel_sheets() %>% 
        setdiff(remove) %>% 
        set_names() %>% 
        map_dfr(~read_excel(path, sheet = .x, skip = 3, col_types = "text")) 
    
    #munge and reshape long
      df <- df %>% 
        select(-matches("\\..."), -`End date`, -`Initial visit`) %>% #get rid of extra blank columns at end
        mutate_at(vars(`Start date`), 
                  ~ as.double(.) %>% as.Date(origin = "1899-12-30")) %>% #convert date
        mutate_all(~na_if(., 0)) %>% 
        gather(indicator_label, val, -`Start date`:-`Site ID (from DATIM)`, na.rm = TRUE) %>% #reshape long
        mutate(val = as.double(val),
               month = month(`Start date`, label = TRUE) %>% factor(., c("Dec", "Jan", "Feb", "Mar", "Apr"))) %>% 
        select(month, `Start date`, everything())
    
    #clean names
      df <- df %>% 
        rename(week = `Start date`,
               site = `Site Name`,
               orgunituid = `Site ID (from DATIM)`) %>% 
        rename_all(tolower) %>% 
        mutate(site = str_remove_all(site, " -.*$"))
    
    #map indicator name on
      df <- df %>% 
        left_join(ind_names, by = "indicator_label") %>% 
        select(-indicator_label, -val, everything())
      
    return(df)
      
  }
  
  

# Import data -------------------------------------------------------------

  #import from weekly folder
    weekly_files <- list.files("data/Weekly Data", full.names = TRUE)
    df <- map_dfr(weekly_files, import_weekly)
    
  #fix missing partner name
    df <- df %>% 
      mutate(partner = ifelse(is.na(partner), "EGPAF", partner),
             agency = ifelse(is.na(agency), "USAID", agency))
  
  #add in visit date
  df_visits <- read_excel("data/Site Monitoring Schedule _USAID 4.11.2019.xlsx") 
  
  df_visits <- df_visits %>% 
    select(site = Site, Initial = `First(initial) Visit`, `Follow Up` = `Scheduled Visit  2`) %>% 
    gather(visit_type, visit_date, -site, na.rm = TRUE)  %>% 
    mutate(visit_date = ymd(visit_date), 
           week = floor_date(visit_date, unit="week"),
           site = str_remove_all(site, " -.*$|HC "))
  
 df <- df %>% 
    left_join(df_visits, by = c("site", "week")) %>% 
    select(month, week, visit_date, everything())

#viz
  df %>% 
    filter(indicator == "POS_SEX") %>% 
    ggplot(aes(week, val, fill = month)) +
    geom_col() +
    labs(x = "", y = "",
         subtitle = "Adults with sexual contacts elicited during the three latest months") +
    theme_light() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 13),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank())

 
  
  df %>% 
    filter(indicator %in%  c("INDEX_A", "INDEX_SEX", "HTS_SEX", "POS_SEX")) %>% 
    mutate(indicator = factor(indicator, c("INDEX_A", "INDEX_SEX", "HTS_SEX", "POS_SEX"))) %>% 
    ggplot(aes(indicator, val, fill = month)) +
    geom_col() +
    labs(x = "", y = "",
         subtitle = "Index Testing Cascade") +
    facet_grid(. ~ week) +
    theme_light() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 13),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank())
  
  
  df_cas <- df %>% 
    mutate(ind = case_when(indicator %in% c("HTS_M", "HTS_F", "HTS_PEDS") ~ "HTS_TST",
                           indicator %in% c("POS_M", "POS_F", "POS_PEDS") ~ "HTS_POS",
                           indicator %in% c("TX_NEW_M", "TX_NEW_F", "TX_NEW_PEDS") ~ "TX_NEW")) %>% 
    filter(!is.na(ind)) %>% 
    mutate(ind = factor(ind, c("HTS_TST", "HTS_POS", "TX_NEW"))) %>% 
    group_by(partner, site, ind, week, month, visit_date) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup()
  
  #order sites by HTS_POS volumne in latest week
    site_order <- df_cas %>% 
      filter(ind == "HTS_POS") %>% 
      group_by(partner, site) %>% 
      filter(week == max(week)) %>% 
      ungroup() %>% 
      arrange(desc(val)) %>% 
      pull(site)
    
    df_cas <- mutate(df_cas, site = factor(site, site_order))
  
  df_cas %>% 
    filter(partner == "Deloitte Consulting") %>% 
    ggplot(aes(week, val, color = month, #group = month
               )) +
    #geom_col() +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = visit_date), 
               linetype = "dashed",
               na.rm = TRUE) +
    geom_line(size = 1, color = "#595959") +
    geom_point(size = 4) +
    scale_y_continuous(labels = comma) +
    expand_limits(y = 0) +
    facet_grid(ind ~ site, scales = "free") +
    labs(x = "", y = "",
         title = "FY19Q2 Weekly Trend in Deloitte Morogoro Sites",
         subtitle = "pre- and post- USG site visit; sites ordered on their latest HTS_POS volume",
         caption = "Note: dotted line indicates USG site visit and color depicts month
         Source: Deloitte, Additional_Weekly_Report_Data_Morogoro_17th_April_2019.xlsx") +
    theme_light() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 11),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(size = 9, color = "#595959"))
  
  ggsave("out/TZA_Deloitte_FY19Q2_Weekly_Morogoro.png", dpi = 300, width = 11, height = 8.5, units = "in")
  
  df %>%
    filter(partner == "Deloitte Consulting",
           indicator == "HTS_SEX") %>% 
    group_by(site, month, week, visit_date) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    ggplot(aes(week, val, fill = month)) +
    geom_col() +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = visit_date), 
               linetype = "dashed",
               na.rm = TRUE) +
    # geom_line(size = 1, color = "#595959") +
    # geom_point(size = 4) +
    scale_y_continuous(labels = comma) +
    expand_limits(y = 0) +
    facet_grid(. ~ site, scales = "free") +
    labs(x = "", y = "",
         title = "FY19Q2 Weekly Trend in Deloitte Morogoro Sites",
         subtitle = "pre- and post- USG site visit; sites ordered on their latest HTS_POS volume",
         caption = "Note: dotted line indicates USG site visit and color depicts month
         Source: Deloitte, Additional_Weekly_Report_Data_Morogoro_17th_April_2019.xlsx") +
    theme_light() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 13),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(size = 10, color = "#595959"))

  link <- df %>% 
    mutate(indicator = ifelse(indicator %in% c("POS_M", "POS_F"), "HTS_POS", indicator)) %>% 
    filter(indicator %in% c("HTS_POS", "LCM_A")) %>% 
    group_by(partner, indicator, month, week, visit_date, site) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(link = LCM_A/HTS_POS,
           link = ifelse(is.na(HTS_POS), NA, link))
  
  link %>% 
    filter(partner == "Deloitte Consulting") %>% 
    ggplot(aes(week, link, color = month)) +
    geom_hline(aes(yintercept = 0)) +
    geom_vline(aes(xintercept = visit_date), 
               linetype = "dashed",
               na.rm = TRUE) +
    geom_line(size = 1, color = "#595959", na.rm = TRUE) +
    geom_point(size = 4, na.rm = TRUE) +
    scale_y_continuous(labels = percent) +
    facet_grid(. ~ site) +
    labs(x = "", y = "") +
    theme_light() +
    theme(legend.position = "none",
          text = element_text(family = "Gill Sans MT", size = 13),
          axis.ticks = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          plot.caption = element_text(size = 10, color = "#595959"))
  
    
 
