library(tidyverse)
library(readxl)
library(scales)
library(extrafont)

# Import and Munge MER data FY17Q1-FY19Q1 ---------------------------------

  df_tza <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-19_20190322_v2_1.rds") %>% 
    filter(operatingunit == "Tanzania")
  
  key_ind <- c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW")
  
  df_tza_key <- df_tza %>% 
    filter(indicator %in% key_ind,
           standardizeddisaggregate == "Total Numerator") %>% 
    group_by(snu1, indicator) %>% 
    summarise_at(vars(contains("q")), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    mutate(indicator = factor(indicator, key_ind)) %>% 
    arrange(snu1, indicator) %>% 
    gather(pd, val, contains("q"))


# Import and Munge TZA Monthly Data FY19Q2 --------------------------------

    files <- list.files("data", pattern = "Raw Data.*2019", full.names = TRUE)
  
  #import monthly data
    df_monthly <- map_dfr(.x = files, ~ read_excel(.x, col_types = "text"))
    
  #unify variables and aggregate up to SNU1
    df_monthly <- df_monthly %>% 
      select(snu1 = `SNU1: Region`, HTS_M, HTS_F, HTS_PEDS, POS_M, POS_F, POS_PEDS,
             TX_NEW_M, TX_NEW_F, TX_NEW_PEDS, TX_CURR, TX_PREV) %>% 
      mutate_at(vars(HTS_M:TX_PREV), as.numeric) %>% 
      mutate_at(vars(TX_CURR, TX_PREV), ~ ifelse(is.na(.), 0, .)) %>% 
      mutate(TX_NET_NEW = TX_CURR - TX_PREV) %>% 
      select(-TX_PREV) %>% 
      mutate_all(na_if, 0) %>% 
      gather(indicator, val, -snu1, na.rm = TRUE) %>% 
      mutate(pd = "fy2019q2",
             indicator = case_when(indicator %in% c("HTS_M", "HTS_F", "HTS_PEDS") ~ "HTS_TST",
                                   indicator %in% c("POS_M", "POS_F", "POS_PEDS") ~ "HTS_TST_POS",
                                   indicator %in% c("TX_NEW_M", "TX_NEW_F", "TX_NEW_PEDS") ~ "TX_NEW",
                                   TRUE ~ indicator)) %>% 
      group_by(snu1, indicator) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      mutate(pd = "fy2019q2",
             indicator = factor(indicator, key_ind)) %>% 
      select(-val, everything())
    

# Merge in FY19Q2 ---------------------------------------------------------

    df_tza_merge <- df_tza_key %>% 
      bind_rows(df_monthly) %>% 
      filter(snu1 != "_Military Tanzania") %>% 
      mutate(pd = str_remove(pd, "20") %>% toupper,
             fy = str_sub(pd, end = 4)) %>% 
      arrange(snu1, indicator)
    

# Vizualize ---------------------------------------------------------------

    plot_trend <- function(snu){
      
      plot_title <- paste("Trends in Core Indicators |", snu)
      savename <- paste0("Core_trends_", snu, ".png")
      
      viz <- df_tza_merge %>% 
        filter(snu1 == snu) %>% 
        ggplot(aes(pd, val, group = fy, color = indicator)) +
        geom_hline(yintercept = 0, color = "#595959") +
        geom_line(size = 1) +
        geom_point(size = 4) +
        labs(x = "", y = "",
             title = plot_title,
             caption = "Sources: (1) FY19Q1c MSD (2) Tanzania Monthly Data Reporting Poral (FY19Q2, Jan-Mar)") +
        expand_limits(y = 0) +
        scale_y_continuous(labels = comma) +
        scale_x_discrete(labels = c("FY17Q1", "", "", "","FY18Q1", "", "", "", "FY19Q1", "")) +
        facet_grid(indicator ~ ., scale = "free_y") +
        theme_light() +
        theme(legend.position = "none",
              panel.border = element_blank(),
              plot.caption = element_text(color = "#595959"),
              text = element_text(family = "Gill Sans MT"))
      
      ggsave(file.path("out", savename), viz, dpi = 300,
             width = 5, height = 8, units = "in")
      
      #return(plot)
    }

    
    plot_trend("Geita")    
    
    regions <- unique(df_tza_merge$snu1)
    
    
    walk(.x = regions, 
          .f = ~ plot_trend(.x))
        
