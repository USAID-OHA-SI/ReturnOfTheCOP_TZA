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
        mutate(site = str_remove_all(site, " -.*$| \\."))
    
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
           site = str_remove_all(site, " -.*$| \\."),
           site = case_when(site == "Njombe health centre Health Center" ~ "Njombe Heath centre",
                            site == "Makole HC Health Center" ~ "Makole Health Center",
                            TRUE ~ site))
  
 df <- df %>% 
    left_join(df_visits, by = c("site", "week")) %>% 
    select(month, week, visit_date, everything())



# INDEX TESTING TREND -----------------------------------------------------
  
  plot_index <- function(df, plot_subtitle, site_filter = NULL){
    
    df_index <- df %>% 
      filter(indicator %in%  c("INDEX_A", "INDEX_SEX", "HTS_SEX", "POS_SEX")) %>% 
      mutate(indicator = factor(indicator, c("INDEX_A", "INDEX_SEX", "HTS_SEX", "POS_SEX")),
             clean_date = paste(month(week, label = TRUE), day(week)),
             clean_date = fct_reorder(clean_date, week)) 
      
    if(!is.null(site_filter))
      df_index <- filter(df_index, site %in% site_filter)
    
    p <- df_index %>% 
      ggplot(aes(indicator, val, fill = indicator)) +
      geom_col() +
      scale_fill_brewer(palette = "Set1") +
      labs(x = "", y = "",
           title = "Weekly Index Testing Cascade",
           subtitle = plot_subtitle,
           caption = "Source: Weekly Partner Reporting Data for 71 USAID sites [Deloitte and EGPAF]") +
      theme_light() +
      theme(text = element_text(family = "Gill Sans MT", size = 13, color = "#595959"),
            legend.position = "bottom",
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.text.x = element_blank(),
            legend.title = element_blank())
    
    if(is.null(site_filter)) {
      p + facet_grid(region ~ clean_date)
    } else {
      p + facet_grid(site ~ clean_date)
    }
  }
  
  
  plot_index(df, "All USAID Surge Sites")
  
  ggsave("out/COP19_TZA_Index_Regional_Trend.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  
  highlight_sites <- df %>% 
    filter(week >= as.Date("2019-03-31"), week < as.Date("2019-04-14"),
           indicator %in% c("TX_NEW_F", "TX_NEW_M", "INDEX_A")) %>%
    mutate(indicator = str_remove(indicator, "_(F|M)")) %>% 
    group_by(partner, region, site, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(share = INDEX_A/TX_NEW) %>% 
    arrange(share) %>% 
    filter(ntile(TX_NEW, 4) == 4) 
  
  highlight_sites_poor <- highlight_sites %>% 
    slice(1:2) %>% 
    pull(site)
    
  highlight_sites_good <- highlight_sites %>% 
    tail(2) %>% 
    pull(site)

  
  plot_index(df, "Large sites with lowest share of partners elicited (from TX_NEW)", highlight_sites_poor)
  ggsave("out/COP19_TZA_Index_Example_Poor.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  
  plot_index(df, "Large sites with highest share of partners elicited (from TX_NEW)", highlight_sites_good)
  ggsave("out/COP19_TZA_Index_Example_Good.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  

# RETENTION TREND ---------------------------------------------------------

  
  plot_retention <- function(df, plot_subtitle, site_filter = NULL){
    
    df_ret <- df %>% 
      filter(indicator %in% c("TX_CURR", "XFER_IN", "TX_PREV", "TX_NEW_M", "TX_NEW_F", "TX_NEW_PEDS"),
             week < as.Date("2019-04-7"), 
             !is.na(region)) 
    
    if(!is.null(site_filter))
      df_ret <- filter(df_ret, site %in% site_filter)
    
    group_vars <- c("week", "region", "indicator")
    
    if(!is.null(site_filter)) 
      group_vars <- c(group_vars, "site", "visit_date")
    

    df_ret <- df_ret %>% 
      group_by_at(vars(one_of(group_vars))) %>% 
      summarise_at(vars(val), sum, na.rm = TRUE) %>% 
      ungroup() %>% 
      spread(indicator, val, fill = 0) %>% 
      mutate(TX_RET = (TX_CURR - XFER_IN)/(TX_PREV + TX_NEW_F + TX_NEW_M + TX_NEW_PEDS))
    
    p <- df_ret %>% 
      ggplot(aes(week, TX_RET)) +
      geom_line(size = 1, color = "#377EB8") +
      geom_point(size = 4, color = "#377EB8") +
      scale_y_continuous(label = percent_format(1)) +
      labs(x = "", y = "",
           title = "Weekly Retention Trend",
           subtitle = plot_subtitle,
           caption = "Note: Retention = (TX_CURR - XFER_IN) / (TX_PREV + TX_NEW)
         Source: Weekly Partner Reporting Data for 71 USAID sites [Deloitte and EGPAF]") +
      theme_light() + 
      theme(text = element_text(family = "Gill Sans MT", size = 13, color = "#595959"),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            legend.title = element_blank())
    
    if(is.null(site_filter)){
      p + facet_wrap(region ~ .)
    } else {
      p + 
        geom_vline(aes(xintercept = visit_date), 
                   linetype = "dashed",
                   na.rm = TRUE) +
        facet_wrap(site ~ .) 
    }
      
  }
  
  plot_retention(df, "All USAID Surge Sites")
  ggsave("out/COP19_TZA_Retention_Regional_Trend.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  
  highlight_sites <- df %>% 
    filter(week >= as.Date("2019-03-01"), week < as.Date("2019-04-01"),
           indicator %in% c("TX_CURR", "XFER_IN", "TX_PREV", "TX_NEW_M", "TX_NEW_F", "TX_NEW_PEDS"),
           week < as.Date("2019-04-7"), 
           !is.na(region)) %>%
    group_by(partner, region, site, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>% 
    mutate(TX_RET = (TX_CURR - XFER_IN)/(TX_PREV + TX_NEW_F + TX_NEW_M + TX_NEW_PEDS)) %>% 
    arrange(TX_RET) %>% 
    filter(TX_CURR > 0, TX_PREV > 0,
           ntile(TX_CURR, 4) == 4) 
  
  highlight_sites_poor <- highlight_sites %>% 
    slice(1:2) %>% 
    pull(site)
  
  highlight_sites_good <- highlight_sites %>% 
    tail(2) %>% 
    pull(site)
  
  plot_retention(df, "Large sites with lowest retention", highlight_sites_poor)
  ggsave("out/COP19_TZA_Retention_Example_Poor.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  plot_retention(df, "Large sites with lowest retention", highlight_sites_good)
  ggsave("out/COP19_TZA_Retention_Example_Poor.png", dpi = 300,
         width = 11, height = 8.5, units = "in")
  


# CASCADE TREND -----------------------------------------------------------
  
  #joining mapping to monthly data, dropping all non-essential indicators
  load("data/key_ind.rda")
  key_ind <- key_ind %>% 
    select(indicator = ind_monthly, ind = indicator, -sex, -age)
  
  df_cas <- inner_join(df, key_ind, by = "indicator") %>% 
    mutate(indicator = ind) %>% 
    select(-ind, -indicator_label)
  
  rm(key_ind)
  
  df_cas <- df_cas %>% 
    group_by(week, region, indicator) %>% 
    summarise_at(vars(val), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    spread(indicator, val, fill = 0) %>%
    mutate(Positivity = HTS_TST_POS/HTS_TST,
           Linkage = TX_NEW/HTS_TST_POS,
           Retention = (TX_CURR - XFER_IN)/(TX_CURR_prior + TX_NEW)) %>% 
    select(-HTS_TST_POS:-XFER_IN) %>% 
    gather(indicator, val, -week, -region)
  
  df_weekly_hts <- df_cas %>% 
    filter(indicator == "HTS_TST") %>% 
    group_by(region, indicator) %>% 
    mutate(max = max(val)*1.1,
           lab = case_when((week == min(week) | week == max(week)) ~ comma(val))) %>% #used to keep labels within bounds
    ungroup() 
  
  df_weekly_pct  <- df_cas %>% 
    mutate(val = ifelse(val == 0, NA, val)) %>% 
    filter(indicator != "HTS_TST",
           !is.na(val)) %>% 
    group_by(region, indicator) %>% 
    mutate(max = max(val) + ifelse(max(val) < .15, .05, .15),
           lab = case_when(indicator == "Positivity" & (week == min(week) | week == max(week)) ~ percent(val, .1),
                           (week == min(week) | week == max(week)) ~ percent(val, 1))) %>% #used to keep labels within bounds
    ungroup() %>% 
    mutate(indicator = factor(indicator, c("Positivity", "Linkage", "Retention")))
    
    
  plot_cascade <- function(region_sel, filepath_save = NULL){
    
    #graph title
    partner_title <- paste(region_sel, "Cascade Trends")
    
    #grah HTS bar chart
    viz_hts <- df_weekly_hts %>% 
      filter(region == region_sel) %>% 
      ggplot(aes(week, val)) +
      geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
      geom_col() +
      geom_point(aes(y = max), color = "white", size = 0) +
      geom_text(aes(label = lab),
                family = "Gill Sans MT",
                vjust = -1,
                color = "#595959",
                na.rm = TRUE) +
      scale_y_continuous(labels = comma) +
      scale_fill_manual(values = c("#67A9CF", "#2166AC")) +
      expand_limits(y = 0) +
      facet_grid(. ~indicator, scales = "free_y") +
      labs(x = "", y = "") +
      theme_light() +
      theme(legend.position = "none",
            panel.border = element_blank(),
            text = element_text(family = "Gill Sans MT", color = "#595959"),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold")
      )
    
    #graph positivity/linkage/retention
    viz_pct <- df_weekly_pct %>% 
      filter(region == region_sel) %>% 
      ggplot(aes(week, val)) +
      geom_hline(yintercept = 0, color = "#bfbfbf", size = .5) +
      geom_point(aes(y = max), color = "white", size = 0, na.rm = TRUE) +
      geom_line(size = 1, color = "#595959", na.rm = TRUE) +
      geom_point(size = 4, color = "#595959",  na.rm = TRUE) +
      geom_text(aes(label = lab),
                family = "Gill Sans MT",
                vjust = -1,
                color = "#595959",
                na.rm = TRUE) +
      expand_limits(y = 0) +
      facet_grid(indicator ~ ., scales = "free_y", switch = "y") +
      scale_y_continuous(labels = percent_format(1)) +
      scale_color_manual(values = c("#67A9CF", "#2166AC")) +
      labs(x = "", y = "") +
      theme_light() +
      theme(legend.position = "none",
            panel.border = element_blank(),
            text = element_text(family = "Gill Sans MT", color = "#595959", size = 12),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            panel.grid.minor.x = element_blank(),
            strip.text = element_text(size = 12, face = "bold"))
    
    #combine
    if(!is.null(filepath_save)){
      plot <- arrangeGrob(viz_hts, viz_pct, ncol = 2, 
                          top = textGrob(partner_title, 
                                         gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
                                         hjust = 1,
                                         x = .2),
                          bottom = textGrob(
                            "Source: Weekly Partner Reporting Data for 71 USAID sites [Deloitte and EGPAF]",
                            gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
                            hjust = 1,
                            x = 1
                          ))
      
    } else {
      plot <- grid.arrange(viz_hts, viz_pct, ncol = 2,
                           top = textGrob(partner_title,
                                          gp = gpar(fontfamily = "Gill Sans MT", fontsize = 13),
                                          hjust = 1,
                                          x = .2),
                           bottom = textGrob(
                             "Source: Weekly Partner Reporting Data for 71 USAID sites [Deloitte and EGPAF]",
                             gp = gpar(fontfamily = "Gill Sans MT", fontsize = 9, col = "#595959"),
                             hjust = 1,
                             x = 1
                           ))
      return(plot)
      
    }
    
    
    if(!is.null(filepath_save)){
      ggsave(paste0("COP19_TZA_Cascade_v_", format(Sys.time(), "%H%M%OS1"), "_", region_sel, ".png"), 
             plot,
             path = filepath_save,
             height = 5.5, width = 9.5, units = "in", dpi = 300)
      print(paste(region_sel, "...saved"))
    }
    
  }  
  
  regs <- df_cas %>% 
    filter(!is.na(region)) %>% 
    distinct(region) %>% 
    pull()
  
  plot_cascade(regs[1])
  
  walk(.x = regs,
       .f = ~ plot_cascade(.x, "out/"))
  
######################################################
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
  
    
 
