library(tidyverse)
library(readxl)
library(scales)
library(extrafont)


# Import Data Pack --------------------------------------------------------


df_tza_geo <- read_rds("~/ICPI/Data/MER_Structured_Dataset_PSNU_IM_FY17-19_20190322_v2_1.rds") %>% 
  filter(operatingunit == "Tanzania") %>% 
  distinct(snu1, psnu, psnuuid)

#Flatpack ouput generated from "TZ COP 19 Data Pack 20190328" (https://jason.datim.org/shiny/apps/datapack/)
  df_dp <- read_excel("data/flatpack_20190422_184237.xlsx", sheet = "SUBNAT_IMPATT", col_types = "text")

  df_dp <- df_dp %>% 
    mutate(value = as.numeric(value)) %>% 
    filter(indicatorCode %in% c("PLHIV.NA.Age/Sex/HIVStatus.20T", "TX_CURR_SUBNAT.N.Age/Sex/HIVStatus.20T")) %>% 
    separate(indicatorCode, c("indicator", NA, NA, NA), sep = "\\.")
  
  df_dp_viz <- df_dp %>% 
    left_join(df_tza_geo, by = c("psnuid" = "psnuuid")) %>% 
    group_by(snu1, indicator) %>% 
    summarise_at(vars(value), sum, na.rm = TRUE) %>% 
    ungroup() %>% 
    filter(snu1 != "_Military Tanzania") %>% 
    spread(indicator, value) %>% 
    mutate(coverage = TX_CURR_SUBNAT/PLHIV)
  
  df_dp_viz %>% 
    ggplot(aes(reorder(snu1, -coverage))) +
    geom_col(aes(y = PLHIV), 
             size = 1.3, width = .7,
             fill = NA, color = "#595959") +
    geom_col(aes(y = TX_CURR_SUBNAT),  width = .7) +
    geom_text(aes(y = 0, label = percent(coverage, 1)),
              hjust = ifelse(df_dp_viz$TX_CURR_SUBNAT < 5000, -1, -.08),
              color = ifelse(df_dp_viz$TX_CURR_SUBNAT < 5000, "#595959", "white"),
              family = "Gill Sans MT", size = 3.5) +
    scale_y_continuous(labels = comma) +
    coord_flip() +
    labs(x = "", y = "PLHIV",
         title = "NATIONAL TREATMENT COVERAGE",
         subtitle = "% of PLHIV on Treatment",
         caption = "Source: TZ COP 19 Data Pack 20190328") +
    theme_light() +
    theme(panel.border = element_blank(),
          axis.ticks = element_blank(),
          panel.grid.major.y = element_blank(),
          text = element_text(family = "Gill Sans MT", color = "#595959"),
          axis.text = element_text(size = 13),
          plot.caption = element_text(family = "Gill Sans MT", color = "#595959"),
          axis.title = element_text(size = 13))
  
  ggsave("out/COP19_TZA_regional_coverage_gap.png", dpi = 300,
         width = 8, height = 8, units = "in")
            