library(tidyverse)

# Date: 2019-04-23 9:24PM
# PSNU by IM
# Daily/Frozen: Daily
# Indicators: HTS_TST, HTS_TST_POS, TX_NEW, TX_CURR, TX_NET_NEW
# Disaggregate: Total Numerator
# Fiscal Year: 2019

df_tza <- ICPIutilities::match_msd("C:/Users/achafetz/Downloads/PEPFAR-Data-Genie-PSNUByIMs-2019-04-23.zip", save_rds = FALSE)

df_overall <- df_tza %>% 
  filter(fiscal_year == "2019",
         indicator %in% c("HTS_TST", "HTS_TST_POS", "TX_NEW", "TX_CURR", "TX_NET_NEW"),
         standardizeddisaggregate == "Total Numerator",
         ) %>% 
  mutate_at(vars(targets:cumulative), as.numeric) %>% 
  group_by(snu1, indicator, standardizeddisaggregate) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

# Date: 2019-04-23 9:29PM
# PSNU by IM
# Daily/Frozen: Daily
# Indicators: HTS_TST
# Fiscal Year: 2019

df_tza_hts <- ICPIutilities::match_msd("C:/Users/achafetz/Downloads/PEPFAR-Data-Genie-PSNUByIMs-2019-04-23 (1).zip", save_rds = FALSE)

df_hts <- df_tza_hts %>% 
  filter(fiscal_year == "2019",
         indicator == "HTS_TST",
         standardizeddisaggregate == "Modality/Age/Sex/Result") %>%
  mutate(type = ifelse(str_detect(modality, "Mod"), "Community", "Facility")) %>% 
  mutate_at(vars(targets:cumulative), as.numeric) %>% 
  group_by(snu1, fundingagency, mechanismid, primepartner, implementingmechanismname, indicator, standardizeddisaggregate, type) %>% 
  summarise_if(is.numeric, sum, na.rm = TRUE)

df_output <- bind_rows(df_hts, df_overall)

write_csv(df_output, "out/regional_achievement.csv", na = "")
