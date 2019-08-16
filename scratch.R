#### BRIEF ####
# help inform for provision of cancer treatment services in NHS Borders, would like to gain better
# understanding of the incidence of cancer in NHS Borders


#### DATA SOURCES ####
# incidence data
# https://www.opendata.nhs.scot/dataset/annual-cancer-incidence

# Board Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/3aef16b7-8af6-4ce0-a90b-8a29d6870014/download/opendata_inc9317_hb2018.csv

# Cancer Network Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/8cba0250-7e78-496d-8559-98c9c9a3d3e3/download/opendata_inc9317_region.csv

# National Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/72c852b8-ee28-4fd8-84a9-5f415f4bc325/download/opendata_inc9216_scotland.csv

# HB lookup
# https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/geography_codes_and_labels_hb2014_01042019.csv


#### SETUP ####

library(tidyverse)
library(plotly)
source("ISD_UTILS.R")

HBs <- read_csv("data/geography_codes_and_labels_hb2014_01042019.csv")

df_board <- read_csv("data/opendata_inc9317_hb2018.csv") %>%
  inner_join(HBs %>% select(HB2014, HB2014Name), by = c("HBR2014" = "HB2014")) %>%
  select(-SexQF, -HBR2014) # drop redundant cols

df_network <- read_csv("data/opendata_inc9317_region.csv") %>%
  select(-SexQF) # drop redundant cols

df_national <- read_csv("data/opendata_inc9216_scotland.csv") %>%
  mutate(Country = "NHS Scotland") %>%
  select(-SexQF) # drop redundant cols


# all cancer incidence over time, for Borders, network and Scotland

board <- df_board %>% 
  filter(CancerSite == "All cancer types",
         HB2014Name == "NHS Borders",
         Sex == "All") %>%
  select(HB2014Name, Year, IncidencesAllAges) %>%
  rename(location = HB2014Name)

network <- df_network %>%
  filter(CancerSite == "All cancer types",
         Region == "South East of Scotland",
         Sex == "All") %>%
  select(Region, Year, IncidencesAllAges) %>%
  rename(location = Region)
    
national <- df_national %>% 
  filter(CancerSite == "All cancer types",
         Country == "NHS Scotland",
         Sex == "All") %>%
  select(Country, Year, IncidencesAllAges) %>%
  rename(location = Country)


board %>% bind_rows(network) %>% bind_rows(national) %>% 
  ggplot(aes(x = Year, y = IncidencesAllAges, colour = location)) +
  geom_point() + 
  geom_line()


# board, network, nation crude rate w' 95% CI

board <- df_board %>% 
  filter(CancerSite == "All cancer types",
         HB2014Name == "NHS Borders",
         Sex == "All") %>%
  select(HB2014Name, Year, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval) %>%
  rename(location = HB2014Name)

network <- df_network %>%
  filter(CancerSite == "All cancer types",
         Region == "South East of Scotland",
         Sex == "All") %>%
  select(Region, Year, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval) %>%
  rename(location = Region)

national <- df_national %>% 
  filter(CancerSite == "All cancer types",
         Country == "NHS Scotland",
         Sex == "All") %>%
  select(Country, Year, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval) %>%
  rename(location = Country)


board %>% bind_rows(network) %>% bind_rows(national) %>% 
  ggplot(aes(x = Year, y = CrudeRate, colour = location)) + 
  geom_point() +
  geom_line() + 
  geom_ribbon(aes(ymin = CrudeRateLower95pcConfidenceInterval, 
                  ymax = CrudeRateUpper95pcConfidenceInterval, 
                  fill = location), alpha = .3, color = NA) +
  theme_bw()


# SCAN boards crude rate w' 95% CI

df_board %>% 
  filter(CancerSite == "All cancer types",
         HB2014Name %in% c("NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Lothian"),
         Sex == "All") %>%
  select(HB2014Name, Year, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval) %>%
  rename(location = HB2014Name) %>% 
  ggplot(aes(x = Year, y = CrudeRate, colour = location)) + 
  geom_point() +
  geom_line() + 
  geom_ribbon(aes(ymin = CrudeRateLower95pcConfidenceInterval, 
                  ymax = CrudeRateUpper95pcConfidenceInterval, 
                  fill = location), alpha = .3, color = NA) +
  theme_bw()
