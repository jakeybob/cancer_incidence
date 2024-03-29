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

# NHS board age projections (NRS) 
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections/2016-based/detailed-tables

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


# board population projections
df_borders_pop <- read_csv("data/population_board_NRS/Population-S08000016-Male.csv") %>%
  mutate(gender = "male") %>%
  bind_rows(read_csv("data/population_board_NRS/Population-S08000016-Female.csv") %>% mutate(gender = "female")) %>%
  filter(Age != "All Ages") %>%
  mutate(age_numeric = as.numeric(Age),
         age_numeric = if_else(is.na(age_numeric)==TRUE, 90, age_numeric)) %>%  # note that age_numeric = 90 is 90+
  mutate(age_group_10yr = case_when(age_numeric < 10 ~ "00-09",
                                    age_numeric < 20 ~ "10-19",
                                    age_numeric < 30 ~ "20-29",
                                    age_numeric < 30 ~ "20-29",
                                    age_numeric < 40 ~ "30-39",
                                    age_numeric < 50 ~ "40-49",
                                    age_numeric < 60 ~ "50-59",
                                    age_numeric < 70 ~ "60-69",
                                    age_numeric < 80 ~ "70-79",
                                    age_numeric < 90 ~ "80-89",
                                    TRUE ~ "90+"),
         age_group_5yr = case_when(age_numeric < 5 ~ "00-04",
                                    age_numeric < 10 ~ "05-09",
                                    age_numeric < 15 ~ "10-14",
                                    age_numeric < 20 ~ "15-19",
                                    age_numeric < 25 ~ "20-24",
                                    age_numeric < 30 ~ "25-29",
                                    age_numeric < 35 ~ "30-24",
                                    age_numeric < 40 ~ "35-39",
                                    age_numeric < 45 ~ "40-44",
                                    age_numeric < 50 ~ "45-49",
                                    age_numeric < 55 ~ "50-54",
                                    age_numeric < 60 ~ "55-59",
                                    age_numeric < 65 ~ "60-64",
                                    age_numeric < 70 ~ "65-69",
                                    age_numeric < 75 ~ "70-74",
                                    age_numeric < 80 ~ "75-79",
                                    age_numeric < 85 ~ "80-84",
                                    age_numeric < 90 ~ "85-89",
                                    TRUE ~ "90+"))

# national population projections
df_scot_pop <- read_csv("data/population_board_NRS/Population-S92000003-Male.csv") %>%
  mutate(gender = "male") %>%
  bind_rows(read_csv("data/population_board_NRS/Population-S92000003-Female.csv") %>% mutate(gender = "female")) %>%
  filter(Age != "All Ages") %>%
  mutate(age_numeric = as.numeric(Age),
         age_numeric = if_else(is.na(age_numeric)==TRUE, 90, age_numeric)) %>%  # note that age_numeric = 90 is 90+
  mutate(age_group_10yr = case_when(age_numeric < 10 ~ "00-09",
                                    age_numeric < 20 ~ "10-19",
                                    age_numeric < 30 ~ "20-29",
                                    age_numeric < 30 ~ "20-29",
                                    age_numeric < 40 ~ "30-39",
                                    age_numeric < 50 ~ "40-49",
                                    age_numeric < 60 ~ "50-59",
                                    age_numeric < 70 ~ "60-69",
                                    age_numeric < 80 ~ "70-79",
                                    age_numeric < 90 ~ "80-89",
                                    TRUE ~ "90+"),
         age_group_5yr = case_when(age_numeric < 5 ~ "00-04",
                                   age_numeric < 10 ~ "05-09",
                                   age_numeric < 15 ~ "10-14",
                                   age_numeric < 20 ~ "15-19",
                                   age_numeric < 25 ~ "20-24",
                                   age_numeric < 30 ~ "25-29",
                                   age_numeric < 35 ~ "30-24",
                                   age_numeric < 40 ~ "35-39",
                                   age_numeric < 45 ~ "40-44",
                                   age_numeric < 50 ~ "45-49",
                                   age_numeric < 55 ~ "50-54",
                                   age_numeric < 60 ~ "55-59",
                                   age_numeric < 65 ~ "60-64",
                                   age_numeric < 70 ~ "65-69",
                                   age_numeric < 75 ~ "70-74",
                                   age_numeric < 80 ~ "75-79",
                                   age_numeric < 85 ~ "80-84",
                                   age_numeric < 90 ~ "85-89",
                                   TRUE ~ "90+"))

write_rds(df_borders_pop, "data/borders_pop.rds")
write_rds(df_scot_pop, "data/scot_pop.rds")


# Borders pop dist
ages <- read_rds("data/borders_pop.rds") %>%
  select(-Age, -gender, -age_numeric, -age_group_10yr) %>%
  gather(key="year", value="pop", -age_group_5yr) %>%
  group_by(year, age_group_5yr) %>%
  summarise(pop = sum(pop))

# do as flat file histogram with bin width 5

ages %>%
  filter(year %in% c(2019, 2039)) %>%
  ggplot() +
  # geom_col(aes(x = age_group_5yr, y = pop, fill = year), alpha = .5, width = 1, position="identity") +
  geom_col(data = ages %>% filter(year==2019), aes(x = age_group_5yr, y = pop, fill="blue"), alpha = .8, width = 1) + 
  # geom_vline(data = ages %>% filter(year==2019), xintercept = stat(median)) +
  geom_col(data = ages %>% filter(year==2039), aes(x = age_group_5yr, y = pop, fill="red"), alpha = .3, width = 1) + 
  theme_bw()
