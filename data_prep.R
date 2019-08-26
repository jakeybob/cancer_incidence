# data_prep.R

library(tidyverse)

#### INCIDENCE DATA ####
# https://www.opendata.nhs.scot/dataset/annual-cancer-incidence
#
# Board Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/3aef16b7-8af6-4ce0-a90b-8a29d6870014/download/opendata_inc9317_hb2018.csv
#
# Cancer Network Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/8cba0250-7e78-496d-8559-98c9c9a3d3e3/download/opendata_inc9317_region.csv
#
# National Level
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/72c852b8-ee28-4fd8-84a9-5f415f4bc325/download/opendata_inc9216_scotland.csv
#
# HB lookup
# https://www.opendata.nhs.scot/dataset/9f942fdb-e59e-44f5-b534-d6e17229cc7b/resource/652ff726-e676-4a20-abda-435b98dd7bdc/download/geography_codes_and_labels_hb2014_01042019.csv

# Other
# https://www.nrscotland.gov.uk/files/statistics/age-standardised-death-rates-esp/age-standard-death-rates-background.pdf

HBs <- read_csv("data/geography_codes_and_labels_hb2014_01042019.csv")

df_board <- read_csv("data/opendata_inc9317_hb2018.csv") %>%
  inner_join(HBs %>% select(HB2014, HB2014Name), by = c("HBR2014" = "HB2014")) %>%
  select(-SexQF, -HBR2014) # drop redundant cols

df_network <- read_csv("data/opendata_inc9317_region.csv") %>%
  select(-SexQF) # drop redundant cols

df_national <- read_csv("data/opendata_inc9216_scotland.csv") %>%
  mutate(Country = "Scotland") %>% 
  rename(Region = Country) %>%
  select(-SexQF) # drop redundant cols

df_nat_net <- full_join(df_network, df_national)

# C44: Other malignant neoplasms of skin



names(df_national)[which(!(names(df_national) %in% names(df_network)))] # cols not in national: StandardisedIncidenceRatioQF

# cols_not_in(df_nat_net, df_national) :  "SIRLower95pcConfidenceInterval" "SIRUpper95pcConfidenceInterval"



#### POPULATION DATA ####
# NHS board and national age projections (NRS) 
# https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-projections/sub-national-population-projections/2016-based/detailed-tables
# https://www.nrscotland.gov.uk/files//statistics/population-estimates/time-series/mid-18/mid-year-pop-est-18-time-series-4.csv

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

write_rds(df_borders_pop, "data/borders_pop.rds")

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

write_rds(df_scot_pop, "data/scot_pop.rds")

# national estimates

df_scot_pop_estimates <- read_csv("data/mid-year-pop-est-18-time-series-4.csv")[1:18,1:40] %>%
  filter(is.na(`X2`) == FALSE)
names(df_scot_pop_estimates) <- as.character(df_scot_pop_estimates[1, ])
df_scot_pop_estimates <- df_scot_pop_estimates[2:dim(df_scot_pop_estimates)[1], ]

write_rds(df_scot_pop_estimates, "data/scot_pop_estimates.rds")
