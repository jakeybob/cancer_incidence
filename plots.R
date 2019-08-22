# plots for 1 page summary on NHS Borders cancer incidence

library(tidyverse)

#### SETUP ####
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

#### 1. current levels ####

p <- df_board %>% filter(Sex == "All",
                    CancerSite == "All cancer types",
                    HB2014Name == "NHS Borders",
                    Year > 207) %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + 
  geom_line() +
  theme_bw()

p2 <- df_board %>% filter(Sex == "All",
                          CancerSite == "All cancer types",
                          HB2014Name == "NHS Borders",
                          Year > 207) %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + 
  geom_line() +
  theme_bw()
p2
