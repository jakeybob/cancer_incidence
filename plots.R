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

dat <- df_board %>% filter(Sex == "All",
                           CancerSite == "All cancer types",
                           HB2014Name == "NHS Borders")
p <- dat %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + 
  geom_line() +
  theme_bw()

p2 <- df_board %>% filter(Sex == "All",
                          CancerSite == "All cancer types",
                          HB2014Name == "NHS Borders") %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + 
  geom_line() +
  theme_bw()
p2

#### 2. outliers ####

dat <- df_board %>% filter(CancerSite != "All cancer types",
                    # Year > 2007,
                    Sex == "All")

dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  ggplot(aes(x = Year, y = EASR)) +
  geom_point() + geom_line() +
  facet_wrap(vars(CancerSite), scales="free_y")

p <- dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  filter(CancerSite %in% 
           c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  # ggplot(aes(x = Year, y = StandardisedIncidenceRatio)) +
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + geom_line() +
  # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
  facet_wrap(vars(CancerSite), scales="free_y")
p

p2 <- dat %>%
  filter(HB2014Name %in% c("NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Lothian")) %>%
  filter(CancerSite %in% 
           c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = CrudeRate, colour=HB2014Name)) +
  geom_point() + geom_line() +
  # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
  facet_wrap(vars(CancerSite), scales="free_y")
p2  

p3 <- df_network %>%
  filter(CancerSite != "All cancer types",
         CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"),
         Sex == "All") %>%
  ggplot(aes(x = Year, y = EASR, colour=Region)) +
  geom_point() + geom_line() +
  # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
  facet_wrap(vars(CancerSite), scales="free_y")
p3

dat <- df_national %>% filter(CancerSite == "All cancer types",
                              Year == 2017,
                              Sex == "All") %>%
  select(starts_with("Incidences")) %>% select(-IncidencesAllAges)

tibble(age_group = names(dat), incidence = t(dat)[,1]) %>%
  ggplot() +
  geom_col(aes(x = age_group, y = incidence, fill="blue"), alpha = .8, width = 1)
