# plots for 1 page summary on NHS Borders cancer incidence

library(tidyverse)
library(gridExtra)

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
  ylab("Total Incidence") + ggtitle("Total Incidence") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))

p2 <- df_board %>% filter(Sex == "All",
                          CancerSite == "All cancer types",
                          HB2014Name == "NHS Borders") %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + 
  geom_line() +
  ylab("Total Incidence Rate per 100,000") + ggtitle("Total Incidence Rate") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))


p_incidence_trends <- grid.arrange(p, p2, ncol = 2)
ggplotify::as.ggplot(p_incidence_trends)

write_rds(ggplotify::as.ggplot(p_incidence_trends), "data/p_incidence_trends.rds")

#### 2. outliers ####

dat <- df_board %>% filter(CancerSite != "All cancer types",
                    # Year > 2007,
                    Sex == "All")

dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  ggplot(aes(x = Year, y = EASR)) +
  geom_point() + geom_line() +
  facet_wrap(vars(CancerSite), scales="free_y")

p_skin <- dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  filter(CancerSite %in% 
           c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  # ggplot(aes(x = Year, y = StandardisedIncidenceRatio)) +
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + geom_line() +
  # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
  facet_wrap(vars(CancerSite), scales="free_y", labeller=label_wrap_gen(width = 20, multi_line = TRUE)) + 
  ggtitle("Incidence of Selected Cancers in NHS Borders") + ylab("Incidence") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face="bold"))
p_skin
write_rds(p_skin, "data/p_skin.rds")

# p2 <- dat %>%
#   filter(HB2014Name %in% c("NHS Borders", "NHS Dumfries and Galloway", "NHS Fife", "NHS Lothian")) %>%
#   filter(CancerSite %in% 
#            c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
#   ggplot(aes(x = Year, y = CrudeRate, colour=HB2014Name)) +
#   geom_point() + geom_line() +
#   # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
#   facet_wrap(vars(CancerSite), scales="free_y")
# p2  

p_skin_network <- df_network %>%
  filter(CancerSite != "All cancer types",
         CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"),
         Sex == "All") %>%
  ggplot(aes(x = Year, y = EASR, colour=Region)) +
  geom_point() + geom_line() +
  scale_colour_manual(values=c("#1b9e77", "#d95f02", "#7570b3")) + 
  # geom_ribbon(aes(ymin = SIRLower95pcConfidenceInterval, ymax = SIRUpper95pcConfidenceInterval), alpha=.3) + 
  facet_wrap(vars(CancerSite), scales="free_y", labeller=label_wrap_gen(width = 20, multi_line = TRUE)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "grey95"),
                strip.text = element_text(face = "bold"),
        legend.position = "bottom",
        plot.title = element_text(face="bold")) +
  labs(title = "Incidence Rate of Selected Cancers by Network", color = "")
p_skin_network
write_rds(p_skin_network, "data/p_skin_network.rds")



dat <- df_national %>% filter(CancerSite == "All cancer types",
                              Year == 2017,
                              Sex == "All") %>%
  select(starts_with("Incidences")) %>% select(-IncidencesAllAges)

age_labels <- paste(seq(0, 90, by=5), "-", seq(0, 90, by=5)+4, sep="")
age_labels[1] <- "00-04"
age_labels[2] <- "05-09"
age_labels[19] <- "90+"

dat2 <- tibble(age_group = age_labels, incidence = t(dat)[,1]) 

p_scot_incidence_demo <- dat2 %>%
  ggplot() +
  geom_col(aes(x = age_group, y = incidence), alpha = 1, width = 1, fill="grey20") +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Age Group", y = "Incidence", title = "Scottish Cancer Incidence (2017) by Age")
p_scot_incidence_demo
write_rds(p_scot_incidence_demo, "data/p_scot_incidence_demo.rds")

# sum(dat2$incidence) # 32234
# sum(dat2$incidence[1:14]) # 15950
# sum(dat2$incidence[15:19]) # 16284


# Borders pop dist
ages <- read_rds("data/borders_pop.rds") %>%
  select(-Age, -gender, -age_numeric, -age_group_10yr) %>%
  gather(key="year", value="pop", -age_group_5yr) %>%
  group_by(year, age_group_5yr) %>%
  summarise(pop = sum(pop))

# do as flat file histogram with bin width 5

p_borders_demo <- ages %>%
  filter(year %in% c(2019, 2039)) %>%
  ggplot() +
  geom_col(aes(x = age_group_5yr, y = pop, fill=year, alpha=year), width = 1, position = "identity") + 
  scale_colour_manual(values = c("grey0", "grey50"), aesthetics = "fill") +
  scale_alpha_manual(values = c(.8, .6), guide=F) +
  # geom_col(data = ages %>% filter(year==2019), aes(x = age_group_5yr, y = pop), alpha = .8, width = 1, fill="black") + 
  # geom_col(data = ages %>% filter(year==2039), aes(x = age_group_5yr, y = pop), alpha = .3, width = 1, fill="black") + 
  theme_bw() +
  theme(legend.position = "right",
        plot.title = element_text(face="bold"),
        axis.text.x = element_text(angle = 90)) +
  labs(x = "Age Group", y = "Population", title = "NHS Borders Age Distribution", fill="")
p_borders_demo
write_rds(p_borders_demo, "data/p_borders_demo.rds")


p_both_demos <- grid.arrange(p_scot_incidence_demo, p_borders_demo, ncol = 2)
write_rds(ggplotify::as.ggplot(p_both_demos), "data/p_both_demos.rds")


ages <- read_rds("data/borders_pop.rds") %>%
  select(Age, `2019`, gender) %>%
  group_by(Age) %>%
  summarise(pop = sum(`2019`)) %>%
  arrange(as.numeric(Age)) 

sum(ages$pop) # 115188 ... 57594
sum(ages$pop[1:49]) # 57514 so median age is 48

pop_over70 <- sum(ages$pop[71:91]) # 20536
  

ages <- read_rds("data/borders_pop.rds") %>%
  select(Age, `2039`, gender) %>%
  group_by(Age) %>%
  summarise(pop = sum(`2039`)) %>%
  arrange(as.numeric(Age)) 

sum(ages$pop) #  117426 ... 58713
sum(ages$pop[1:51]) # 57514 so median age is 50

pop_over70 <- sum(ages$pop[71:91])  # 29722

