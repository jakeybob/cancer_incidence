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


#### more plots etc ####

# 1. all incidence and rates  ####

dat <- df_board %>% filter(Sex == "All",
                           CancerSite == "All cancer types",
                           HB2014Name == "NHS Borders")

# scot_pop_est <- read_rds("data/scot_pop_estimates.rds") %>%
#   filter(Persons == "Scotland")
# scot_pop_est <- scot_pop_est[, 3:dim(scot_pop_est)[2]]
# scot_pop_est <- as_tibble(cbind(nms = names(scot_pop_est), t(scot_pop_est)))
# names(scot_pop_est) <- c("Year", "Population")
# scot_pop_est <- scot_pop_est %>% mutate_all(as.numeric)

dat2 <- df_national %>% 
  filter(Sex == "All",
         CancerSite == "All cancer types") %>%
  select(Year, IncidencesAllAges, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval)
# rename(CrudeRate_o = CrudeRate) %>%
# inner_join(scot_pop_est) %>%
# mutate(CrudeRate = 1e5*IncidencesAllAges / Population)

# 2017 comparisons
borders_2017_crudeRate <- (dat %>% filter(Year == 2017))$CrudeRate
scot_2017_crudeRate <- (dat2 %>% filter(Year == 2017))$CrudeRate
ratio <- borders_2017_crudeRate / scot_2017_crudeRate


fit_incidence <- lm(IncidencesAllAges ~ Year, data = dat)
p <- dat %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = "lm") +
  annotate("text", x=2000, y=850, label=paste0("y ~",  sprintf(fit_incidence$coefficients["Year"], fmt="%.2f"), " x")) +
  annotate("text", x=2010, y=600, label=paste0("2017 value\n= ", (dat %>% filter(Year == 2017))$IncidencesAllAges)) +
  ylab("Total Incidence") + ggtitle("Total Incidence (Borders)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))


fit_incidence_rate <- lm(CrudeRate ~ Year, data = dat)
p2 <- df_board %>% filter(Sex == "All",
                          CancerSite == "All cancer types",
                          HB2014Name == "NHS Borders") %>%
  select(Year, IncidencesAllAges, CrudeRate, CrudeRateLower95pcConfidenceInterval, CrudeRateUpper95pcConfidenceInterval) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = CrudeRateLower95pcConfidenceInterval, 
                  ymax = CrudeRateUpper95pcConfidenceInterval), alpha = .1, color = NA) +
  geom_smooth(method = "lm") +
  annotate("text", x=2000, y=750, label=paste0("y ~",  sprintf(fit_incidence_rate$coefficients["Year"], fmt="%.2f"), " x")) +
  annotate("text", x=2010, y=515, label = paste0("2017 value\n=", sprintf(borders_2017_crudeRate, fmt="%.2f"), 
                                                 "\n\nratio w.r.t Scotland\n=", sprintf(ratio, fmt="%.2f"))) + 
  ylab("Total Incidence Rate per 100,000") + ggtitle("Total Incidence Rate (Borders)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))


fit_incidence_nat <- lm(IncidencesAllAges ~ Year, data = dat2)
p3 <- dat2 %>%
  select(Year, IncidencesAllAges, CrudeRate) %>%
  ggplot(aes(x = Year, y = IncidencesAllAges)) +
  geom_point() + 
  geom_line() +
  geom_smooth(method = "lm") +
  annotate("text", x=2000, y=32000, label=paste0("y ~",  sprintf(fit_incidence_nat$coefficients["Year"], fmt="%.2f"), " x")) +
  annotate("text", x=2010, y=27000, label=paste0("2017 value\n= ", (dat2 %>% filter(Year == 2017))$IncidencesAllAges)) + 
  ylab("Total Incidence") + ggtitle("Total Incidence (Scotland)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))


fit_incidence_rate_nat <- lm(CrudeRate ~ Year, data = dat2)
p4 <- dat2 %>%
ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + 
  geom_line() +
  geom_ribbon(aes(ymin = CrudeRateLower95pcConfidenceInterval, 
                  ymax = CrudeRateUpper95pcConfidenceInterval), alpha = .1, color = NA) +
  geom_smooth(method = "lm") +
  annotate("text", x=2000, y=600, label=paste0("y ~",  sprintf(fit_incidence_rate_nat$coefficients["Year"], fmt="%.2f"), " x")) +
  annotate("text", x=2010, y=525, label=paste0("2017 value\n= ", sprintf((dat2 %>% filter(Year == 2017))$CrudeRate, fmt="%.2f"))) +
  ylab("Total Incidence Rate per 100,000") + ggtitle("Total Incidence Rate (Scotland)") +
  theme_bw() +
  theme(plot.title = element_text(face="bold"))
p4

ggplotify::as.ggplot(grid.arrange(p, p2, p3, p4, ncol = 2, nrow = 2))
ggsave("pics/all_incidence_and_rate.png", width = 8, height = 8, dpi=300)


#### 2. small multiples ####

dat <- df_board %>% filter(CancerSite != "All cancer types",
                           # Year > 2007,
                           Sex == "All")

# relative coefficients
df <- dat %>%
  filter(HB2014Name == "NHS Borders") %>%
  select(CancerSite, Year, IncidencesAllAges, CrudeRate, EASR, StandardisedIncidenceRatio) 

absolute_5yr_av_incidence <- df %>%
  filter(Year > 2012) %>%
  select(CancerSite, Year, IncidencesAllAges) %>%
  rename(cancers = CancerSite) %>%
  group_by(cancers) %>%
  summarise(incidence_5yr_av = mean(IncidencesAllAges, na.rm=T))

cancers <- unique(df$CancerSite)
coeffs_crude <- vector(length=length(cancers))
coeffs_EASR <- coeffs_crude
coeffs_SI <- coeffs_crude

for(i in seq_along(cancers)){
  coeffs_crude[i] <- lm(CrudeRate ~ Year, data = filter(df, CancerSite == cancers[i]))$coefficients["Year"]
  coeffs_EASR[i] <- lm(EASR ~ Year, data = filter(df, CancerSite == cancers[i]))$coefficients["Year"]
  coeffs_SI[i] <- lm(StandardisedIncidenceRatio ~ Year, data = filter(df, CancerSite == cancers[i]))$coefficients["Year"]
}

cancer_trends <- tibble(cancers = cancers,
                        coeffs_crude = coeffs_crude,
                        coeffs_EASR = coeffs_EASR,
                        coeffs_SI = coeffs_SI) %>%
  arrange(-coeffs_crude) %>%
  inner_join(absolute_5yr_av_incidence) %>%
  mutate_if(is.numeric, ~round(., digits=2))
# above shows coeffs of crude rate fit, EASR fit and SI fit
write_csv(cancer_trends, "data/cancer_trends.csv")


# Crude Rate
dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "free_y",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites Crude Rate") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face="bold"))

ggsave("pics/small_multiples_borders_crude_1.png", dpi=300, width=20, height=10, units = "in")

dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "fixed",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites Crude Rate") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("pics/small_multiples_borders_crude_2.png", dpi=300, width=20, height=10, units = "in")


# EASR
dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = EASR)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "free_y",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites EASR") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face="bold"))

ggsave("pics/small_multiples_borders_EASR_1.png", dpi=300, width=20, height=10, units = "in")

dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = EASR)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "fixed",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites EASR") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("pics/small_multiples_borders_EASR_2.png", dpi=300, width=20, height=10, units = "in")

# SI ratio
dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = StandardisedIncidenceRatio)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "free_y",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites SI Ratio") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("pics/small_multiples_borders_SI_1.png", dpi=300, width=20, height=10, units = "in")

dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  mutate(flag = if_else(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin"), T, F)) %>%
  # filter(CancerSite %in% c("Non-melanoma skin cancer", "Squamous cell carcinoma of the skin", "Basal cell carcinoma of the skin")) %>%
  ggplot(aes(x = Year, y = StandardisedIncidenceRatio)) +
  # geom_point() + 
  geom_line() +
  geom_smooth(method="lm", size=.5, aes(colour = flag)) +
  scale_colour_manual(values=c("black", "red")) +
  facet_wrap(vars(CancerSite),
             scales = "fixed",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  labs(x = "", title = "NHS Borders All Sites SI Ratio") +
  theme_bw() + 
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("pics/small_multiples_borders_SI_2.png", dpi=300, width=20, height=10, units = "in")


# 3. breast ####

dat <- df_board %>%
  filter(CancerSite %in% c("Breast", "Carcinoma in situ of the breast"),
         Sex == "All")

dat %>%
  filter(CancerSite == "Breast") %>%
  # filter(CancerSite == "Carcinoma in situ of the breast") %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_line() + geom_point() +
  facet_wrap(vars(HB2014Name),
             scales = "free_y",
             labeller=label_wrap_gen(width = 30, multi_line = TRUE)) +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face = "bold"))

ggsave("pics/small_multiples_breast.png", dpi=300, width=20, height=14, units = "in")


# 4. ageing and skin ####

dat <- df_network %>% filter(CancerSite != "All cancer types",
                           Sex == "All") %>%
  filter(Region == "South East of Scotland") %>%
  filter(CancerSite %in% c("Non-melanoma skin cancer",
                           "Squamous cell carcinoma of the skin",
                           "Basal cell carcinoma of the skin")) %>%
  filter(CancerSite %in% c("Non-melanoma skin cancer")) %>%
  filter(Year %in% c(2017, 2007)) %>%
  select(Year, CancerSite, contains("IncidenceRate")) %>%
  gather(key="age_group", value = "rate", -Year, -CancerSite) %>%
  mutate(age_group = str_remove(age_group, "IncidenceRateAge")) %>%
  mutate(age_group = case_when(age_group == "Under5" ~ "00-04",
                                age_group == "5To9" ~ "05-09",
                                age_group == "10To14" ~ "10-14",
                                age_group == "15To19" ~ "15-19",
                                age_group == "20To24" ~ "20-24",
                                age_group == "25To29" ~ "25-29",
                                age_group == "30To34" ~ "30-24",
                                age_group == "35To39" ~ "35-39",
                                age_group == "40To44" ~ "40-44",
                                age_group == "45To49" ~ "45-49",
                                age_group == "50To54" ~ "50-54",
                                age_group == "55To59" ~ "55-59",
                                age_group == "60To64" ~ "60-64",
                                age_group == "65To69" ~ "65-69",
                                age_group == "70To74" ~ "70-74",
                                age_group == "75To79" ~ "75-79",
                                age_group == "80To84" ~ "80-84",
                                age_group == "85To89" ~ "85-89",
                                age_group == "90AndOver" ~ "90+",
                                TRUE ~ "NA")) 

# distribution of crude rate with age group for C44 cancer in SE Scot
p <- dat %>%  
  filter(Year == 2017) %>%
  mutate(Year = as.factor(Year)) %>%
  ggplot(aes(x = age_group, y = rate)) + 
  geom_col(width=1, position="identity", alpha=.7) +
  theme_bw() +
  labs(title = "SE Scotland | C44 Incidence by Age | 2017", y = "crude rate") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# ggsave("pics/C44_SE_Scotland_2017.png", dpi=300, width=8, height=8, units = "in")

  

dat_incidence_SEscot_2017 <- dat %>% 
  filter(Year == 2017) %>%
  select(-Year, -CancerSite)

df_borders_pop <- read_rds("data/borders_pop.rds") %>%
  select(age_group_5yr, starts_with("20")) %>%
  group_by(age_group_5yr) %>%
  summarise_at(vars(starts_with("20")), sum) %>%
  rename(age_group = age_group_5yr) %>%
  inner_join(dat_incidence_SEscot_2017)

df_c44_projections <- df_borders_pop %>%
  mutate(rate = rate/1e5) %>%
  mutate_at(vars(starts_with("20")), .funs = list(~ . * rate ))

df_c44_projections_all_ages <- df_c44_projections %>%
  select( `2019`:`2039`) %>%
  summarise_all(sum) %>%
  gather(key = "Year", value="IncidenceAll") %>%
  mutate(`IncidenceAll` = round(`IncidenceAll`))

df_c44_projections_over70 <- df_c44_projections %>%
  filter(age_group %in% c("70-74", "75-79", "80-84", "85-89", "90+")) %>%
  select( `2019`:`2039`) %>%
  summarise_all(sum) %>%
  gather(key = "Year", value="Incidence70+") %>%
  mutate(`Incidence70+` = round(`Incidence70+`))

df_c44_projections_comp <- df_c44_projections_all_ages %>%
  full_join(df_c44_projections_over70) %>%
  mutate(over_70_ratio = 100*`Incidence70+` / IncidenceAll) %>%
  gather(key="incidence_type", value="Incidence", -Year) %>%
  mutate(incidence_type = as.factor(incidence_type))

p1 <- df_c44_projections_comp %>%
  filter(incidence_type != "over_70_ratio") %>%
  ggplot(aes(x = Year, y = Incidence, group=incidence_type)) + 
  geom_point(aes(colour=incidence_type)) +
  geom_line(aes(colour=incidence_type)) +
  theme_bw() +
  labs(title = "Projected C44 Incidence") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = "bottom")

p2 <- df_c44_projections_comp %>%
  filter(incidence_type == "over_70_ratio") %>%
  ggplot(aes(x = Year, y = Incidence, group=incidence_type)) +
  geom_point() + geom_line() +
  theme_bw() +
  labs(y = "over_70_ratio", title = "Projected Percentage of >70s") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p_C44_projections <- grid.arrange(p, p1, p2, ncol = 3)
ggplotify::as.ggplot(p_C44_projections)
ggsave("pics/C44_projections.png", dpi=300, width=15, height=8, units = "in")


# 5. skin/sex ####
dat <- df_board %>% filter(CancerSite != "All cancer types",
                           Sex != "All")

p_skin_sex <- dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  filter(CancerSite %in% c("Non-melanoma skin cancer")) %>%
  ggplot(aes(x = Year, y = EASR)) +
  geom_point() + geom_line() + 
  geom_ribbon(aes(ymin = EASRLower95pcConfidenceInterval, 
                  ymax = EASRUpper95pcConfidenceInterval), alpha = .1, color = NA) +
  facet_wrap(vars(Sex), scales="fixed", labeller=label_wrap_gen(width = 20, multi_line = TRUE)) + 
  ggtitle("NHS Borders | C44 | Sex") + ylab("EASR") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face="bold"))

p_skin_sex2 <- dat %>% 
  filter(HB2014Name == "NHS Borders") %>%
  filter(CancerSite %in% c("Non-melanoma skin cancer")) %>%
  ggplot(aes(x = Year, y = CrudeRate)) +
  geom_point() + geom_line() +
  geom_ribbon(aes(ymin = CrudeRateLower95pcConfidenceInterval, 
                  ymax = CrudeRateUpper95pcConfidenceInterval), alpha = .1, color = NA) +
  facet_wrap(vars(Sex), scales="fixed", labeller=label_wrap_gen(width = 20, multi_line = TRUE)) + 
  ggtitle("NHS Borders | C44 | Sex") + ylab("crude rate") +
  theme_bw() +
  theme(legend.position = "none",
        strip.background = element_rect(fill = "grey95"),
        strip.text = element_text(face = "bold"),
        plot.title = element_text(face="bold"))
p_skin_sex2

p_skin_sex_combined <- grid.arrange(p_skin_sex, p_skin_sex2, ncol = 2)
ggplotify::as.ggplot(p_skin_sex_combined)
ggsave("pics/p_skin_sex_combined.png", dpi=300, width=15, height=8, units = "in")
