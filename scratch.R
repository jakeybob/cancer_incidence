# incidence data
# https://www.opendata.nhs.scot/dataset/annual-cancer-incidence
# https://www.opendata.nhs.scot/dataset/c2c59eb1-3aff-48d2-9e9c-60ca8605431d/resource/3aef16b7-8af6-4ce0-a90b-8a29d6870014/download/opendata_inc9317_hb2018.csv

# help inform for provision of cancer treatment services in NHS Borders, would like to gain better
# understanding of the incidence of cancer in NHS Borders

library(tidyverse)
source("ISD_UTILS.R")

df <- read_csv("data/opendata_inc9317_hb2018.csv") %>%
  recode_health_boards(hbcols = "HBR2014", from = "code", to = "name")
