# GLOBAL

# Packages ----
library(dplyr)
library(leaflet)
library(shiny)
library(RColorBrewer)
library(scales)
library(lattice)

# Data ----

df02 <- read.csv("data/df1_QC_2022_06_16_115936.csv")

cleantable02 <- df02 %>%
  select(.
         , site = MonitoringLocation
         , CBP = boo.CBP144
         , Num_Events = Num_Events
         , Year_min = year_min
         , Year_max = year_max
         , SampleDepth_90th = maxSampleDepth_90
         , n_90th = n_DO_90
         , Latitude = Latitude
         , Longitude = Longitude) %>%
  arrange(., desc(CBP), site)

siteData02 <- df02

