##########################################
## created: 8/28/2026  
## last modified: 10/14/2026
## author: tyler mcdaniel
## purpose: create EPA pollution data
##########################################



library(ncdf4)
library(sf)
library(tidycensus)
library(magrittr)
library(readr)
library(dplyr)
library(purrr)
library(readxl)
library(dataRetrieval)
library(jsonlite)
library(arcgislayers)
library(haven)
library(tidygeocoder)
library(janitor)
library(stringr)


setwd("/Users/tylermcdaniel/Library/CloudStorage/GoogleDrive-tylermc@stanford.edu/My Drive/ARPA-H/")


################################################################################
## brownfields


# brownfields
brownfields1 <- read_csv("Test_data_files/EPA/CIMC Basic Search Result.csv")

brownfields2 <- read_csv("Test_data_files/EPA/CIMC_Brownfields_Download.csv")

# Using sub() to remove everything before the ID
brownfields1 %<>%
  mutate(Brownfield_ID = sub(".*P31_ID:", "", `Brownfields Link CSV`),
         Registry_ID = sub(".*p_registry_id=", "", `FRS Link CSV`))

# now set IDs for geos
brownfields2 %<>%
  mutate(Brownfield_ID = as.character(`Property ID`))

# Remove duplicates in site file based on Brownfield ID
brownfields1 %<>%
  distinct(Brownfield_ID, .keep_all = TRUE)

# Remove duplicates in geo file based on Brownfield ID
brownfields2 %<>%
  distinct(Brownfield_ID, .keep_all = TRUE)

# Join site and geo files
brownfields <- left_join(brownfields1, brownfields2, by = c("Brownfield_ID"))
#write_csv(brownfields, "Test_data_files/EPA/brownfields.csv")


# create clean version
brownfields_clean <- brownfields %>%
  clean_names()

# fix character issue
# Replace 'df' with your dataframe name
# clean up
brownfields_clean[] <- lapply(brownfields_clean, function(x) {
  if (is.character(x)) iconv(x, from = "UTF-8", to = "UTF-8", sub = "") else x
})


# for brownfield sites missing lat/long, impute values
brownfields_clean %<>%
  mutate(address_full = paste(location_address, city_name, state_code, postal_code)) 


brownfields_lat_long_imp <- brownfields_clean %>%
  filter(is.na(latitude) | is.na(longitude)) %>%
  select(epa_id, brownfields_link_csv, address_full) %>%
  geocode(address = address_full, method = "osm", lat = latitude_imp, long = longitude_imp)

# currently 15 missing

# merge back with brownfields
brownfields_clean %<>%
  left_join(brownfields_lat_long_imp) %>%
  mutate(latitude = recode_values(latitude, 
                               NA ~ latitude_imp, 
                               default = latitude),
         longitude = recode_values(longitude,
                                NA ~ longitude_imp,
                                default = longitude))


# drop 15 obs with no lat/long
brownfields_clean %<>%
  filter(!is.na(latitude) & !is.na(longitude))

# make spatial
#brownfields_clean %<>%
#  st_as_sf(coords = c("longitude", "latitude"))

# save
write_csv(brownfields_clean, "Test_data_files/EPA/brownfields_clean.csv")
brownfields_clean <- read_csv("Test_data_files/EPA/brownfields_clean.csv") %>% st_as_sf(coords = c("longitude", "latitude"))

################################################################################
## superfund sites

# Superfund sites 
superfund <- read_csv("Test_data_files/School_hazards/2wxpv-osfstorage-archive/Data/Pollution Sites/Superfund/superfund_sites_with_coordinates_new.csv")

#url <- "https://services.arcgis.com/cJ9YHowT8TU7DUyn/ArcGIS/rest/services/FAC_Superfund_Site_Boundaries_EPA_Public/FeatureServer/0"
#layer <- arc_open(url)
#npl_boundaries <- arc_select(layer)  # pulls all records, paginating internally

# write as geojson file
#st_write(npl_boundaries, "Test_data_files/EPA/npl_boundaries.geojson")

npl_boundaries <- st_read("Test_data_files/EPA/npl_boundaries.geojson")


# create clean version
superfund_clean <- superfund %>%
  clean_names()

# remove missing site ids
superfund_clean %<>%
  filter(!is.na(epa_id))

# flag lat/long values less than 4 decimals (not very precise)
superfund_clean %<>%
  mutate(imprecise_flag = case_when(nchar(latitude) < 7 ~ 1, 
                                    .default = 0))


# for superfund sites missing lat/long, impute values
superfund_clean %<>%
  mutate(address_full = paste(street_address, city, state, zip)) 


superfund_lat_long_imp <- superfund_clean %>%
  filter(is.na(latitude) | is.na(longitude) | imprecise_flag == 1) %>%
  select(epa_id, address_full) %>%
  geocode(address = address_full,
          method = "osm", lat = latitude_imp, long = longitude_imp)

# currently, 628 coded, 1078 not coded


# merge back with superfund
superfund_clean %<>%
  left_join(superfund_lat_long_imp) %>%
  mutate(latitude = case_when(is.na(latitude) | imprecise_flag == 1 ~ latitude_imp, 
                              .default = as.numeric(latitude)),
         longitude = case_when(is.na(longitude) | imprecise_flag == 1 ~ longitude_imp,
                               .default = as.numeric(longitude)))

# drop 1078 obs with no lat/long
superfund_clean %<>%
  filter(!is.na(latitude) & !is.na(longitude))

# make spatial
#superfund_clean %<>%
#  st_as_sf(coords = c("longitude", "latitude"))

# save
write_csv(superfund_clean, "Test_data_files/EPA/superfund_clean.csv")
superfund_clean <- read_csv("Test_data_files/EPA/superfund_clean.csv") 


################################################################################
## tris


# TRI sites
tri <- read_csv("Test_data_files/School_hazards/2wxpv-osfstorage-archive/Data/Pollution Sites/TRI/2024_us.csv")

# clean up names
tri_clean <- tri %>%
  rename_with(~ str_sub(., 3)) %>%
  clean_names()

# make spatial
#tri_clean %<>%
#  st_as_sf(coords = c("longitude", "latitude"))

# save
write_csv(tri_clean, "Test_data_files/EPA/tri_clean.csv")
tri_clean <- read_csv("Test_data_files/EPA/tri_clean.csv")


################################################################################
## nei



### 2023


## point level 

# download files (had to download by region because total was too large)

nei2023_12 <- read_xlsx("Test_data_files/EPA/NEI/58102b9a-b39b-4295-983a-9037122abbfb.xlsx")

nei2023_34 <- read_xlsx("Test_data_files/EPA/NEI/0179b776-1472-489b-8630-bdb64207330d.xlsx")

nei2023_56 <- read_xlsx("Test_data_files/EPA/NEI/a74904d6-0ed7-483a-a536-5425b891710b.xlsx")

nei2023_78 <- read_xlsx("Test_data_files/EPA/NEI/3674c006-6d39-4ed5-beef-83bd44d8b6a3.xlsx")

nei2023_910 <- read_xlsx("Test_data_files/EPA/NEI/f490b052-13c4-4f0c-ba75-1e850f3aa9af.xlsx")


# bind rows
nei2023_total <- bind_rows(nei2023_12,
                       nei2023_34,
                       nei2023_56,
                       nei2023_78,
                       nei2023_910)



### 2020

nei2020_county <- read_xlsx("Test_data_files/EPA/NEI/e019c47c-f60f-4c30-beb7-d7f6c276180b.xlsx")

