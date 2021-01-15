################################################
# Open Streetmap Raw Data Acquisition ##########
################################################


##########
## Setup #
##########
### Packages

library(tidyverse)
library(sf)
library(osmdata)
library(tmap)
# library(RANN)

### Helper Functions
source("R/helper.R")

### Bounding Box for Berlin
bbox_berlin <- c(13.03, 52.34, 13.76, 52.7)

### Districts
districts_raw <- opq(bbox_berlin) %>%
  add_osm_feature("boundary", "administrative") %>%
  add_osm_feature("admin_level", c(10)) %>%
  osmdata_sf() %>% 
  encode_osm() 
save(districts_raw, file = "data/districts.Rdat")

### Restaurants
restaurants_raw <- opq(bbox = bbox_berlin) %>%
  add_osm_feature("amenity", "restaurant") %>%
  osmdata_sf() %>% 
  encode_osm()
restaurants_raw <- restaurants
save(restaurants_raw, file = "data/restaurants.Rdat")

### Transport
stops_raw <- opq(bbox = bbox_berlin) %>%
  add_osm_feature(key = 'public_transport', value = 'stop_position') %>%
  osmdata_sf() %>% 
  encode_osm()  
save(stops_raw, file = "data/stops.Rdat")

### Parks
parks_raw <- opq(bbox = bbox_berlin) %>%
  add_osm_feature(key = 'leisure', value = 'park') %>%
  osmdata_sf() %>% 
  encode_osm()
save(parks_raw, file = "data/parks.Rdat")

### Forests
forest_raw <- opq(bbox = bbox_berlin) %>%
  add_osm_feature(key = 'landuse', value = c('forest')) %>%
  osmdata_sf() %>% 
  encode_osm() 
save(forest_raw, file = "data/forest.Rdat")

### Tourist attraction
attraction <- opq(bbox = bbox_berlin) %>%
  add_osm_feature(key = 'tourism', value = 'attraction') %>%
  osmdata_sf() %>% 
  encode_osm() 
attraction_raw <- attraction
save(attraction_raw, file = "data/attraction.Rdat")