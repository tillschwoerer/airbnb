##########
## Setup #
##########
### Packages

library(tidyverse)
library(sf)
library(osmdata)
library(tmap)
library(RANN)

### Helper Functions
source("R/helper.R")

###############################
# AirBnb data #################
###############################
airbnb <- read_csv("orig/listings.csv")

# Pre-select columns
airbnb <- airbnb %>% 
  select(id, listing_url, listing_name = name, description, picture_url, amenities, 
         host_response_time, neighbourhood_cleansed, neighbourhood_group_cleansed, latitude, 
         longitude, property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, 
         price, number_of_reviews, review_scores_rating)

# Cast column type
airbnb <- airbnb %>% mutate(price = as.numeric(str_sub(price, 2))) %>% relocate(price, .after=2)

# Turn into simple features

# Amenities
airbnb <- airbnb %>% 
  mutate(wifi = str_detect(amenities, "Wifi"),
         tv = str_detect(amenities, "TV"),
         bed_linens = str_detect(amenities, "Bed linens"),
         parking = str_detect(amenities, "parking"),
         garden = str_detect(amenities, "Garden"),
         coffeemaker = str_detect(amenities, "Coffee maker"),
         pool = str_detect(amenities, "Pool"),
         bathtub = str_detect(amenities, "Bathtub"),
         cooking_basics = str_detect(amenities, "Cooking basics"),
         gym = str_detect(amenities, "Gym")) 

# Size
airbnb <- airbnb %>%
  mutate(altbau = str_detect(description, "Altbau")) %>%
  mutate(qm = str_extract(description, "\\d+(?= {0,1}qm)"),
         sqm = str_extract(description, "\\d+(?= {0,1}sqm)"),
         m2 = str_extract(description, "\\d+(?= {0,1}m2)"),
         square = str_extract(description, "\\d+(?= {0,1}square met)"),
         m_2 = str_extract(description, "\\d+(?= {0,1}m²)"),
         mq = str_extract(description, "\\d+(?= {0,1}mq)"),
         sq.m = str_extract(description, "\\d+(?= {0,1}sq.m)")
  ) %>%
  mutate(across(qm:sq.m, as.numeric))

airbnb <- airbnb %>% mutate(min = pmin(qm, sqm, m2, square, m_2, mq, sq.m, square, na.rm=TRUE),
                          max = pmax(qm, sqm, m2, square, m_2, mq, sq.m, square, na.rm=TRUE)) %>%
  mutate(size = ifelse(min==max & min>=10 & min <300, min, NA)) %>%
  select(-c(qm, sqm, m2, square, m_2, mq, sq.m, min, max))


### Generated variables
airbnb <- airbnb %>% 
  mutate(price_qm = price / size, 
         price_person = price / accommodates,
         price_bedrooms = price / bedrooms,
         size_person = size / accommodates)


### Summary Stats

skimr::skim(airbnb)

vars <- c("price", "accommodates", "beds", "price_person")
probs <-  c(0.01, 0.02, 0.03, 0.1, 0.5, 0.9, 0.97, 0.98, 0.99)
airbnb %>% summarise(across(vars, quantile, probs, na.rm=TRUE))


airbnb <- airbnb %>% 
  mutate(extreme = ifelse(price <= quantile(price, 0.02, na.rm=TRUE) | 
                          price >= quantile(price, 0.98, na.rm=TRUE), 1, 0)) %>%
  filter(!extreme)

# Checks
airbnb %>% 
  select(vars) %>% 
  pivot_longer(everything()) %>% 
  ggplot(aes(value)) + 
  geom_density() + 
  facet_wrap(~name, scales = "free")

# SF object
airbnb <- airbnb %>% st_as_sf(coords = c("longitude", "latitude"), crs = 4326, agr = "constant")



#######################################
# Open Stree map data #################
#######################################
features <- list("districts", "restaurants", "stops", "parks", "forest", "attraction") 
for (feature in glue::glue("data/{features}.Rdat")){
  load(feature)
}


tmap_mode("view")

## Districts
districts <- districts_raw$osm_multipolygons %>% select(name)
tm_shape(districts) + tm_fill(col = "name")

## Public Transport Stops
stops <- stops_raw$osm_points %>% 
  filter(railway %in% c("stop","tram_stop")) %>% 
  select(name)
stops <- stops %>% count(name) %>% ungroup() %>% select(-n)
stops <- stops %>% st_centroid()
tm_shape(stops) + tm_dots(col = "name")


## Restaurans
restaurants <- restaurants_raw %>% 
  polygons_to_points() %>% 
  select(name, cuisine, opening_hours)
tm_shape(restaurants) + tm_dots()

## Attractions
attraction1 <- attraction_raw$osm_multipolygons %>% 
  filter(!is.na(name)) %>% 
  st_centroid() %>% 
  select(name, description, building, architect, historic, museum, opening_hours, start_date, website) 
attraction2 <- attraction_raw$osm_polygons %>% 
  filter(!is.na(name)) %>% 
  st_centroid() %>% 
  select(name, description, building, architect, historic, museum, opening_hours, start_date, website)
attractions <- bind_rows(attraction1, attraction2)

tm_shape(attractions) + tm_dots(siz = 0.2, col="museum") 


######## Joining Airbnb and OSM
df <- airbnb

### Districts
df <- df %>% st_join(districts, join = st_covered_by)  # Which district covers an airbnb object?

### Restaurants
# What are the number of restaurants within radius around airbnb object?
airbnb <- airbnb %>% st_transform(3035)
restaurants <- restaurants %>% st_transform(3035)
attractions <- attractions %>% st_transform(3035)

# Get coordinates of restaurants and airbnb objcts
coord_airbnb <- st_coordinates(airbnb$geometry)
coord_restaurants <- st_coordinates(restaurants$geometry)
coord_attractions <- st_coordinates(attractions$geometry)

# Get Restaurants within 300m radius
radius_restaurants <- nn2(data = coord_restaurants, query = coord_airbnb, k = 500, searchtype = "radius", radius = 300, eps = 0.0)
radius_attractions <- nn2(data = coord_attractions, query = coord_airbnb, k = 200, searchtype = "radius", radius = 1000, eps = 0.0)


# Count restaurants and add to data
df <- df %>% mutate(n_restaurants = rowSums(radius_restaurants$nn.idx>0))
df <- df %>% mutate(n_attractions = rowSums(radius_attractions$nn.idx>0))


### Stops
stop_distances <- nngeo::st_nn(df, stops, maxdist = 15000, k = 1, returnDist = TRUE)
df <- df %>% mutate(stop_dist = round(as.numeric(stop_distances$dist)))


### Attractions
attractions <- attractions %>% st_transform(crs = 4326)
attraction_distances <- nngeo::st_nn(df, attractions, maxdist = 15000, k = 1, returnDist = TRUE)
df <- df %>% mutate(attraction_dist = round(as.numeric(attraction_distances$dist)))


### Parks
#tmap_mode("view")
#forest_raw$osm_polygons %>% tm_shape() + tm_fill("black")


#forest_area <- forest_raw$osm_polygons %>% st_area()
#forest_large <- forest_raw$osm_polygons[forest_area > units::set_units(1000000, m^2), ]

# nearest_forest <- nngeo::st_nn(df[1:10,], forest_large, maxdist = 10000, k = 1, returnDist = FALSE)
# nearest_forest_connect <- nngeo::st_connect(df[1:10,], forest_large, ids = bla)
# tmap_mode("view")
# tm_shape(forest_large) + tm_fill("darkgreen") + 
#   tm_shape(nearest_forest_connect) + tm_lines() + 
#   tm_shape(df[1:10,]) + tm_dots("blue", size = 0.02)
# Distance Calculation
# nearest_forest <- nngeo::st_nn(df, forest_large, maxdist = 10000, k = 1, returnDist = TRUE)
# length(nearest_forest$dist)
# df <- df %>% mutate(forest_dist = round(as.numeric(nearest_forest$dist)))

# parks_area <- parks_raw$osm_polygons %>% st_area()
# parks_large <- parks_raw$osm_polygons[parks_area > units::set_units(1000000, m^2), ]
# parks_large
# parks_large %>% tm_shape() + tm_fill("name")
# parks_raw$osm_multipolygons %>% tm_shape() + tm_fill("black")

save(df, restaurants, attractions, stops, districts, file = "data/prepared_data.Rdat")
