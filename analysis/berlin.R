# Packages

library(tidyverse)
library(sf)
library(tmap)
library(osmdata)
library(RANN)


# Helper Functions
encode_osm <- function(list){
    # For all data frames in query result
    for (df in (names(list)[map_lgl(list, is.data.frame)])) {
        last <- length(list[[df]])
        # For all columns except the last column ("geometry")
        for (col in names(list[[df]])[-last]){
            # Change the encoding to UTF8
            Encoding(list[[df]][[col]]) <- "UTF-8"
        }
    }
    return(list)
}

# AirBnB
airbnb <- read_csv("C:/Users/tschwoer/Downloads/listings.csv")
df <- airbnb %>% select(id, listing_url, host_response_time,
              neighbourhood_cleansed, neighbourhood_group_cleansed,
              latitude, longitude, property_type, room_type, accommodates, bathrooms,
              bedrooms, beds, price, number_of_reviews, review_scores_rating) %>% 
  mutate(price = as.numeric(str_sub(price, 2)))

df <- st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326, agr = "constant")

##############################################################################
# Open Street Map ############################################################
##############################################################################
## Districts
osm <- opq("Berlin, Germany") %>%
  add_osm_feature("boundary", "administrative") %>%
  add_osm_feature("admin_level", c(10)) %>%
  osmdata_sf() %>% 
  unname_osmdata_sf() %>%
  encode_osm()

## 
osm_restaurant <- opq("Berlin, Germany") %>%
  add_osm_feature("amenity", "restaurant") %>%
  osmdata_sf() %>% 
  unname_osmdata_sf() %>%
  encode_osm()

#############################################################################
# Convert data into features ################################################
#############################################################################

## Polygons to points
points <- osm_restaurant[["osm_points"]]
polygons <- osm_restaurant[["osm_polygons"]]
points_not_covered <- points[!lengths(st_intersects(points, polygons)), ]
polygons_centroids <- polygons %>% st_centroid()

polygons %>% tm_shape() + tm_fill("red") +
  tm_shape(polygons_centroids) + tm_dots("black", size = 0.01)

points_polygons <- bind_rows(points_not_covered, polygons_centroids)


## Calculate distances
airbnb_coord <- st_coordinates(df$geometry)
rest_coord <- st_coordinates(points_polygons)

closest <- nn2(data = rest_coord, query = airbnb_coord, 
               k = 500, searchtype = "radius", 
               radius = 0.005,eps = 0.0)

df <- df %>% mutate(n_close_rest = rowSums(closest$nn.idx>0))

##############################################################################
# Visualization ############################################################
##############################################################################
tmap_mode("view")
tm_shape(osm$osm_multipolygons, name = "Districts") + tm_borders() +
  tm_fill(id = "name") +
  tm_shape(df) + tm_dots(col = "review_scores_rating", size = 0.01) +
  tm_shape(osm_restaurant$osm_points) + tm_dots(col = "blue", size = 0.01)


#############################################################################
# Regression ################################################################
#############################################################################
df2 <- df %>% filter(accommodates==3)
View(df2)
model <- lm(price ~ neighbourhood_group_cleansed + room_type + bedrooms + review_scores_rating + I(cut_number(n_close_rest, 4)), data = df2)
summary(model)
summary(df2$n_close_rest)
df2 %>% count(beds)
cut_in
