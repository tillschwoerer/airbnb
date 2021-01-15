library(tidyverse)
library(sf)
library(tmap)
library(osmdata)


# Encode OSM data

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


## Conert polygons to points
polygons_to_points <- function(osm){
  points <- osm[["osm_points"]]           
  polygons <- osm[["osm_polygons"]]
  points_not_covered <- points[!lengths(st_intersects(points, polygons)), ]
  polygons_centroids <- polygons %>% st_centroid()
  bind_rows(points_not_covered, polygons_centroids)
}


## Define Visualization Function
vis_residuals <- function(model, data, mode = "plot", textsize = 1){
  districts_n <- df %>% count(name) %>% st_drop_geometry()
  tmap_mode(mode)
  model %>% 
    broom::augment_columns(data) %>%
    group_by(name) %>%
    summarise(error = mean(.resid)) %>% 
    inner_join(districts, by = "name") %>%
    inner_join(districts_n, by = "name") %>%
    st_as_sf() %>% 
    tm_shape() + 
    tm_fill(col = "error", n = 8) + 
    tm_text(text = "n", size = textsize) +
    tm_borders()  
}
