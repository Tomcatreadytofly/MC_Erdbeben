# load mandatory packages

# function to check if package is present
install_if_not_present <- function(pkg_name){
  if(!requireNamespace(pkg_name, quietly = TRUE)){
    install.packages(pkg_name)
  }
}

install_if_not_present("tidyverse")
install_if_not_present("geojsonR")
# install_if_not_present("sf")
# install_if_not_present("tidyjson")
install_if_not_present("jsonlite")
install_if_not_present("lubridate")

library(tidyverse)
library(geojsonR)
# library(sf)
# library(tidyjson)
library(jsonlite)
library(lubridate)


# create variables for different periodes
past_hours <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
past_day <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson"
past_week <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_week.geojson"
past_month <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.geojson"

pipeline <- function(geojson_file) {

      # read data from website
      dump <- Dump_From_GeoJson(geojson_file)
      
      # convert to JSON
      js <- fromJSON(dump)
      
      # extract metadata
      metadata <- js$metadata
      
      # extract bbox
      bbox <- js$bbox
      bbox <- data.frame(bbox, names = c("lon_min", "lat_min", "depth_min", "lon_max", "lat_max", "depth_max"))
      
      # extract features
      features <- js$features
      
      # extract coordinates
      geometry <- features$geometry
      coordinates <- geometry$coordinates
      coordinates <- do.call(rbind, coordinates)
      colnames(coordinates) <- c("lon", "lat", "depth")
      
      # extract properties into a dataframe
      properties <- cbind(features$properties, coordinates)
      
      # convert columns time and updated into datetime
      basedate <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
      properties <- properties %>% 
        mutate(time = basedate + (time/ 1000)) %>% 
        mutate(updated = basedate + (updated/1000))
      
      # convert metadata info generated int datetime
      metadata$generated <- basedate + (metadata$generated/1000)
      
      # drop NA for mandatory fields
      properties <- properties %>% 
        drop_na(mag, place, time, updated, type, lon, lat, code)
      
      # return list with prepared data
      list(properties_df = properties, metadata_obj = metadata, bbox_obj = bbox)

}

# results <- pipeline(past_hours)
# results <- pipeline(past_day)
# results <- pipeline(past_week)
results <- pipeline(past_month)

properties <- results$properties_df
bbox <- results$bbox_obj
metadata <- results$metadata_obj
bbox_wider <- pivot_wider(bbox, names_from = names, values_from = bbox)


