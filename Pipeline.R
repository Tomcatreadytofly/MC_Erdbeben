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
install_if_not_present("leaflet")

library(tidyverse)
library(geojsonR)
# library(sf)
# library(tidyjson)
library(jsonlite)
library(lubridate)
library(leaflet)


# create variables for different periodes
past_hours <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_hour.geojson"
past_day <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_day.geojson"
past_week <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_week.geojson"
past_month <- "https://earthquake.usgs.gov/earthquakes/feed/v1.0/summary/all_month.geojson"

# pipeline <- function(geojson_file) {
# 
#       # read data from website
#       dump <- Dump_From_GeoJson(geojson_file)
#       
#       # convert to JSON
#       js <- fromJSON(dump)
#       
#       # extract metadata
#       metadata <- js$metadata
#       
#       # extract bbox
#       bbox <- js$bbox
#       bbox <- data.frame(bbox, coord = c("lon_min", "lat_min", "depth_min", "lon_max", "lat_max", "depth_max"))
#       
#       # extract features
#       features <- js$features
#       
#       # extract coordinates
#       geometry <- features$geometry
#       coordinates <- geometry$coordinates
#       coordinates <- do.call(rbind, coordinates)
#       colnames(coordinates) <- c("lon", "lat", "depth")
#       
#       # extract properties into a dataframe
#       properties <- cbind(features$properties, coordinates)
#       
#       properties <- properties %>% select(mag, place, time, status, updated, type, lon, lat, depth, tsunami, sig, code)
#       
#       # convert columns time and updated into datetime
#       basedate <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
#       properties <- properties %>% 
#         mutate(time = basedate + (time/ 1000)) %>% 
#         mutate(updated = basedate + (updated/1000))
#       
#       # only accept values in the permitted range
#       properties <- properties %>% 
#         filter(between(lon, -180, 180),
#                between(lat, -90, 90),
#                between(depth, 0, 1000),
#                between(mag, 5, 10))
#       
#       # convert metadata info generated int datetime
#       metadata$generated <- basedate + (metadata$generated/1000)
#       
#       # drop NA for mandatory fields
#       properties <- properties %>% 
#         drop_na(mag, place, time, updated, type, lon, lat, code)
#       
#       # return list with prepared data
#       list(properties_df = properties, metadata_obj = metadata, bbox_obj = bbox)
# 
# }


pipeline <- function(geojson_file) {
  
  # read data from website
  dump <- Dump_From_GeoJson(geojson_file)
  
  # convert to JSON
  js <- fromJSON(dump)
  
  # extract features
  features <- js$features
  
  # extract coordinates
  geometry <- features$geometry
  coordinates <- geometry$coordinates
  coordinates <- do.call(rbind, coordinates)
  colnames(coordinates) <- c("lon", "lat", "depth")
  
  # extract properties into a dataframe
  properties <- cbind(features$properties, coordinates)
  
  properties <- properties %>% select(mag, place, time, status, updated, type, lon, lat, depth, tsunami, sig, code)
  
  # convert columns time and updated into datetime
  basedate <- as.POSIXct("1970-01-01 00:00:00", tz = "UTC")
  properties <- properties %>% 
    mutate(time = basedate + (time/ 1000)) %>% 
    mutate(updated = basedate + (updated/1000))
  
  # only accept values in the permitted range
  properties <- properties %>% 
    filter(between(lon, -180, 180),
           between(lat, -90, 90),
           between(depth, 0, 1000),
           between(mag, 5, 10))
  
  # drop NA for mandatory fields
  properties <- properties %>% 
    drop_na(mag, place, time, updated, type, lon, lat, code)
  
  # # return list with prepared data
  # list(properties_df = properties, metadata_obj = metadata, bbox_obj = bbox)
  return(properties)
  
}
# 
# results <- pipeline(past_hours)
# results <- pipeline(past_day)
# results <- pipeline(past_week)
# results <- pipeline(past_month)

properties <- pipeline(past_hours)
properties <- pipeline(past_day)
properties <- pipeline(past_week)
properties <- pipeline(past_month)

# properties <- results$properties_df
# bbox <- results$bbox_obj
# metadata <- results$metadata_obj
# bbox_wider <- pivot_wider(bbox, names_from = coord, values_from = bbox)

leaflet(properties) %>% 
  addTiles() %>% 
  addCircleMarkers(
    lat = ~lat,
    lng = ~lon,
    radius = ~mag *2,  # Groesse der Punkte nach Magnitude
    # fillColor = ~colorQuantile("YlOrRd", mag)(mag),  # Farbschema
    fillColor = ~colorNumeric(palette = "YlOrRd", domain = mag)(mag),
    color = "#000000",  # Umrandungsfarbe
    weight = 1,  # Umrandungsdicke
    opacity = 1,  # Umrandungstransparenz
    fillOpacity = 0.7,  # Fuelltransparenz
    popup = ~paste("Ort:", place, "<br>Magnitude:", round(mag, 4), "<br>Tiefe:", round(properties$depth, 2), "km")  # Popup-Info
  )


