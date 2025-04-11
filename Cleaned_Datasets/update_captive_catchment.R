library(tidyverse)
library(httr)    
library(jsonlite) 
library(FactoMineR)   
library(factoextra)
library(fuzzyjoin)

## --------- Load Data --------- ##

stations <- read_csv("Raw_datasets/stations.csv", show_col_types = FALSE) %>%
  dplyr::select(-c("source", "comment")) %>%
  mutate(station_pattern = paste0("\\b", station_code, "\\b"))

dist_mrt <- read.csv("Cleaned_Datasets/number_of_mrt_near_street.csv") %>%
  select(Street_name, dist_to_nearest_mrt, StationCode)

all_bus_stops_cleaned <- read_csv("Cleaned_Datasets/bus_stops.csv", show_col_types = FALSE)

dist_bus <- read.csv("Cleaned_Datasets/number_of_bus_stops_near_street.csv",
                     colClasses = c(BusStopCode = "character")) %>%
  select(Street_name, dist_to_nearest_bus_stop, BusStopCode)

pop_and_num_schools <- read_csv("Cleaned_Datasets/streets_malls_school_pop.csv")



## --------- API Fetching Function ----------- ##

get_lta_paginated_data <- function(api_url, api_key, top = 50, dedup = FALSE) {
  skip <- 0
  all_data <- data.frame()
  
  while (TRUE) {
    response <- GET(
      url = api_url,
      add_headers(AccountKey = api_key, accept = "application/json"),
      query = list(`$skip` = skip, `$top` = top)
    )
    
    if (http_status(response)$category != "Success") {
      warning(paste("API request failed at skip =", skip, "-", http_status(response)$message))
      break
    }
    
    response_content <- content(response, "text", encoding = "UTF-8")
    json_data <- fromJSON(response_content)
    
    current_data <- as.data.frame(json_data$value)
    
    all_data <- rbind(all_data, current_data)
    
    if (nrow(current_data) < top) {
      break
    }
    
    skip <- skip + top
  }
  
  if (dedup) {
    all_data <- distinct(all_data)
  }
  
  return(all_data)
}



## -------- API Key --------- ##

source("api_keys.R")



## ------- Passenger Volume at Nearest MRT Station --------- ##

url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Train"
response <- GET(url, add_headers(AccountKey = api_key))

# Convert JSON Response to R List
data <- fromJSON(content(response, "text"))
csv_url <- data$value[[1]]  # Extract the URL from JSON

# Download CSV File
csv_file_train <- "train_passenger_volume.csv"
download.file(csv_url, csv_file_train, mode = "wb")  # Download CSV file

# Read in downloaded CSV file
# For each station, finding total tap out for all weekdays and all weekends in a month
passenger_volume_train <- read_csv(csv_file_train) %>%
  group_by(PT_CODE) %>%
  summarise(total_tap_in_mrt = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE), total_tap_out_mrt = sum(TOTAL_TAP_OUT_VOLUME, na.rm = TRUE)) %>%
  ungroup()

# Get month_tag
month_tag <- read_csv(csv_file_train)[[1, 1]]

# Fuzzy join to join based on whether station_code is a substring of PT_CODE because some stations are on more than one line
df_train <- regex_left_join(passenger_volume_train, stations, by = c("PT_CODE" = "station_pattern")) %>%
  dplyr::distinct(total_tap_in_mrt, .keep_all = TRUE)

expanded_df <- df_train %>%
  separate_rows(PT_CODE, sep = "/") %>%
  distinct()

# weights = 1/distance 
# create a new variable called dist_weighted_PV_bus that is a measure of Captive Catchment
mrt_code_cood_dist <- left_join(dist_mrt, expanded_df, by = c("StationCode" = "PT_CODE")) %>%
  dplyr::select(-c("station_code", "station_pattern")) %>%
  mutate(dist_weighted_PV_train = (1/dist_to_nearest_mrt) * total_tap_out_mrt)



## ------- Passenger Volume at Nearest Bus Stop --------- ##

url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Bus"
response <- GET(url, add_headers(AccountKey = api_key))

# Convert JSON Response to R List
data <- fromJSON(content(response, "text"))
csv_url <- data$value[[1]]  # Extract the first URL from JSON response

# Download and Save the CSV File
csv_file_bus <- "bus_passenger_volume.csv"  # Change path if needed
download.file(csv_url, csv_file_bus, mode = "wb")

# Load CSV into R
bus_data <- read_csv(csv_file_bus) %>%
  group_by(PT_CODE) %>%
  summarise(total_tap_in_bus = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE), total_tap_out_bus = sum(TOTAL_TAP_OUT_VOLUME, na.rm = TRUE)) %>%
  ungroup()

bus_code_cood <- left_join(bus_data, all_bus_stops_cleaned, by = c("PT_CODE" = "BusStopCode"))

# create a new variable called dist_weighted_PV_bus that is a measure of captive catchment
# weights = 1 / distance for distance-weighted passenger volume
bus_code_cood_dist <- left_join(dist_bus, bus_code_cood, by = c("BusStopCode" = "PT_CODE")) %>%
  dplyr::select(-c("RoadName", "Description")) %>%
  mutate(dist_weighted_PV_bus = (1/dist_to_nearest_bus_stop) * total_tap_out_bus)



## ---------- Computation for Captive Catchment ----------- ##

bus_and_mrt_tap_out <- bus_code_cood_dist %>%
  left_join(mrt_code_cood_dist, join_by("Street_name")) %>%
  left_join(pop_and_num_schools, join_by("Street_name")) %>%
  dplyr::select(c(dist_weighted_PV_bus, dist_weighted_PV_train, total_pop, Number_of_Schools))

## PCA model for Captive Catchment and determining the number of variable to choose
pca_model_captive_catchment <- PCA(bus_and_mrt_tap_out, scale.unit = TRUE, graph = FALSE)
fviz_eig(pca_model_captive_catchment, addlabels = TRUE, ylim = c(0, 100))
variance_explained <- pca_model_captive_catchment$eig[,2]  # 2nd column = % variance explained

## Choose the first 3 

## Obtaining an overall Captive Catchment score from PCA 
pca_scores_captive_catchment <- pca_model_captive_catchment$ind$coord[,0:3]
weights <- c(0.515, 0.27843,0.205814)  
captive_catchment <- as.matrix(pca_scores_captive_catchment) %*% weights  

## Maximum - minimum normalisation 
max_min_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

## Obtaining Captive Catchment scores with normalisation
captive_catchment_score <- cbind(pop_and_num_schools, captive_catchment) %>%
  dplyr::select("Street_name", "captive_catchment") %>%
  mutate(across(where(is.numeric), max_min_normalize))



## -------------- Save this month's captive catchment --------- ##

# Define output file path
output_file <- paste0("Cleaned_Datasets/Captive_Catchment/captive_catchment_score_", month_tag, ".csv")

# Write to CSV
write_csv(captive_catchment_score, output_file)
