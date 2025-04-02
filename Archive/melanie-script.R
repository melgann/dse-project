library(readr)
library(readxl)
library(stringr)
library(lubridate)
library(tidyverse)
library(dplyr)
library(dynlm)
library(vars)
library(tseries)
library(forecast)
library(sf)
library(geosphere)
library(fuzzyjoin)

### FINDING OUT WHICH PLANNING AREA HAS THE HIGHEST POPULATION ###
population <- read_csv("Raw_datasets/respopagesex2024.csv", show_col_types = FALSE)

population_by_area <- population %>% dplyr::select(PA, Pop) %>%
  group_by(PA) %>% 
  summarise(total_pop = sum(Pop)) %>%
  arrange(desc(total_pop)) 


### FINDING THE NO OF SCHOOLS IN EACH PLANNING AREA ###
planning_areas <- st_read("Raw_datasets/district_and_planning_area.geojson")
schools <- st_read("Raw_datasets/LTASchoolZone.geojson")

ggplot() +
  geom_sf(data = planning_areas, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = schools, color = "red", size = 2) +
  theme_minimal() +
  ggtitle("Planning Areas and Schools in Singapore")

# Transform to Singapore's standard coordinate system (SVY21, EPSG:3414)
planning_areas <- st_transform(planning_areas, 3414)
schools <- st_transform(schools, 3414)


# Spatial join to count schools in each planning area
school_counts <- planning_areas %>%
  st_join(schools, join = st_contains) %>%  # Join schools inside planning areas
  group_by(Planning_Area = planning_area) %>% 
  summarise(Number_of_Schools = n()) %>%
  ungroup() %>%
  arrange(desc(Number_of_Schools)) 

# ROW IS THE PLANNING AREAS AND COLS IS THE SCHOOL
#distance_matrix <- as.data.frame(st_distance(planning_areas, schools))



### FINDING THE NO OF MALLS IN EACH OF THE PLANNING AREA ###
malls <- read.csv("Raw_datasets/shopping_mall_coordinates.csv")

# Transform the longitude and latitude to geometry?
malls <- malls %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(3414)

# Transform to Singapore's standard coordinate system (SVY21, EPSG:3414)
planning_areas <- st_transform(planning_areas, 3414)

ggplot() +
  geom_sf(data = planning_areas, fill = "lightblue", alpha = 0.5) +
  geom_sf(data = malls, color = "red", size = 2) +
  theme_minimal() +
  ggtitle("Planning Areas and Malls in Singapore")


# Spatial join to count malls in each planning area
mall_counts <- planning_areas %>%
  st_join(malls, join = st_contains) %>%  # Join schools inside planning areas
  group_by(Planning_Area = planning_area) %>% 
  summarise(Number_of_Malls = n()) %>%
  ungroup() %>%
  arrange(desc(Number_of_Malls)) 

# Drop geometry to join them together
mall_counts_nogeom <- st_drop_geometry(mall_counts)
school_counts_nogeom <- st_drop_geometry(school_counts)

combine_dataframe <- left_join(mall_counts_nogeom, school_counts_nogeom, by = "Planning_Area") %>%
  mutate(Planning_Area = toupper(Planning_Area)) 


### COMBINE ALL THE DATAFRAMES WITH STREETS AND THE CORRESPONDING LANGITUDE AND LONGITUDE ###
streets <- read_csv("Cleaned CSV Datasets/street_name_planning_area.csv")

population_by_area <- population_by_area %>%
  mutate(PA = toupper(PA))

final_dataframe <- streets %>%
  left_join(combine_dataframe, by = "Planning_Area") %>%
  left_join(population_by_area, by = c("Planning_Area" = "PA"))


### EXPORT AS CSV ###
write.csv(final_dataframe, "/Users/melaniegan/Downloads/streets_malls_school_pop.csv", row.names = FALSE)


### FINDING THE DISTANCE FROM THE STREET TO THE MRT STATIONS ###
stations <- read_csv("Raw_datasets/stations.csv") %>%
  dplyr::distinct(station_name, .keep_all = TRUE)


#Function to calculate distance
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# Define the distance threshold (in meters)
distance_threshold <- 500

# Initialize a list to store results
results <- list()

# Loop through each street
for (i in 1:nrow(streets)) {
  street_name <- streets$Street_name[i]
  street_lat <- streets$latitude[i]
  street_lon <- streets$longitude[i]
  
  # Loop through each bus stop
  for (j in 1:nrow(stations)) {
    station_code <- stations$station_code[j]
    station_lat <- stations$lat[j]
    station_lon <- stations$lon[j]
    
    # Calculate the distance
    distance <- calculate_distance(street_lat, street_lon, station_lat, station_lon)
    
    # Check if the distance is below the threshold
    if (distance <= distance_threshold) {
      results <- rbind(results, data.frame(
        StreetName = street_name,
        StationCode = station_code,
        Distance = distance
      ))
    }
  }
}

# Convert the results to a data frame
results_df <- as.data.frame(results)

# Print the results
print(results_df)

number_of_mrt_near_street <- results_df %>%
  group_by(StreetName) %>%
  summarise(number_of_stations=n(), average_dist=sum(Distance)/n())

# Get a dataframe with the street, PA, number of stations that are 'near', and their average distance
number_of_mrt_near_street <- left_join(streets,number_of_mrt_near_street, by=join_by("Street_name" == "StreetName"))
number_of_mrt_near_street <-number_of_mrt_near_street %>%
  dplyr::select("Street_name", "Planning_Area", "number_of_stations", "average_dist")

#Export CSV
#write.csv(number_of_mrt_near_street, "/Users/melaniegan/Documents/University/Y3S2/DSE3101/dse-project/Cleaned CSV Datasets/number_of_mrt_near_street.csv")





###########
library(httr)
library(jsonlite)

# API Key
api_key <- "S4FSEoEgQc2BazhsvqOHjQ=="

# API Endpoint
url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Train"

# Send Request and Get Response
response <- GET(url, add_headers(AccountKey = api_key))

# Convert JSON Response to R List
data <- fromJSON(content(response, "text"))
csv_url <- data$value[[1]]  # Extract the URL from JSON

#print(paste("Download CSV from:", csv_url))  # Check the link

# Download CSV File
csv_file_train <- "train_passenger_volume.csv"
download.file(csv_url, csv_file_train, mode = "wb")  # Download CSV file


### PASSENGER VOLUMES (TRAIN) ###
stations <- read_csv("../Raw_datasets/stations.csv") %>%
  dplyr::select(-c("source", "comment")) %>%
  mutate(station_pattern = paste0("\\b", station_code, "\\b"))

# Read in downloaded CSV file
# For each station, finding what's the total tap in and tap out 
passenger_volume_train <- read_csv(csv_file_train) %>%
  group_by(PT_CODE) %>%
  summarise(total_tap_in_mrt = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE), total_tap_out_mrt = sum(TOTAL_TAP_OUT_VOLUME, na.rm = TRUE)) %>%
  ungroup()

# Fuzzy join to join based on whether station_code is a substring of PT_CODE because some stations are on more than one line
df_train <- regex_left_join(passenger_volume_train, stations, by = c("PT_CODE" = "station_pattern")) %>%
  dplyr::distinct(total_tap_in_mrt, .keep_all = TRUE)



#################
#Function to calculate distance
calculate_distance <- function(lat1, lon1, lat2, lon2) {
  distHaversine(c(lon1, lat1), c(lon2, lat2))
}

# Define the distance threshold (in meters)
distance_threshold <- 500000

# Initialize a list to store results
results <- list()

# Loop through each street
for (i in 1:nrow(streets)) {
  street_name <- streets$Street_name[i]
  street_lat <- streets$latitude[i]
  street_lon <- streets$longitude[i]
  
  # Loop through each station
  for (j in 1:nrow(stations)) {
    station_code <- stations$station_code[j]
    station_lat <- stations$lat[j]
    station_lon <- stations$lon[j]
    
    # Calculate the distance
    distance <- calculate_distance(street_lat, street_lon, station_lat, station_lon)
    
    # Check if the distance is below the threshold
    
    results <- rbind(results, data.frame(
      StreetName = street_name,
      StationCode = station_code,
      Distance = distance))
    
  }
}

# Convert the results to a data frame
results_df <- as.data.frame(results)

##########################

dist_mrt <- results_df %>%
  group_by(StreetName) %>%
  summarise(dist_to_nearest_mrt=min(Distance), StationCode = StationCode[which.min(Distance)])


# weights = 1/distance
# create a new variable called dist_weighted_PV_bus that is a measure of footfall
expanded_df <- df_train %>%
  separate_rows(PT_CODE, sep = "/") %>%
  distinct()

mrt_code_cood_dist <- left_join(dist_mrt, expanded_df, by = c("StationCode" = "PT_CODE")) %>%
  dplyr::select(-c("station_code", "station_pattern")) %>%
  mutate(dist_weighted_PV_train = (1/dist_to_nearest_mrt) * total_tap_out_mrt)






############## BUS STOPS #################################

# Step 1: Set API Key
api_key <- "S4FSEoEgQc2BazhsvqOHjQ=="

# Step 2: Define API Endpoint
url <- "http://datamall2.mytransport.sg/ltaodataservice/PV/Bus"

# Step 3: Send Request and Get Response
response <- GET(url, add_headers(AccountKey = api_key))

# Step 4: Convert JSON Response to R List
data <- fromJSON(content(response, "text"))

# Step 5: Extract CSV Download Link
csv_url <- data$value[[1]]  # Extract the first URL from JSON response

print(paste("Download CSV from:", csv_url))  # Check the link

# Step 6: Download and Save the CSV File
csv_file_bus <- "bus_passenger_volume.csv"  # Change path if needed
download.file(csv_url, csv_file_bus, mode = "wb")

# Step 7: Load CSV into R
bus_data <- read_csv(csv_file_bus) %>%
  group_by(PT_CODE) %>%
  summarise(total_tap_in_bus = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE), total_tap_out_bus = sum(TOTAL_TAP_OUT_VOLUME, na.rm = TRUE)) %>%
  ungroup()


all_bus_stops_cleaned <- read_csv("../Cleaned_Datasets/bus_stops.csv")

bus_code_cood <- left_join(bus_data, all_bus_stops_cleaned, by = c("PT_CODE" = "BusStopCode"))


##### SAME code as in combned
# Convert to data.table 
coords_dt <- as.data.table(coords)
bus_stops_dt <- as.data.table(distinct(all_bus_stops_cleaned))

results_list <- list()  

for (i in 1:nrow(coords_dt)) {
  street_name <- coords_dt$Street_name[i]
  street_lat <- coords_dt$latitude[i]
  street_lon <- coords_dt$longitude[i]
  
  # Vectorized distance calculation for all bus stops
  distances <- distHaversine(
    matrix(c(street_lon, street_lat), nrow = 1),
    matrix(c(bus_stops_dt$Longitude, bus_stops_dt$Latitude), ncol = 2)
  )
  
  # Store results in a list
  results_list[[i]] <- data.frame(
    StreetName = street_name,
    BusStopCode = bus_stops_dt$BusStopCode,
    Distance = distances
  )
}

# Combine results 
results_df_bus <- rbindlist(results_list)
#############

# Finding the distance to the nearest bus stop from each street
dist_bus <- results_df_bus %>%
  group_by(StreetName) %>%
  summarise(dist_to_nearest_bus_stop = min(Distance, na.rm = TRUE), BusStopCode = BusStopCode[which.min(Distance)])

# weights = 1/distance
# create a new variable called dist_weighted_PV_bus that is a measure of footfall


bus_code_cood_dist <- left_join(dist_bus, bus_code_cood, by = c("BusStopCode" = "PT_CODE")) %>%
  dplyr::select(-c("RoadName", "Description")) %>%
  mutate(dist_weighted_PV_bus = (1/dist_to_nearest_bus_stop) * total_tap_out_bus)




pop_and_num_schools <- read_csv("../Cleaned_Datasets/streets_malls_school_pop.csv")

bus_and_mrt_tap_out <- bus_code_cood_dist %>%
  left_join(mrt_code_cood_dist, join_by("StreetName")) %>%
  left_join(pop_and_num_schools, join_by("StreetName" == "Street_name")) %>%
  dplyr::select(c(dist_weighted_PV_bus, dist_weighted_PV_train, total_pop, Number_of_Schools))

pca_model <- PCA(bus_and_mrt_tap_out, scale.unit = TRUE, graph = FALSE)
fviz_eig(pca_model, addlabels = TRUE, ylim = c(0, 100))
variance_explained <- pca_model$eig[,2]  # 2nd column = % variance explained

print(variance_explained)

pca_scores <- pca_model$ind$coord[,0:3]
weights <- c(0.515, 0.27843,0.205814)  
footfall <- as.matrix(pca_scores_) %*% weights  

new_df <- cbind(df_normalized, footfall) %>%
  dplyr::select("Street_name", "footfall")

footfall_score <- new_df %>%
  mutate(across(where(is.numeric), max_min_normalize))

df1 <- read.csv("../Cleaned_Datasets/number_of_bus_stops_near_street.csv")
df2 <- read.csv("../Cleaned_Datasets/rental_prices.csv")
df3 <- read.csv("../Cleaned_Datasets/streets_malls_school_pop.csv") %>%
  dplyr:: select(Street_name, Number_of_Malls, Number_of_Schools, total_pop, longitude, latitude, "Planning_Area")
df4 <- read.csv("../Cleaned_Datasets/number_of_mrt_near_street.csv") %>%
  dplyr:: select(Street_name, number_of_stations, dist_to_nearest_mrt)

accessibility_scores <- read.csv("../Cleaned_Datasets/accessibility_scores.csv")

df_final <- df3 %>%
  left_join(df2, join_by("Street_name" == "Street")) %>%
  left_join(footfall_score, join_by("Street_name")) %>%
  left_join(accessibility_scores, join_by("Street_name"))

max_min_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}


df_select <- df_final %>%
  dplyr::select(c("footfall", "Number_of_Malls", "avg_median_price", "accessibility_score", "Street_name")) %>%
  mutate(across(where(is.numeric), max_min_normalize))


final_score <- 0.379537954*df_select$footfall + 0.2202202202*df_select$Number_of_Malls + 0.2202202202*df_select$accessibility_score -
  0.180418042*df_select$avg_median_price
cbind(final_score, df_select$"Street_name")
