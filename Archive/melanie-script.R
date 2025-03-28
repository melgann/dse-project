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



### PASSENGER VOLUMES (TRAIN) ###
stations <- read_csv("Raw_datasets/stations.csv") %>%
  dplyr::select(-c("source", "comment")) %>%
  mutate(station_pattern = paste0("\\b", station_code, "\\b"))

# For each station, finding what's the total tap in and tap out during weekends and weekdays
passenger_volume_train <- read_csv("Raw_datasets/transport_node_train_202502.csv") %>%
  group_by(PT_CODE, DAY_TYPE) %>%
  summarise(total_tap_in = sum(TOTAL_TAP_IN_VOLUME, na.rm = TRUE), total_tap_out = sum(TOTAL_TAP_OUT_VOLUME, na.rm = TRUE)) %>%
  ungroup()

# Fuzzy join to join based on whether station_code is a substring of PT_CODE because some stations are on more than one line
df <- regex_left_join(passenger_volume_train, stations, by = c("PT_CODE" = "station_pattern")) %>%
  dplyr::distinct(total_tap_in, .keep_all = TRUE)

#df <- full_join(stations, passenger_volume_train, by = c("station_code" = "PT_CODE"))











