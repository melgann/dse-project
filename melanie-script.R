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

### FINDING OUT WHICH PLANNING AREA HAS THE HIGHEST POPULATION ###
population <- read_csv("respopagesex2024.csv", show_col_types = FALSE)

population_by_area <- data %>% dplyr::select(PA, Pop) %>%
  group_by(PA) %>% 
  summarise(total_pop = sum(Pop)) %>%
  arrange(desc(total_pop)) 


### FINDING THE NO OF SCHOOLS IN EACH PLANNING AREA ###
planning_areas <- st_read("district_and_planning_area.geojson")
schools <- st_read("LTASchoolZone.geojson")

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
distance_matrix <- as.data.frame(st_distance(planning_areas, schools))



### FINDING THE NO OF MALLS IN EACH OF THE PLANNING AREA ###
malls <- read.csv("shopping_mall_coordinates.csv")

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
streets <- read_csv("street_name_planning_area.csv")

population_by_area <- population_by_area %>%
  mutate(PA = toupper(PA))

final_dataframe <- streets %>%
  left_join(combine_dataframe, by = "Planning_Area") %>%
  left_join(population_by_area, by = c("Planning_Area" = "PA"))


### EXPORT AS CSV ###
write.csv(final_dataframe, "/Users/melaniegan/Downloads/streets_malls_school_pop.csv", row.names = FALSE)

