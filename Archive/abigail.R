# load libraries
library(tidyverse)
library(dplyr)
library(stringr)

# cleaning rental dataset
q1_rental_og = read.csv("Raw_datasets/CommercialRentalStatsByStreet20250303231245.csv")
q2_rental_og = read.csv("Raw_datasets/CommercialRentalStatsByStreet20250303230811.csv")
q3_rental_og = read.csv("Raw_datasets/CommercialRentalStatsByStreet20250303230853.csv")
q4_rental_og = read.csv("Raw_datasets/CommercialRentalStatsByStreet20250303231354.csv")
# prices are per square meter per month

rental_prices = rbind(q1_rental_og,q2_rental_og,q3_rental_og,q4_rental_og)%>%
  rename("Quarter" = "Reference.Quarter") %>%
  rename_with(~c("price_25%", "price_median","price_75%"), .cols=3:5) %>%
  filter(!if_all(3:5, ~ . == "-"))  %>%
  mutate(Quarter=str_sub(Quarter,-1,-1)) %>%
  mutate(across(3:5, as.numeric))
  
rental_prices = rental_prices  %>%
  group_by(Street) %>%
  summarise(avg_median_price=mean(price_median, na.rm=TRUE))

#write csv
#write.csv(rental_prices, "rental_prices.csv", row.names = FALSE)


#drive timing
population <- read_csv("Raw_datasets/respopagesex2024.csv", show_col_types = FALSE)

population_by_area <- population %>% dplyr::select(PA, Pop) %>%
  group_by(PA) %>% 
  summarise(total_pop = sum(Pop)) %>%
  arrange(desc(total_pop)) 

streets <- read_csv("Cleaned_Datasets/street_name_planning_area.csv")
streets_vector <- unique(streets$Street_name)  # Get unique street names
planning_areas_with_streets_vector <- unique(streets$Planning_Area)
planning_areas_vector <- unique(planning_areas$planning_area) 

nobody = population_by_area %>%
  filter(total_pop==0)
nobody = nobody$PA

df_all_combis_drive_timing <- crossing(Planning_Area = planning_areas_vector, Street = streets_vector)

drive_time = read_csv("Cleaned_Datasets/drive_timing_all.csv")

df_all_combis_drive_timing = df_all_combis_drive_timing %>%
  full_join(drive_time, by=c("Planning_Area"="Planning_area", "Street"="Street_name")) %>%
  filter(!Planning_Area %in% nobody)

#pt timing
df_all_combis_pt_timing = read.csv("Cleaned_Datasets/final_public_transport_to_streets_from_planning_area.csv")

#diff between pt and drive
df_drive_timing = df_all_combis_drive_timing %>%
  dplyr::select(1,2,4)

df_pt_timing = df_all_combis_pt_timing %>%
  dplyr::select(2,3,4)

df_pt_vs_drive = df_pt_timing %>%
  full_join(df_drive_timing, by=c("Street_name"="Street", "Planning_area"="Planning_Area")) %>%
  rename("Time_taken_pt"="Time_taken") %>%
  mutate(diff = Time_taken_pt-Time_taken_drive) %>%
  rename("Street (Destination)"="Street_name", "Residential Planning Area (Origin)"="Planning_area")

#weighted avg by population
street_name_planning_area = read.csv("Cleaned_Datasets/street_name_planning_area.csv")

#add info on planning area for each DESTINATION street
df_pt_vs_drive = df_pt_vs_drive %>%
  left_join(street_name_planning_area %>% select(Street_name,Planning_Area), by = c("Street (Destination)"="Street_name"))%>%
  rename("Planning_Area (Destination)" = "Planning_Area") %>%
  select(`Street (Destination)`, `Planning_Area (Destination)`, everything()) 

#add info on population per ORIGIN planning area
df_pt_vs_drive = df_pt_vs_drive %>%
  left_join(population_by_area, by = c("Residential Planning Area (Origin)"="PA"))%>%
  select(1,2,3, total_pop, everything()) 

#weighted diffs
df_pt_vs_drive = df_pt_vs_drive %>%
  group_by(`Street (Destination)`) %>%
  mutate(weight = total_pop / sum(total_pop)) %>%
  ungroup() %>%
  mutate(weighted_diff = diff * weight) 

street_by_weighted_diff = df_pt_vs_drive %>%
  group_by(`Street (Destination)`) %>%
  summarise(weighted_diff_sum = sum(weighted_diff))
  
#write.csv(street_by_weighted_diff, "street_by_weighted_diff.csv", row.names = FALSE)
            