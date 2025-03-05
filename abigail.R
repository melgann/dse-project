# load libraries
library(tidyverse)

# cleaning rental dataset
q1_rental_og = read.csv("CommercialRentalStatsByStreet20250303231245.csv")
q2_rental_og = read.csv("CommercialRentalStatsByStreet20250303230811.csv")
q3_rental_og = read.csv("CommercialRentalStatsByStreet20250303230853.csv")
q4_rental_og = read.csv("CommercialRentalStatsByStreet20250303231354.csv")
# prices are per square meter per month

rental_prices = rbind(q1_rental_og,q2_rental_og,q3_rental_og,q4_rental_og)
  
rental_prices = rental_prices %>%
  rename_with(~c("price_25%", "price_median","price_75%"), .cols=3:5) %>%
  filter(!if_all(3:5, ~ . == "-")) %>%
  rename("Quarter" = "Reference.Quarter")
  
# read in planning area
street_name_planning_area = read.csv("street_name_planning_area.csv")
