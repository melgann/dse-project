library(polite)
library(rvest)
library(tidyverse)
library(shinyWidgets)
library(sortable)
library(leaflet)
library(sf)
library(shiny)
library(DT)
library(leaflet.extras) 

########################
### Loading Datasets ###
########################

planning_areas_geo <- st_read("../Raw_datasets/district_and_planning_area.geojson") %>%
  st_transform(4326)

planning_area_streets <- read.csv("../Cleaned_Datasets/street_name_planning_area.csv")

rental_streets <- read.csv(("../Cleaned_Datasets/rental_prices.csv"))

no_of_mrt <- read.csv(("../Cleaned_Datasets/number_of_mrt_near_street.csv"))

no_of_busstops <- read.csv(("../Cleaned_Datasets/number_of_bus_stops_near_street.csv"))

no_of_malls_schools <- read.csv(("../Cleaned_Datasets/streets_malls_school_pop.csv"))


########################################
### Getting Regions & Planning Areas ###
########################################

url <- "https://en.wikipedia.org/wiki/Planning_areas_of_Singapore"
url_use <- bow(url)
results <- scrape(url_use) %>%
  html_elements("table.wikitable") %>%
  html_table()
planning_area_regions <- results[[2]] %>%
  dplyr::select(1, Region) %>%
  rename(planningArea = `Name (English)`, region = Region)


###############################
### Forming Table for Tab 2 ###
###############################

planning_area_regions$planningArea <- toupper(planning_area_regions$planningArea)

streets_PA_region <- inner_join(planning_area_regions, planning_area_streets, by = c("planningArea" = "Planning_Area")) 

temp_table <- streets_PA_region %>% 
  inner_join(rental_streets, by = c("Street_name" = "Street")) %>% 
  inner_join(no_of_mrt, by = c("Street_name" = "Street_name")) %>% 
  inner_join(no_of_busstops, by = c("Street_name" = "StreetName")) %>% 
  inner_join(no_of_malls_schools, by = c("Street_name" = "Street_name")) %>%
  mutate(number_of_stations = replace(number_of_stations, is.na(number_of_stations), 0)) %>% 
  mutate(total_pop = total_pop/1000)

table_scaled <- temp_table %>%
  mutate(indicator_mrt = ifelse(is.na(average_dist_mrt), 0, 1)) %>%
  mutate(average_dist_mrt = replace(average_dist_mrt, is.na(average_dist_mrt), 10000))

max_min_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

table_normalized <- table_scaled %>%
  mutate(across(where(is.numeric), max_min_normalize))

table_modelled <- table_normalized %>% 
  mutate(attractiveness_score = round((-0.2 * avg_median_price + 
                                         0.2 * total_pop + 
                                         0.15 * Number_of_Malls + 
                                         0.15 * Number_of_Schools + 
                                         0.15 * number_of_bus_stops - 
                                         0.15 * indicator_mrt * average_dist_mrt) * 1000, 2)) %>% 
  mutate(attractiveness_score = max_min_normalize(attractiveness_score) * 100) %>% 
  select(c("Street_name", "attractiveness_score"))

final_table <- temp_table %>% 
  left_join(table_modelled, by = "Street_name") %>%
  rename("Rental Price ($)" = "avg_median_price", 
         "Planning Area" = "planningArea",
         "Region" = "region", 
         "Street" = "Street_name", 
         "Number of MRT Stations" = "number_of_stations",
         "Number of Bus Stops" = "number_of_bus_stops",
         "Number of Malls" = "Number_of_Malls", 
         "Number of Schools" = "Number_of_Schools",
         "Population (in thousands)" = "total_pop", 
         "Score (upon 100)" = "attractiveness_score") %>% 
  select(4, 1, 2, 7, 10, 13, 19, 20, 21, 22) %>% 
  mutate(`Rental Price ($)` = round(`Rental Price ($)`, 2)) %>% 
  mutate(`Score (upon 100)` = round(`Score (upon 100)`, 2))

################
### Heat Map ###
################

planning_areas_geo <- planning_areas_geo %>% 
  mutate(planning_area = toupper(planning_area), 
         `Region` = toupper(district)) %>% 
  select(-c("district"))

map_table <- final_table %>% 
  select(2, 3, 4, 10) %>% 
  group_by(`Planning Area`) %>% 
  summarise(
    `Median Score (upon 100)` = median(`Score (upon 100)`), 
    `Median Rental Price ($)` = median(`Rental Price ($)`)) %>% 
  left_join(planning_areas_geo, by = c(`Planning Area` = "planning_area"))

#################
### Shiny App ###
#################

ui <- fluidPage(
  #tags$head(
  # tags$style(HTML("
  #   /* Targeting the 'Select All' and 'Deselect All' button text */
  #   .bs-actionsbox .btn-group .btn {
  #     font-size:14px !important;  /* Adjust the font size */
  #   }
  #  "))
  #),
  
  titlePanel("Name of Project"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      pickerInput(
        inputId = "regionFilter",
        label = "Region", 
        choices = sort(unique(planning_area_regions$region)),
        multiple = TRUE,
        options = pickerOptions(container = "body",
                                title = "Select",
                                actionsBox = TRUE)
      ),
      
      helpText(
        "To further narrow down areas, please select planning areas below."
      ),
      
      pickerInput(
        inputId = "planningAreaFilter",
        label = "Planning Area", 
        choices = split(planning_area_regions$planningArea,
                        planning_area_regions$region),
        multiple = TRUE,
        options = pickerOptions(container = "body",
                                title = "Select",
                                selectedTextFormat = "count > 3",
                                actionsBox = TRUE,
                                liveSearch = TRUE)
      ),
  
      sliderTextInput(
        inputId = "rentalPriceRangeFilter",
        label = "Rental Price Range", 
        choices = seq(from = 30, to = 500, by = 25), #should take the min and max from the data set tho
        selected = c(30, 480),
        grid = TRUE
      ),
      
      rank_list(
        text = "Rank the importance of the following factors:",
        labels = c("Proximity to MRT", 
                   "Foot Traffic", "Rental Cost", 
                   "Nearby Malls", 
                   "Population Density"),
        input_id = "user_ranking",
        options = sortable_options(multiDrag = FALSE)
      ), 
      
      # PickerInput for selecting columns to display in table
      pickerInput(
        inputId = "variablesFilter",
        label = "Select Columns to Display",
        choices = c("Rental Price ($)", 
                    "Number of MRT Stations", 
                    "Number of Bus Stops", 
                    "Number of Malls", 
                    "Number of Schools", 
                    "Population (in thousands)"),  
        multiple = TRUE,
        selected = colnames(final_table), # Default to all columns
        options = pickerOptions(container = "body",
                                actionsBox = TRUE)
      )
    ),
  
    mainPanel(
      tabsetPanel(
        tabPanel("Map", leafletOutput("map", height = 600)),
        tabPanel("Table", DT::DTOutput("table")),
        tabPanel("Comparison", DT::DTOutput("comparison"))
      )
    )
    # tabsetPanel(
    #   tabPanel("Map", leafletOutput("map")),
    #   tabPanel("Table", DT::DTOutput("table")),
    #   tabPanel("Comparison", tableOutput("comparisonTable"))
    # )
  )
)

server <- function(input, output, session) {
  
  observe({
    selected_regions <- input$regionFilter
    
    # Filter planning areas based on selected region
    filtered_planning_areas <- planning_area_regions %>%
      filter(region %in% selected_regions) %>%
      pull(planningArea)
    
    # Update pickerInput choices dynamically
    updatePickerInput(session, "planningArea",
                      choices = split(filtered_planning_areas, selected_regions))
  }) 
  
  # Reactive expression for map tab to filter based on region, planning area and rental price 
  filtered_data_map <- reactive({
    filtered_data <- map_table
    
    # Filter by region
    selected_regions <- input$regionFilter
    if (length(selected_regions) > 0) {
      filtered_data <- filtered_data %>%
        filter(`Region` %in% selected_regions)
    }
    
    # Filter by planning area
    selected_planningareas <- input$planningAreaFilter
    if (length(selected_planningareas) > 0) {
      filtered_data <- filtered_data %>%
        filter(`Planning Area` %in% selected_planningareas)
    }
    
    selected_rental_price_range <- input$rentalPriceRangeFilter  # Get the selected price range
    filtered_data <- filtered_data %>%
      filter(`Median Rental Price ($)` >= selected_rental_price_range[1] & 
               `Median Rental Price ($)` <= selected_rental_price_range[2])
    
    return(filtered_data)
  })
  
  output$map <- renderLeaflet({
    # Filtered data for heatmap
    filtered_map_data <- filtered_data_map()
    
    # Ensure it's an sf object (with geometry column)
    if (!inherits(filtered_map_data, "sf")) {
      filtered_map_data <- st_as_sf(filtered_map_data)
    }
    
    # Define color palette based on Median Score
    pal <- colorNumeric(
      palette = "YlOrRd",  # You can change the color scheme
      domain = filtered_map_data$`Median Score (upon 100)`
    )
    
    leaflet(filtered_map_data) %>%
      addProviderTiles(providers$CartoDB.PositronNoLabels) %>%
      addPolygons(
        data = filtered_map_data,
        color = "black",
        weight = 1,
        fillColor = ~pal(`Median Score (upon 100)`),
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "black",
          fillOpacity = 0.6,
          bringToFront = TRUE
        ),
        label = ~paste0(`Region`, ": ", `Planning Area`),
        popup = ~paste0("<b>Planning Area:</b> ", `Planning Area`, "<br>",
                        "<b>Median Score (upon 100):</b> ", `Median Score (upon 100)`, "<br>",
                        "<b>Median Rental Price ($):</b> ", `Median Rental Price ($)`)
      ) %>%
      setView(lng = 103.815539, lat = 1.346594, zoom = 12) %>%
      setMaxBounds(
        lng1 = 103.6, lat1 = 1.15,   # Southwest corner
        lng2 = 104.1, lat2 = 1.48    # Northeast corner
    )
  })
  
  # Reactive expression for table tab to filter based on region, planning area and rental price 
  filtered_data_table <- reactive({
    filtered_data <- final_table
    
    # Filter by region
    selected_regions <- input$regionFilter
    if (length(selected_regions) > 0) {
      filtered_data <- filtered_data %>%
        filter(Region %in% selected_regions)
    }
    
    # Filter by planning area
    selected_planningareas <- input$planningAreaFilter
    if (length(selected_planningareas) > 0) {
      filtered_data <- filtered_data %>%
        filter(`Planning Area` %in% selected_planningareas)
    }
    
    # Filter by rental price 
    selected_rental_price_range <- input$rentalPriceRangeFilter  # Get the selected price range
    filtered_data <- filtered_data %>%
      filter(`Rental Price ($)` >= selected_rental_price_range[1] & 
               `Rental Price ($)` <= selected_rental_price_range[2])
    
    # Filter by variables 
    selected_variables <- input$variablesFilter 
    filtered_data <- filtered_data %>% 
      select(c(Street, `Planning Area`, Region, all_of(selected_variables), `Score (upon 100)`))
    
    return(filtered_data)
  })
  
  # Render the table based on filtered data
  output$table <- renderDT({
    datatable(filtered_data_table())  # Render the filtered data as a table
  })
}

shinyApp(ui, server)