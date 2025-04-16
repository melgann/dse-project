library(polite)
library(rvest)
library(tidyverse)
library(shinyWidgets)
library(shinyjs)
library(sortable)
library(leaflet)
library(sf)
library(shiny)
library(DT)
library(leaflet.extras) 
library(scales)
library(shinyalert)

##################################
### Read in Data from Back-end ###
##################################

# 0. Planning Area Map, Street Names and their Coordinates
planning_areas_geom <- st_read("Raw_datasets/district_and_planning_area.geojson") %>%
  rename("Region" = "district",
         "Planning Area" = "planning_area") %>%
  mutate(`Planning Area` = toupper(`Planning Area`)) # to help with joining

street_pa_coord <- read.csv("Cleaned_Datasets/street_name_planning_area.csv") %>%
  select(Street = Street_name,
         `Planning Area` = Planning_Area,
         longitude, latitude)

# 1. Rental Prices
street_rentals <- read.csv("Cleaned_Datasets/rental_prices.csv") %>%
  select(Street,
         `Avg. Rent ($/m²/month)` = avg_median_price)

# 2. Captive Catchment Factors
today <- Sys.Date()
if (as.integer(format(today, "%d")) < 10) {
  month_tag <- format(seq(today, length = 2, by = "-2 month")[2], "%Y-%m")
} else {
  month_tag <- format(seq(today, length = 2, by = "-1 month")[2], "%Y-%m")
}
input_file <- paste0("Cleaned_Datasets/Captive_Catchment/captive_catchment_score_", month_tag, ".csv")

captive_catchment_scores <- read.csv(input_file) %>%
  select(Street = Street_name, 
         captive_catchment_score = captive_catchment)

  # 2.1 Population by Planning Area
  # 2.2 Number of Schools by Planning Area
street_pop_schools <- read.csv("Cleaned_Datasets/streets_malls_school_pop.csv") %>%
  select(Street = Street_name,
         `Population (in thousands)` = total_pop,
         `School Count` = Number_of_Schools)

  # 2.3 Passenger Volume at Nearest Bus Stop
  # 2.4 Passenger Volume at Nearest MRT Station
  # Data not needed for app

# 3. Malls
  # 3.1 Number of Malls within 500m
  # 3.2 Distance to Nearest Mall
  # 3.3 Name of Nearest Mall
street_malls <- read.csv("Cleaned_Datasets/dist_and_no_of_malls.csv") %>% 
  select(Street = StreetName,
         `Nearest Mall` = MallName,
         `Dist. to Nearest Mall (m)` = Distance,
         `Mall Count` = number_of_malls)

# 4. Accessibility
accessibility_scores <- read.csv("Cleaned_Datasets/accessibility_scores.csv") %>%
  select(Street = Street_name, accessibility_score)

# 4.1 Number of MRT Stations within 500m
# 4.2 Distance to Nearest MRT Station
# 4.3 Name of Nearest MRT Station
street_mrt <- read.csv("Cleaned_Datasets/number_of_mrt_near_street.csv") %>%
  select(Street = Street_name,
         `Nearest MRT` = station_name,
         `Dist. to Nearest MRT (m)` = dist_to_nearest_mrt,
         `MRT Count` = number_of_stations)

# 4.4 Number of Bus Stops within 500m
# 4.5 Distance to Nearest Bus Stop
street_bus <- read.csv("Cleaned_Datasets/number_of_bus_stops_near_street.csv") %>%
  select(Street = Street_name,
         `Dist. to Nearest Bus Stop (m)` = dist_to_nearest_bus_stop, 
         `Bus Stop Count` = number_of_bus_stops)

  # 4.6 Public VS Private Transport Travelling Time
  # !!! to combine data set -- why are there different number of rows?
public_transport_timing <- read.csv("Cleaned_Datasets/drive_timing.csv") %>%
  select(Street = Street_name,
         `Origin (Planning Area)` = Planning_area,
         `Travelling Time (Private)` = Time_taken_drive)

private_transport_timing <- read.csv("Cleaned_Datasets/final_public_transport_to_streets_from_planning_area.csv") %>%
  select(Street = Street_name,
         `Origin (Planning Area)` = Planning_area,
         `Travelling Time (Public)` = Time_taken)

######################
### Combining Data ###
######################

data <- street_pa_coord %>%
  left_join(st_drop_geometry(planning_areas_geom), by = "Planning Area") %>%
  left_join(street_rentals, by = "Street") %>%
  left_join(street_pop_schools, by = "Street") %>%
  left_join(street_malls, by = "Street") %>%
  left_join(street_mrt, by = "Street") %>%
  left_join(street_bus, by = "Street") %>%
  left_join(accessibility_scores, by = "Street") %>%
  left_join(captive_catchment_scores, by = "Street") %>%
  mutate(across(c("Street", "Planning Area"), str_to_title)) %>%
  relocate(longitude, latitude, .after = last_col())

planning_areas_geom$`Planning Area` <- str_to_title(planning_areas_geom$`Planning Area`)

################################
### Computing Default Scores ###
################################

# Normalization Function
max_min_normalize <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

data_scoring <- data %>%
  mutate(across(where(is.numeric), max_min_normalize))

weight_captive_catchment_default = 0.379537954
weight_dist_to_nearest_mall_default = 0.2202202202
weight_accessibility_default = 0.2202202202
weight_rental_price_default = 0.180418042

default_score <- (weight_captive_catchment_default*data_scoring$captive_catchment_score
  - weight_dist_to_nearest_mall_default*data_scoring$`Dist. to Nearest Mall (m)`
  + weight_accessibility_default*data_scoring$accessibility_score
  - weight_rental_price_default*data_scoring$`Avg. Rent ($/m²/month)`)

data <- as.data.frame(cbind(data, default_score)) %>%
  mutate(`Score (upon 100)` = round(max_min_normalize(default_score) * 100, 2))

######################
### Vectors for UI ###
######################

# Region and Planning Area Filters
region_planning_area <- data %>%
  select(`Planning Area`, Region) %>% 
  distinct() %>%
  arrange(`Planning Area`)

# Rental Price Range Slider
price_range_min <- floor(min(data$`Avg. Rent ($/m²/month)`)/10) * 10
price_range_max <- ceiling(max(data$`Avg. Rent ($/m²/month)`)/10) * 10
price_range_breaks <- pretty_breaks(n = 20)(c(price_range_min, price_range_max))

# Average values for Metrics
median_values <- c(
  "Avg. Rent ($/m²/month)" =  median(data$`Avg. Rent ($/m²/month)`),
  "Population (in thousands)" = median(data$`Population (in thousands)`),
  "MRT Count" = median(data$`MRT Count`),
  "Bus Stop Count" = median(data$`Bus Stop Count`),
  "School Count" = median(data$`School Count`),
  "Mall Count" = median(data$`Mall Count`)
)
# Metric descriptions for comparison tab
metric_descriptions <- list(
  "Avg. Rent ($/m²/month)" = "Average monthly rental price of retail properties on street",
  "Population (in thousands)" = "Population in street's planning area",
  "MRT Count" = "Number of MRT stations within 500m from street",
  "Bus Stop Count" = "Number of bus stops within 250m from street",
  "School Count" = "Number of schools in street's planning area",
  "Mall Count" = "Number of malls within 500m from street"
)

# default weights for importance sliders
normalized_default_weights <- c(weight_rental_price_default,
                                weight_accessibility_default,
                                weight_captive_catchment_default,
                                weight_dist_to_nearest_mall_default)

multiplier <- 10/max(normalized_default_weights)
default_weights <- normalized_default_weights*multiplier
def_importance_rent <- round(default_weights[1], 0)
def_importance_access <- round(default_weights[2], 0)
def_importance_captive_catchment <- round(default_weights[3], 0)
def_importance_mall_dist <- round(default_weights[4], 0)

#################
### Shiny App ###
#################

ui <- fluidPage(
  
  useShinyjs(),
  
  #################
  ### CSS Stuff ###
  #################
  tags$head(
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css"),
    tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/bootstrap-icons@1.10.5/font/bootstrap-icons.css"),
  
    tags$style(HTML("
      .street-card {
        position: relative;
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 15px;
        margin-bottom: 15px;
        background-color: #f9f9f9;
        box-shadow: 1px 1px 5px rgba(0,0,0,0.05);
        cursor: pointer;
      }
      
      .street-card.selected-card {
        border: 2px solid #007bff;
        background-color: #eef7ff;
        box-shadow: 0 0 10px rgba(0,123,255,0.4);
      }
      
      .street-card h4 {
        margin: 0 0 5px;
        color: #333;
      }
      
      .bookmark-icon {
        position: absolute;
        top: 15px;
        right: 20px;
        cursor: pointer;
        font-size: 18px;
        color: #333;
      }
      
      .bookmark-icon:hover i {
        color: #2F80BF;
      }
      
      .bookmark-icon i.fas {
        color: #2F80BF;
      }
      
      .region-badge {
        padding: 2px 8px;
        border-radius: 10px;
        font-size: 12px;
        font-weight: normal;
        color: white;
        background-color: #a1a1a1;
        margin-left: 0px; 
      }
      
      .pa-badge {
        padding: 2px 8px;
        border-radius: 10px;
        font-size: 12px;
        font-weight: normal;
        color: white;
        background-color: #a1a1a1;
        margin-left: 2px; 
      }
       
      .score-badge {
        padding: 2px 8px;
        border-radius: 10px;
        font-size: 12px;
        font-weight: normal;
        color: white;
        margin-left: 2px;
      }
      
      .score-badge.high { background-color: #2ecc71; }  /* Green */
      .score-badge.medium { background-color: #f1c40f; } /* Yellow */
      .score-badge.low { background-color: #e74c3c; }    /* Red */
      
      .toggle-details {
        font-size: 13px;
        color: #2F80BF;
        cursor: pointer;
        display: block;
        margin-top: 8px;
      }
      
      .custom-button {
        background-color: #f8f9fa;
        color: black;
        border: none;
        padding: 10px 20px;
        font-size: 14px;
        font-weight: bold;
        border: 1px solid #ccc;
        border-radius: 5px;
        white-space: normal;
        word-wrap: break-word;
        width: 100%;
      }
      
      .custom-button:hover {
        background-color: #5B9BD5;
      }
      
      .slider-label {
        font-size: 12px !important;
      }
      
      .pill-label {
        display: inline-block;
        padding: 4px 10px;
        border-radius: 50px;
        font-size: 13px;
        font-weight: 600;
        color: white;
        margin-top: 5px;
      }
      
      .badge-pill-high { background-color: #28a745; } /* Bootstrap green */
  
      .badge-pill-low { background-color: #dc3545; } /* Bootstrap red */
      
      .btn:disabled {
        background-color: #e0e0e0 !important;
        color: #888888 !important;
        border-color: #cccccc !important;
        cursor: not-allowed;
        opacity: 0.65;
      }
      
      .shiny-output-error-validation {
        color: grey;
        text-align: center;
        margin-top: 20px;
        white-space: normal;
      }
    ")),
    
    tags$style(
      type = "text/css",
      ".irs-grid-pol {height: 5px;}",
      ".irs-grid-pol.small {height: 0px;}"
    ),
    
    tags$script(HTML("
    
      $(document).on('mouseenter', '[data-toggle=\"tooltip\"]', function () {
        if (!$(this).data('bs.tooltip')) {
          $(this).tooltip();
          $(this).tooltip('show');
        }
      });
    
      $(document).on('click', '.bookmark-icon', function(e) {
        var icon = $(this).find('i');
    
        // Toggle icon class between far and fas
        if (icon.hasClass('far')) {
          icon.removeClass('far').addClass('fas');
        } else {
          icon.removeClass('fas').addClass('far');
        }
    
        // Send to Shiny
        const street = $(this).closest('.street-card').data('street');
        const isSaved = icon.hasClass('fas');
        Shiny.setInputValue('bookmark_toggle', {
          street: street,
          saved: isSaved,
          nonce: Math.random()
        });
      });
      
      function removeStreet(streetName) {
        Shiny.setInputValue('remove_street', {
          name: streetName,
          nonce: Math.random()
        });
      }
  
      // Toggle visibility of details section
      $(document).on('click', '.toggle-details', function(e) {
        e.preventDefault();
        $(this).next('.street-details').slideToggle();
      });
    
      function centreOnStreet(elem) {
        var lat = $(elem).data('lat');
        var lng = $(elem).data('lng');
        var street = $(elem).data('street');
    
        // Send to Shiny
        Shiny.setInputValue('clicked_street_coords', {
          lat: lat,
          lng: lng,
          name: street,
          nonce: Math.random()  // ensures it's always a new value
        });
  
        // Highlight the clicked card
        $('.street-card').removeClass('selected-card');
        $(elem).addClass('selected-card');
      }
    "))
  ),
  
  ########################
  ### End of CSS Stuff ###
  ########################
  
# ------- title and intro --------- #
  div(
    style = "display: flex; align-items: flex-end; margin-bottom: 20px;",

    tags$img(
      src = "logo.png",
      alt = "StreetSwiper Logo",
      style = "max-height: 100px; margin-right: 25px;"
    ),
    
    tags$div(
      style = "font-size: 16px; font-weight: 500; line-height: 1.5;",
      HTML("Discover the most strategic location to start your business.<br>"),
      span("Make smarter decisions with our smart scoring system."),
      actionLink("toggle_button", "More Details", style = "color: #007bff; font-weight: 400; margin-left: 6px;")
    )
  ),  

# ----------- side bar panel --------------- #
  
  fluidRow(
    column(width = 3,
           
      ## -------------- filters section for Map and Table -------------- ##
      
      conditionalPanel(
        condition = "input.mainTabs == 'Map' || input.mainTabs == 'Table'", 
        div(
          style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; background-color: #f8f9fa;",
          
          h3("Filters", style = "margin-top: 0px;"),
          
          pickerInput(
            inputId = "regionFilter",
            label = h4("Region", style = "margin-bottom: 4px"), 
            choices = sort(unique(data$Region)),
            multiple = TRUE,
            options = pickerOptions(container = "body",
                                    title = "Select",
                                    actionsBox = TRUE)
          ),
          
          helpText("To further narrow down streets, please select planning areas below."),
          
          pickerInput(
            inputId = "planningAreaFilter",
            label = h4("Planning Area", style = "margin-bottom: 4px"), 
            choices = NULL,  # updates accordingly to user's input in regionFilter (see observe in server)
            multiple = TRUE,
            options = pickerOptions(
              container = "body",
              title = "Select",
              selectedTextFormat = "count > 3",
              actionsBox = TRUE,
              liveSearch = TRUE
            )
          ),
          
          sliderTextInput(
            inputId = "rentalPriceRangeFilter",
            label = HTML('<h4>Rental Price Range <span style="font-weight: normal; font-size: 14px;">($/sqm/month)</span></h4>'),
            choices = price_range_breaks,
            selected = c(price_range_breaks[2], 
                         price_range_breaks[length(price_range_breaks) - 1]),
            grid = TRUE
          )
        )
      ),
      
      ## -------------- metrics selection section for Comparison -------------- ##
      
      conditionalPanel(
        condition = "input.mainTabs == 'Comparison'", 
        div(
          style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; background-color: #f8f9fa;",
          
          checkboxGroupInput(
            inputId = "variableSelectionFilter", 
            label = tagList(
              h3("Select Metrics to Compare", style = "margin-top: 0px;"),
              uiOutput("variableSelectionFilterDeactivatedMessage")
            ),
            choices = list("Rental Price ($ per m²)" = "Avg. Rent ($/m²/month)",
                           "Population (in thousands)" = "Population (in thousands)",
                           "MRT Count" = "MRT Count",
                           "Bus Stop Count" = "Bus Stop Count",
                           "School Count" = "School Count",
                           "Mall Count" = "Mall Count"),
            selected = c("Avg. Rent ($/m²/month)", 
                         "Population (in thousands)", 
                         "MRT Count", 
                         "Bus Stop Count", 
                         "School Count",
                         "Mall Count")
          ),

          actionButton("toggleThirdStreet", label = "+ Add Street", class = "btn btn-outline-primary", width = "150px"),
          
          uiOutput("toggleThirdStreetDeactivatedMessage"),
          
          tags$hr(),
          
          tags$div(
            style = "font-size: 13px; margin-top: 10px;",
            
            tags$p(tags$b("Legend:"), style = "margin-bottom: 4px;"),
            
            tags$ul(style = "padding-left: 15px;",
                    tags$li(
                      tags$span(class = "pill-label badge-pill-high", "High"),
                      " = desirable (e.g., high MRT count, cheaper rent)"
                    ),
                    tags$li(
                      tags$span(class = "pill-label badge-pill-low", "Low"),
                      " = less desirable (e.g., low mall count, more expensive rent)"
                    )
            )
          )
        )
      ),
      
      ## -------------- saved streets section for all -------------- ##
      
      div(
        style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; margin-bottom: 10px; background-color: #f8f9fa; ",
        
        h3("Saved Streets", style = "margin-top: 0px;"),
        
        uiOutput("numberOfSavedStreets", style = "margin-top: 0px; color: grey;"),
        
        div(style = "max-height: 120px; overflow-y: auto;", 
            uiOutput("savedStreetCards"))
      ),
      
      ## -------------- our scoring system section for Map and Table -------------- ##
      
      conditionalPanel(
        condition = "input.mainTabs == 'Map' || input.mainTabs == 'Table'",
        
        div(
          style = "border: 1px solid #ccc; padding: 10px; border-radius: 5px; background-color: #f0f3ff;",
          
          h3("Our Scoring System", style = "color: #001675; margin-top: 0px;"),
          
          # Explanation for final scores
          tags$p("Each street is assigned a score based on factors such as
                 average rental cost, accessibility, captive catchment, and distance to the nearest mall.
                 Higher-ranked streets reflect better suitability based on your preferences.",
                 style = "font-size: 13px; color: #555; margin-bottom: 10px;"),
          
          # Checkbox and info icon in the same row
          tags$div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 0px; padding-bottom: 0px;",
                   checkboxInput("useCustomWeights", 
                                 label = "I want to adjust the scores based on my preferences", 
                                 value = FALSE),
                   tags$i(
                     class = "bi bi-info-circle-fill",
                     style = "color: #666; cursor: pointer;",
                     `data-toggle` = "tooltip",
                     `data-placement` = "top",
                     title = "By default, location scores reflect weightings based on retail industry research."
                   )
          ),
          
          ## -------------- custom weights section for Map and Table -------------- ##
          
          conditionalPanel(
            condition = "input.useCustomWeights == true",
            
            sliderTextInput(
              inputId = "rentalCostWeight",
              label = span(class = "slider-label", "Rental Cost"), 
              choices = 1:10,
              selected = def_importance_rent,
              grid = TRUE
            ), 
            
            sliderTextInput(
              inputId = "accessibilityWeight",
              label = span(class = "slider-label", "Accessibility"), 
              choices = 1:10,
              selected = def_importance_access,
              grid = TRUE
            ), 
            
            sliderTextInput(
              inputId = "captiveCatchmentWeight",
              label = span(class = "slider-label", "Captive Catchment"), 
              choices = 1:10,
              selected = def_importance_captive_catchment,
              grid = TRUE
            ),
            
            sliderTextInput(
              inputId = "distToMallWeight",
              label = span(class = "slider-label", "Dist. to Nearest Mall"), 
              choices = 1:10,
              selected = def_importance_mall_dist,
              grid = TRUE
            )
          )
        )
      )
    ),
    
# ------------ main panel with tabs --------------- #
    
    column(width = 9,
           
           tabsetPanel(id = "mainTabs",
             tabPanel(
               "Map",
               fluidRow(
                 column(width = 8, leafletOutput("map", height = 600)),
                 column(width = 4,
                        h3("Filtered Streets"),
                        
                        uiOutput("hasFilteredStreets", style = "margin-top: 0px; color: grey;"),
                        
                        div(
                          style = "height: 600px; overflow-y: auto; padding-right: 10px;",
                          uiOutput("filteredStreets")
                        ))
                 )
             ),
             
             tabPanel(
               "Table",
               DT::DTOutput("table")
             ),
             
             tabPanel(
               "Comparison", 
               uiOutput("streetPickerRow"),
               
               div(
                 style = "height: 600px; overflow-y: auto;",
                 fluidRow(uiOutput("comparisonDetails"))
               )
             )
      
          )
    )
  )
)

server <- function(input, output, session) {
  
  # Action to show the pop-up when the button is clicked
  observeEvent(input$toggle_button, {
    shinyalert(
      title = "More Details",  # Title of the pop-up
      text = HTML(
        "<style>
        /* Ensures all text within the modal is black */
        .shinyalert-container p,
        .shinyalert-container li,
        .shinyalert-container ul,
        .shinyalert-container div {
          color: #000000 !important;
        }
        /* Override the background and ensure the text remains black */
        .shinyalert-container {
          color: #000000 !important;
        }
        /* Additional styling for list items */
        .shinyalert-container ol li {
          color: #000000 !important; /* Ensure li items are black */
        }
        /* Override any text elements */
        .shinyalert-container * {
          color: #000000 !important;
        }
        </style>
        <div style='font-size:16px; text-align:left;'>
          <p>Our app is designed to help business owners identify the most suitable locations to open a business using a data-driven scoring system. We have identified the streets that have properties available for rent for retail purposes.</p>
          
          <p>&nbsp;</p> <!-- Empty line -->
          
          <p>The score for each street is calculated based on multiple key variables, each variable assigned a weight representing its importance. The variables used in the scoring system are:</p>
          
          <p>&nbsp;</p> <!-- Empty line -->
          
          <ol>
            <li>Average Monthly Rental Price of Retail Properties on Street</li>
            <p>&nbsp;</p> <!-- Empty line -->
            
            <li>Captive Catchment
              <ul>
                <li>Population in street’s planning area</li>
                <li>Number of schools in street’s planning area</li>
                <li>Passenger volume at the nearest MRT station</li>
                <li>Passenger volume at the nearest bus stop</li>
              </ul>
            </li>
            <p>&nbsp;</p> <!-- Empty line -->
            
            <li>Distance to Nearest Mall</li>
            <p>&nbsp;</p> <!-- Empty line -->
            
            <li>Accessibility
              <ul>
                <li>Distance to nearest MRT station</li>
                <li>Number of MRT stations within 500m from street</li>
                <li>Distance to nearest bus stop</li>
                <li>Number of bus stops within 250m from street</li>
                <li>Difference in public vs private transportation travel time from all planning areas to the street</li>
              </ul>
            </li>
          </ul>
        </ol>
          
        <p>&nbsp;</p> <!-- Empty line -->
          
        <p>Each variable is influenced by a set of detailed factors to provide a more accurate and localized analysis. The scoring is calculated by multiplying each variable by its assigned weight, giving users a clear and customized ranking of potential locations.</p>
        </div>"
      ),
      size = "m",  # Medium-sized pop-up
      closeOnClickOutside = TRUE,  # Allows the user to close the pop-up by clicking outside
      html = TRUE,  # Enable HTML content inside the modal
      showConfirmButton = TRUE,  # Show "OK" button
      confirmButtonText = "OK",  # Text for the button
      confirmButtonCol = "#0056b3" # Darker blue color for the OK button
    )
  })
  
  ## -------------- update planningAreaFilter options based on input$regionFilter -------------- ##
  
  observe({
    selected_regions <- input$regionFilter
    
    if (is.null(selected_regions) || length(selected_regions) == 0) {
      updatePickerInput(session, "planningAreaFilter",
                        choices = split(region_planning_area$`Planning Area`, region_planning_area$Region))
    } else {
      filtered_planning_areas <- region_planning_area %>%
        filter(Region %in% selected_regions)
      
      updatePickerInput(session, "planningAreaFilter",
                        choices = split(filtered_planning_areas$`Planning Area`,
                                        filtered_planning_areas$Region))
    }
  }) 
  
  ## -------------- computing final scores (both default or custom) -------------- ##
  
  score_computed_data <- reactive({
    req(data)
    
    # Define weights
    if (isTRUE(input$useCustomWeights)) {
      # Get weights from sliders (1 to 10)
      custom_weights <- c(input$rentalCostWeight,
                          input$accessibilityWeight,
                          input$captiveCatchmentWeight,
                          input$distToMallWeight)
      
      normalized_weights <- max_min_normalize(custom_weights)
    } else {
      normalized_weights <- normalized_default_weights
    }
    
    # Extract individual weights
    weight_captive_catchment <- normalized_weights[3]
    weight_dist_to_nearest_mall <- normalized_weights[4]
    weight_accessibility <- normalized_weights[2]
    weight_rental_price <- normalized_weights[1]
    
    # Compute custom score
    custom_score <- (weight_captive_catchment*data_scoring$captive_catchment_score
                      - weight_dist_to_nearest_mall*data_scoring$`Dist. to Nearest Mall (m)`
                      + weight_accessibility*data_scoring$accessibility_score
                      - weight_rental_price*data_scoring$`Avg. Rent ($/m²/month)`)
    
    data %>%
      mutate(`Score (upon 100)` = round(max_min_normalize(custom_score) * 100, 2))
  })
  
  ## -------------- filter data based on input filters -------------- ##
  
  filtered_data <- reactive({
    df <- score_computed_data()
    
    # Region filter
    if (!is.null(input$regionFilter) && length(input$regionFilter) > 0) {
      df <- df %>% filter(`Region` %in% input$regionFilter)
    }
    
    # Planning Area filter
    if (!is.null(input$planningAreaFilter) && length(input$planningAreaFilter) > 0) {
      df <- df %>% filter(`Planning Area` %in% input$planningAreaFilter)
    }
    
    # Rental price range
    if (!is.null(input$rentalPriceRangeFilter)) {
      df <- df %>%
        filter(`Avg. Rent ($/m²/month)` >= input$rentalPriceRangeFilter[1],
               `Avg. Rent ($/m²/month)` <= input$rentalPriceRangeFilter[2])
    }
    
    return(df)
  })
  
  ## -------------- map output -------------- ##
  
  output$map <- renderLeaflet({
    filtered_data_map <- filtered_data() %>% 
      select(`Planning Area`, `Score (upon 100)`, `Avg. Rent ($/m²/month)`) %>% 
      group_by(`Planning Area`) %>% 
      summarise(
        `Median Score (upon 100)` = median(`Score (upon 100)`, na.rm = TRUE)) %>% 
      left_join(planning_areas_geom, by = "Planning Area")
    
    # Ensure it's an sf object (with geometry column)
    if (!inherits(filtered_data_map, "sf")) {
      filtered_data_map <- st_as_sf(filtered_data_map)
    }
    
    # Define color palette based on Median Score
    pal <- colorNumeric(
      palette = "YlOrRd", 
      domain = filtered_data_map$`Median Score (upon 100)`
    )
    
    leaflet(filtered_data_map) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        data = filtered_data_map,
        color = "black",
        weight = 1,
        fillColor = ~pal(`Median Score (upon 100)`),
        fillOpacity = 0.4,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "black",
          fillOpacity = 0.4,
          bringToFront = TRUE
        ),
        label = ~paste0(`Region`, ": ", `Planning Area`),
        popup = ~paste0("<b>Planning Area:</b> ", `Planning Area`, "<br>",
                        "<b>Median Score (upon 100):</b> ", `Median Score (upon 100)`)
      ) %>%
      addLegend(
        "bottomright",                        
        pal = pal,                            
        values = filtered_data_map$`Median Score (upon 100)`,  
        title = "Median Score (100)",       
        opacity = 0.4
      ) %>%
      setView(lng = 103.815539, lat = 1.346594, zoom = 12) %>%
      setMaxBounds(
        lng1 = 103.6, lat1 = 1.15,   # Southwest corner
        lng2 = 104.1, lat2 = 1.48    # Northeast corner
    )
  })
  
  ## -------------- filtered streets output -------------- ##
  
  output$hasFilteredStreets <- renderUI({
    filtered_data <- filtered_data()
    
    if (!(is.null(input$planningAreaFilter) & is.null(input$regionFilter)) &
        nrow(filtered_data) > 0)
      tags$p(paste(nrow(filtered_data), "Streets · Sorted by score (highest to lowest)"))
  })
  
  output$filteredStreets <- renderUI({
    
    if(is.null(input$planningAreaFilter) & is.null(input$regionFilter))
       return("Please apply Region and/or Planning Area filter.")
    
    filtered_data <- filtered_data() %>%
      arrange(desc(`Score (upon 100)`))
    
    if (nrow(filtered_data) == 0)
      return("No streets match your filters.")
    
    tagList(
      
      lapply(1:nrow(filtered_data), function(i) {
        street <- filtered_data[i, ]
        
        # Determine score badge class
        score <- round(street$`Score (upon 100)`, 1)
        badge_class <- if (score >= 80) "high" else if (score >= 60) "medium" else "low"
        icon_class <- if (street$Street %in% saved_streets()) "fas fa-bookmark" else "far fa-bookmark"
        
        tags$div(
          class = "street-card",
          `data-street` = street$Street,
          `data-lat` = street$latitude,
          `data-lng` = street$longitude,
          onclick = "centreOnStreet(this)",
          
          tags$h4(style = "margin-bottom: 2px;", street$Street), 

          tags$div(class = "bookmark-icon",
                   tags$i(class = icon_class)),
          
          tags$span(class = "badge region-badge",
                    street$Region),
          
          tags$span(class = "badge pa-badge",
                    street$`Planning Area`),
          
          tags$span(class = paste("badge", "score-badge", badge_class), 
                    paste("Score:", score)),

          tags$p(tags$span("Nearest Mall:", style = "font-weight: 500;"),
                 street$`Nearest Mall`,
                 tags$span(paste0("(", round(street$`Dist. to Nearest Mall (m)`, 0), "m away)"), style = "color: #999999;"),
                 style = "margin-top: 10px; margin-bottom: 4px;"
                 ),
          
          tags$p(tags$span("Nearest MRT:", style = "font-weight: 500;"),
                 street$`Nearest MRT`,
                 tags$span(paste0("(", round(street$`Dist. to Nearest MRT (m)`, 0), "m away)"), style = "color: #999999;"),
                 style = "margin-bottom: 4px;"
                 ),
          
          tags$p(tags$span("Average Rental Price:", style = "font-weight: 500;"),
                 paste0("$", round(street$`Avg. Rent ($/m²/month)`, 2), "/m²/month")
            ),
        
          tags$a("More info ▼", href = "#", class = "toggle-details"),
          
          tags$div(
            class = "street-details",
            style = "display: none;",
            tags$p(tags$span("No. of MRT within 500m:", style = "font-weight: 500;"),
                   street$`MRT Count`,
                   style = "margin-bottom: 4px;"
            ),
            tags$p(tags$span("No. of Bus Stops within 250m:", style = "font-weight: 500;"),
                   street$`School Count`,
            ),
          )
        )
      })
    )
  })
  
  ## -------------- update map view based on input filters -------------- ##
  
  observe({
    streets <- filtered_data()
    
    if (nrow(streets) == 0) return()
    
    leafletProxy("map") %>%
      clearGroup("streetMarkers") %>%
      addMarkers(
        clusterOptions = markerClusterOptions(),
        lng = streets$longitude,
        lat = streets$latitude,
        label = streets$Street,
        group = "streetMarkers"
      ) %>%
      fitBounds(
        lng1 = min(streets$longitude, na.rm = TRUE),
        lat1 = min(streets$latitude, na.rm = TRUE),
        lng2 = max(streets$longitude, na.rm = TRUE),
        lat2 = max(streets$latitude, na.rm = TRUE)
      )
  })
  
  ## -------------- update map view based on clicked street -------------- ##
  
  observeEvent(input$clicked_street_coords, {
    leafletProxy("map") %>%
      clearGroup("highlightedStreet") %>%
      addCircleMarkers(
        lng = input$clicked_street_coords$lng,
        lat = input$clicked_street_coords$lat,
        radius = 12,
        fillColor = "blue",
        color = "blue",
        fillOpacity = 1,
        label = input$clicked_street_coords$name,
        group = "highlightedStreet"
      ) %>%
      setView(
        lng = input$clicked_street_coords$lng,
        lat = input$clicked_street_coords$lat,
        zoom = 16
      )
  })
  
  ## -------------- saved_streets -------------- ##
  
  # initialize saved_streets
  saved_streets <- reactiveVal(character(0))
  
  # update saved_streets based on bookmark toggle 
  observeEvent(input$bookmark_toggle, {
    current <- saved_streets()
    if (input$bookmark_toggle$saved) {
      saved_streets(unique(c(current, input$bookmark_toggle$street)))
    } else {
      saved_streets(setdiff(current, input$bookmark_toggle$street))
    }
  })
  
  # update saved_streets based on remove (trash can) toggle
  observeEvent(input$remove_street, {
    current <- saved_streets()
    saved_streets(setdiff(current, input$remove_street$name))
  })
  
  # saved_streets output
  output$savedStreetCards <- renderUI({
    saved_streets <- saved_streets()
    
    if (length(saved_streets) == 0)
      return("No streets saved yet.")
    
    tagList(
      lapply(saved_streets(), function(street_name) {
        street <- data %>% filter(Street == street_name)
        tags$div(
          class = "street-card",
          style = "padding: 8px; padding-bottom: 0px; margin-bottom: 4px;",
          tags$p(street$Street),
          tags$div(
            class = "delete-icon",
            style = "position: absolute; top: 10px; right: 10px; cursor: pointer;",
            tags$i(class = "fas fa-trash", 
                   onclick = sprintf("removeStreet('%s')", street$Street))
          )
        )
      })
    )
  })
  
  # number of saved_streets output
  output$numberOfSavedStreets <- renderUI({
    num <- length(saved_streets())
    if (num == 1) {
      tags$p("1 street saved")
    } else if (num > 1) {
      tags$p(paste(num, "streets saved"))
    }
  })
  
  ## -------------- table output -------------- ##
  
  output$table <- renderDT({
    datatable(
      filtered_data() %>% 
        select(Street, 
               `Score (upon 100)`, 
               everything(), 
               -longitude, 
               -latitude, 
               -accessibility_score, 
               -captive_catchment_score, 
               -default_score) %>%
        rename(`Score<br>(upon 100)` = `Score (upon 100)`,
               `Population<br>(in thousands)` = `Population (in thousands)`,
               `Mall Count<br>(within 500m)` = `Mall Count`,
               `MRT Count<br>(within 500m)` = `MRT Count`,
               `Bus Stop Count<br>(within 250m)` = `Bus Stop Count`),
      extensions = c('Buttons', 'FixedColumns'),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = TRUE,
        fixedColumns = list(leftColumns = 2),
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'colvis', columns = c(2:14),
               text = 'Hide/Show More Columns')
        ), 
        columnDefs = list(
          list(visible = FALSE, targets = c(5, 6, 8, 9, 11, 13))  
        )
      ),
      escape = FALSE,
      class = 'stripe hover nowrap'
    )
  })
  
  ## -------------- showThirdStreet logic -------------- ##
  
  # Track whether the third street input should be shown
  rv <- reactiveValues(showThirdStreet = FALSE)
  
  # Toggle state when button is clicked
  observeEvent(input$toggleThirdStreet, {
    rv$showThirdStreet <- !rv$showThirdStreet
    
    # Dynamically change button label too
    updateActionButton(
      session,
      "toggleThirdStreet",
      label = if (rv$showThirdStreet) "– Remove Street" else "+ Add Street"
    )
  })
  
  output$showThirdStreet <- reactive({
    rv$showThirdStreet
  })
  
  outputOptions(output, "showThirdStreet", suspendWhenHidden = FALSE)
  
  observe({
    toggle_state <- length(saved_streets()) >= 3
    shinyjs::toggleState("toggleThirdStreet", condition = toggle_state)
  })
  
  observeEvent(saved_streets(), {
    if (length(saved_streets()) < 3 && rv$showThirdStreet) {
      rv$showThirdStreet <- FALSE
      updateActionButton(session, "toggleThirdStreet", label = "+ Add Street")
    }
  })
  
  output$toggleThirdStreetDeactivatedMessage <- renderUI({
    if (length(saved_streets()) < 3) {
      tags$p("Please save at least 3 streets in the 'Map' tab to add a third street for comparison.", 
             style = "color: grey; margin-top: 5px;")
    }
  })
  
  ## -------------- streetPickerRow output -------------- ##
  
  output$streetPickerRow <- renderUI({
    current_saved <- saved_streets()
    
    # default options
    empty_choices <- c("Save at least 2 streets in 'Map'" = "")
    
    choices_1_2 <- if (length(current_saved) >= 2) current_saved else empty_choices
    choices_3    <- if (length(current_saved) >= 3) current_saved
    
    selected1 <- if (length(current_saved) >= 2) current_saved[1] else ""
    selected2 <- if (length(current_saved) >= 2) current_saved[2] else ""
    selected3 <- if (length(current_saved) >= 3) current_saved[3] else ""
    
    if (rv$showThirdStreet) {
      fluidRow(
        column(4, align = "center",
               pickerInput("street1", h4("Street 1", style = "margin-top: 10px;"),
                           choices = choices_1_2, selected = selected1,
                           options = pickerOptions(container = "body", title = "Select", liveSearch = TRUE))),
        column(4, align = "center",
               pickerInput("street2", h4("Street 2", style = "margin-top: 10px;"),
                           choices = choices_1_2, selected = selected2,
                           options = pickerOptions(container = "body", title = "Select", liveSearch = TRUE))),
        column(4, align = "center",
               pickerInput("street3", h4("Street 3", style = "margin-top: 10px;"),
                           choices = choices_3, selected = selected3,
                           options = pickerOptions(container = "body", title = "Select", liveSearch = TRUE)))
      )
    } else {
      fluidRow(
        column(6, align = "center",
               pickerInput("street1", h4("Street 1", style = "margin-top: 10px;"),
                           choices = choices_1_2, selected = selected1,
                           options = pickerOptions(container = "body", title = "Select", liveSearch = TRUE))),
        column(6, align = "center",
               pickerInput("street2", h4("Street 2", style = "margin-top: 10px;"),
                           choices = choices_1_2, selected = selected2,
                           options = pickerOptions(container = "body", title = "Select", liveSearch = TRUE)))
      )
    }
  })
  
  comparison_data <- reactive({
    list(
      data1 = data %>% filter(Street == input$street1),
      data2 = data %>% filter(Street == input$street2),
      data3 = if (isTRUE(rv$showThirdStreet) && input$street3 != "") {
        filter(data, Street == input$street3)
      } else {
        NULL
      }
    )
  })
  
  observe({
    if (length(saved_streets()) < 2) {
      shinyjs::disable("variableSelectionFilter")
    } else {
      shinyjs::enable("variableSelectionFilter")
    }
  })
  
  output$variableSelectionFilterDeactivatedMessage <- renderUI({
    if (length(saved_streets()) < 2) {
      tags$p("Please save at least 2 streets in the 'Map' tab to enable this section.", 
             style = "color: grey; font-weight: 400; margin-top: 5px;")
    }
  })
  
  ## -------------- comparisonDetails output -------------- ##
  
  output$comparisonDetails <- renderUI({
    
    req(input$street1, input$street2, input$variableSelectionFilter)
    
    if (isTRUE(rv$showThirdStreet)) {
      req(input$street3)
      
      validate(
        need(
          input$street1 != input$street2 &&
            input$street1 != input$street3 &&
            input$street2 != input$street3,
          "Please select 3 different streets."
        )
      )
    } else {
      validate(
        need(
          input$street1 != input$street2,
          "Please select 2 different streets."
        )
      )
    }
    
    metrics <- input$variableSelectionFilter
    show_icons <- input$show_icons
    data1 <- comparison_data()$data1
    data2 <- comparison_data()$data2
    data3 <- comparison_data()$data3
    
    # Define icon class for each metric (Bootstrap Icons)
    metric_icon_classes <- list(
      "Avg. Rent ($/m²/month)" = "bi-cash-coin", 
      "Population (in thousands)" = "bi-people-fill",
      "MRT Count" = "bi-train-front",
      "Bus Stop Count" = "bi-bus-front",
      "School Count" = "bi-book",
      "Mall Count" = "bi-handbag"
    )
    
    # Reusable block for each metric & street
    render_metric_block <- function(value, metric) {
      icon_class <- metric_icon_classes[[metric]]
      
      # Ensure value is not NULL or NA
      if (is.null(value) || is.na(value)) {
        value <- 0  # Or handle as appropriate
      }
      
      median_value <- median_values[metric]
      
      # Ensure median_value is not NULL or NA
      if (is.null(median_value) || is.na(median_value)) {
        median_value <- 0  # Or handle as appropriate
      }
      
      # Highlight metrics green if equal to or above median and red if below median
      is_high <- if (metric == "Avg. Rent ($/m²/month)") {
        value < median_value  # lower rent = better
      } else {
        value >= median_value
      }
      
      # Assign badge class
      badge_class <- if (is_high) "badge-pill-high" else "badge-pill-low"
      badge_label <- if (is_high) "High" else "Low"
      
      tags$div(style = "text-align: center; margin-bottom: 10px;",
               tags$i(class = paste("bi", icon_class), style = "font-size: 24px; margin-bottom: 5px; display: block;"),
               tags$span(class = paste("pill-label", badge_class), round(value, 2))
      )
    }
    
    ui_list <- lapply(metrics, function(metric) {
      # Set the column width depending on rv$showThirdStreet
      column_width <- if (isTRUE(rv$showThirdStreet)) 4 else 6
      
      # Create the first two columns for street1 and street2
      row_content <- list(
        column(column_width, render_metric_block(data1[[metric]], metric)),
        column(column_width, render_metric_block(data2[[metric]], metric))
      )
      
      # Add third column if showThirdStreet is TRUE
      if (isTRUE(rv$showThirdStreet)) {
        row_content <- append(row_content, list(column(4, render_metric_block(data3[[metric]], metric))))
      }
      
      tags$div(
        style = "margin-bottom: 30px;",
        
        # Divider row with metric label + help icon
        tags$div(style = "display: flex; align-items: center; text-align: center; margin: 20px 0;",
                 tags$hr(style = "flex-grow: 1; border-top: 1px solid #ccc;"),
                 
                 tags$span(style = "padding: 0 10px; font-weight: bold; display: flex; align-items: center; gap: 6px;",
                           metric,
                           tags$i(
                             class = "bi bi-info-circle-fill",
                             style = "color: #666; cursor: pointer;",
                             `data-toggle` = "tooltip",
                             `data-placement` = "top",
                             title = metric_descriptions[[metric]]
                           )
                 ),
                 
                 tags$hr(style = "flex-grow: 1; border-top: 1px solid #ccc;")
        ),
        
        # Actual metric row
        fluidRow(row_content)
      )
      
      
    })
    
    do.call(tagList, args = ui_list)
  })
  
}

shinyApp(ui, server)
