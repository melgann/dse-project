# StreetSwiper 

## Set Up Instructions

### Configuration
The API key for the LTA DataMall dataset should be stored in a local R script called api_keys.R where there is only one variable api_key = "your_key_here". Please create such an R script to store your own API key which will be referenced in the code where there is 'source("api_keys.R")'

### Running App 
Step 1: Clone Github Repository 
```bash
git clone https://github.com/melgann/dse-project.git 
cd dse-project 
```

Step 2: Install Necessary Packages to run Shiny App 

Step 3: Run App 


### Data Preparation

To ensure the app generates the most up-to-date scores, download the latest dataset and place it in the designated folder. Follow these steps:

**Step 1**: Ensure that the following Dependencies are installed in your local. 

##### R Package Requirements

```r
install.packages(c(
  "tidyverse","dplyr","data.table","jsonlite","sf","geosphere","dynlm","httr", 
  "rvest","fuzzyjoin","FactoMineR","factoextra", "readr", "readxl", "stringr", "lubridate"      
))

```

##### Python Package Requirements 
```bash
pip install pandas geopandas
````
**Step 2**: Download the Relevant Datasets 

##### Rental Prices 
To get the most current retail rental data:
1. Visit the [URA Retail Rental Analysis portal](https://eservice.ura.gov.sg/property-market-information/pmiCommercialRentalRetailAnalysis)
2. Select **"Retail"** under Commercial Rental Analysis
3. Choose the latest available quarter(s) for the current year
4. Download the dataset in CSV format
5. Save the file to `Raw_datasets`

##### Obtaining Coordinates for New Streets 
To get the coordinates for the newest streets 
1. Go to `Archive\Obtaining_coords_street.ipynb`
2. Update df_q1,df_q2,df_q3,df_q4
 
`
df_q1 = pd.read_csv("CommercialRentalStatsByStreet_YYYYMMDDHHMMSS.csv")  
`

4. Replace the `Bearer` to your own token by signing up to [OneMapApi](https://www.onemap.gov.sg/apidocs/register)
5. Execute cells 1-11 to obtain to new csv with the corresponding Planning Areas and Coordinates of the newly obtained Streets  

##### Public and Private Transport Timings 
1. Go to `Archive\Obtaining_coords_street.ipynb`
2. Replace the `Bearer` to your own token by signing up to [OneMapApi](https://www.onemap.gov.sg/apidocs/register) 
3.  Run code chunk 11 to the end of the file to obtain Public and Private Transport Timings of New Streets 


**Step 3**: Go to `backend_final_code.Rmd` and click `Run all` under `Run` to obtain the latest final scores with the updated datasets!










