library(httr)
library(jsonlite)
library(dplyr)

# Function to query SDWIS API for water systems by state
get_state_water_systems <- function(state_code, 
                                  activity_status = "A",  # A for Active, I for Inactive
                                  output_dir = "output",
                                  save_format = c("both", "csv", "rds", "none")) {
  
  # Match save_format argument
  save_format <- match.arg(save_format)
  
  # Validate state code
  state_code <- toupper(state_code)
  if (nchar(state_code) != 2) {
    stop("State code must be a 2-letter abbreviation (e.g., 'AK', 'CA')")
  }
  
  # Base URL for SDWIS REST Services
  base_url <- "https://data.epa.gov/efservice/WATER_SYSTEM"
  
  # Parameters for query
  query_params <- list(
    paste0("PRIMACY_AGENCY_CODE/=/", state_code),
    paste0("PWS_ACTIVITY_CODE/=/", activity_status),
    "output/JSON"
  )
  
  # Rest of function remains the same...
} 