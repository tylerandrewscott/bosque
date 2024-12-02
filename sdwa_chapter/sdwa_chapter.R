library(httr)
library(jsonlite)
library(dplyr)

# Function to query SDWIS API for water systems by state
get_state_water_systems <- function(state_code, 
                                  activity_status = "A",  # A for Active, I for Inactive
                                  pws_type_code = NULL,   # C (Community), NC (Non-Community), NTNC (Non-Transient Non-Community)
                                  output_dir = NULL,      # NULL means no directory specified
                                  save_format = c("both", "csv", "rds", "none"),
                                  CLOBBER = FALSE) {      # Add CLOBBER parameter
  
  # Match save_format argument
  save_format <- match.arg(save_format)
  
  # Validate state code
  state_code <- toupper(state_code)
  if (nchar(state_code) != 2) {
    stop("State code must be a 2-letter abbreviation (e.g., 'AK', 'CA')")
  }
  
  # Handle output directory
  if (is.null(output_dir)) {
    output_dir <- "output"  # Default directory
  } else {
    # Clean up path (normalize slashes, remove trailing slash)
    output_dir <- normalizePath(output_dir, mustWork = FALSE)
  }
  
  # Check if files exist and CLOBBER is FALSE
  if (save_format != "none" && !CLOBBER) {
    rds_file <- file.path(output_dir, paste0(tolower(state_code), "_water_systems.rds"))
    csv_file <- file.path(output_dir, paste0(tolower(state_code), "_water_systems.csv"))
    
    files_exist <- FALSE
    if (save_format %in% c("both", "rds") && file.exists(rds_file)) {
      files_exist <- TRUE
    }
    if (save_format %in% c("both", "csv") && file.exists(csv_file)) {
      files_exist <- TRUE
    }
    
    if (files_exist) {
      message("Files already exist and CLOBBER = FALSE. Skipping download.")
      return(NULL)
    }
  }
  
  # Rest of the function remains the same until the saving part
  base_url <- "https://data.epa.gov/efservice/WATER_SYSTEM"
  
  query_params <- list(
    paste0('PRIMACY_AGENCY_CODE/=/', state_code),
    paste0('PWS_ACTIVITY_CODE/=/', activity_status)
  )
  
  if (!is.null(pws_type_code)) {
    pws_type_code <- toupper(pws_type_code)
    if (!pws_type_code %in% c("C", "NC", "NTNC")) {
      stop("PWS type code must be one of: 'C' (Community), 'NC' (Non-Community), or 'NTNC' (Non-Transient Non-Community)")
    }
    query_params <- append(query_params, paste0('PWS_TYPE_CODE/=/', pws_type_code))
  }
  
  query_params <- append(query_params, 'output/JSON')
  full_url <- paste(c(base_url, query_params), collapse = "/")
  
  tryCatch({
    response <- GET(full_url)
    
    if (status_code(response) == 200) {
      water_systems <- fromJSON(rawToChar(response$content))
      
      if (!is.data.frame(water_systems)) {
        water_systems <- as.data.frame(water_systems)
      }
      
      if (save_format != "none") {
        dir_created <- dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
        
        if (!dir.exists(output_dir)) {
          stop(paste("Could not create or access output directory:", output_dir))
        }
        
        rds_file <- file.path(output_dir, paste0(tolower(state_code), "_water_systems.rds"))
        csv_file <- file.path(output_dir, paste0(tolower(state_code), "_water_systems.csv"))
        
        if (save_format %in% c("both", "rds")) {
          saveRDS(water_systems, rds_file)
          cat("Data saved to", rds_file, "\n")
        }
        
        if (save_format %in% c("both", "csv")) {
          write.csv(water_systems, csv_file, row.names = FALSE)
          cat("Data saved to", csv_file, "\n")
        }
      }
      
      cat("Retrieved", nrow(water_systems), "water systems from", state_code, "\n")
      return(water_systems)
      
    } else {
      stop(paste("API request failed with status code:", status_code(response)))
    }
    
  }, error = function(e) {
    message("Error fetching water systems data: ", e$message)
    return(NULL)
  })
}

# Example usage:
# Default behavior (won't overwrite existing files)
# alaska_systems <- get_state_water_systems("AK")

# Force overwrite of existing files
# alaska_systems <- get_state_water_systems("AK", CLOBBER = TRUE)

# Skip if files exist
# ca_systems <- get_state_water_systems("CA", save_format = "both", CLOBBER = FALSE)
