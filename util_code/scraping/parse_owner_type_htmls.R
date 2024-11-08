library(openai)
k <- fread('../openai_eis_key.txt',header = F)$V1

Sys.setenv(
  OPENAI_API_KEY = k
)

ht_loc <- 'scratch/html_reports/'
fls <- list.files(ht_loc)
mssg <- "This html file has information about a single water district. Aggregate these data into one-row data frame. Exclude table components that describe all potential values. Return as json object."
library(jsonlite)
process_single_html <- function(file_path, save_dir = file.path("scratch", "html_responses"), CLOBBER = FALSE) {
  # Check if file exists and CLOBBER is FALSE
  response_path <- file.path(save_dir, paste0(tools::file_path_sans_ext(basename(file_path)), ".json"))
  if(file.exists(response_path) && !CLOBBER) {
    message("File exists and CLOBBER=FALSE, skipping: ", response_path)
    return(invisible(NULL))
  }
  
  # Read the HTML content
  html_content <- read_file(file_path)
  
  # Create the OpenAI API prompt
  prompt <- mssg
  
  # Call OpenAI API
  response <- openai::create_chat_completion(
    model = "gpt-3.5-turbo",
    messages = list(
      list(
        "role" = "user",
        "content" = paste(prompt, html_content)
      )
    )
  )
  
  # Save response as JSON to specified directory
  dir.create(dirname(response_path), recursive = TRUE, showWarnings = FALSE)
  jsonlite::write_json(response, response_path, auto_unbox = TRUE, pretty = TRUE)
}

# Loop through files and process each one
for(fl in fls) {
  file_path <- file.path(ht_loc, fl)
  process_single_html(file_path, CLOBBER = FALSE)
}
