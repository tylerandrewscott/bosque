library(openai)
library(readr)
library(data.table)
library(TheOpenAIR)

k <- fread('../openai_eis_key.txt',header = F)$V1
tokmax = 16385
Sys.setenv(
  OPENAI_API_KEY = k
)

ht_loc <- 'input/texas_dww/html_reports/'
fls <- rev(list.files(ht_loc))
mssg <- "You are a helpful assistant. Record the owner type, # I/C w/other PWS, and total storage of this water system." 

library(jsonlite)
process_single_html <- function(file_path, save_dir = file.path("input","texas_dww", "html_extracts"), CLOBBER = FALSE) {
  # Check if file exists and CLOBBER is FALSE
  response_path <- file.path(save_dir, paste0(tools::file_path_sans_ext(basename(file_path)), ".json"))
  if(file.exists(response_path) && !CLOBBER) {
    return(invisible(NULL))
  }
  
  # Read the HTML content
  html_content <- read_file(file_path)
  
  # Create the OpenAI API prompt
  prompt <- mssg
  
  # Calculate total tokens using tiktoken
  full_text <- paste(prompt, html_content)
  token_count <- count_tokens(full_text)
  
  # Truncate if needed
  if(token_count > tokmax) {
    # Calculate how many tokens we need to remove
    excess_tokens <- token_count - tokmax + 100  # buffer of 100 tokens
    
    # Truncate the HTML content by removing tokens from the end
    truncated_text <- truncate_text_to_tokens(paste(prompt, html_content), tokmax - 100)
    # Remove the prompt from the truncated text to get just the truncated HTML
    html_content <- substr(truncated_text, nchar(prompt) + 1, nchar(truncated_text))
  }
  
  # Call OpenAI API with error handling
  tryCatch({
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
  }, error = function(e) {
    message("Error processing file: ", file_path)
    message("Error message: ", e$message)
  })
}

# Helper function to truncate text to specific token count
truncate_text_to_tokens <- function(text, max_tokens) {
  # Start with full text
  current_text <- text
  
  # Binary search to find appropriate truncation point
  left <- 1
  right <- nchar(text)
  
  while (left < right) {
    mid <- floor((left + right) / 2)
    test_text <- substr(text, 1, mid)
    tokens <- count_tokens(test_text)
    
    if (tokens == max_tokens) {
      return(test_text)
    } else if (tokens < max_tokens) {
      left <- mid + 1
    } else {
      right <- mid - 1
    }
  }
  
  return(substr(text, 1, left))
}

# Loop through files and process each one
for(fl in fls) {
  file_path <- file.path(ht_loc, fl)
  process_single_html(file_path, CLOBBER = FALSE)
}
