# ==============================================================================
# MODULE: INITIALIZATION & CONFIGURATION
# ==============================================================================
# 
# This module loads the Input rds. with all input links. It creates a separate input 
# to use in this project. It initializes the output data set, which will be the result
# of this project. 
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
#
# ==============================================================================

# Load required packages
library(data.table)

# Path Configuration Function
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input = file.path(base_path, "data", "input"),
    output = file.path(base_path, "data", "output"),
    config = file.path(base_path, "data", "config"),
    state = file.path(base_path, "data", "state"),
    logs = file.path(base_path, "data", "logs")
  )
}

# 1. initialize input if not existing. 
init_input_dataset <- function(paths) {
  # Define path to original input dataset
  original_input_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/all_links_filtered_by_date.rds"
  # Define path to output input.rds
  output_input_path <- file.path(paths$input, "input.rds")
  
  # Check if input.rds already exists
  if (file.exists(output_input_path)) {
    message("Input dataset already exists at ", output_input_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Load the original input dataset
  input_dt <- readRDS(original_input_path)
  
  # Ensure data.table format
  library(data.table)
  setDT(input_dt)
  
  # Helper function to clean domains from URLs, remove 'www.' and TLDs (.de, .com, etc.)
  .extract_domain <- function(url) {
    # Remove protocol and www.
    domain <- sub("^https?://(?:www\\.)?", "", url)
    # Remove everything after first slash
    domain <- sub("/.*$", "", domain)
    # Remove TLD by cutting from last dot to end
    domain <- sub("\\.[^.]+$", "", domain)
    domain
  }
  
  # Create the new input dataset with cleaned domain and initialized flags
  input <- input_dt[, .(
    id = .I,                       # unique incremental identifier
    domain = .extract_domain(domain_url),  # cleaned domain
    url = result_link,
    processed = FALSE,             # initialize as FALSE
    retry = FALSE                 # initialize as FALSE
  )]
  
  # Save the new input dataset as input.rds
  saveRDS(input, output_input_path)
  
  message("Input dataset successfully created and saved to ", output_input_path)
  
  return(invisible(TRUE))
}

# initialize, create and load 
paths <- get_module_paths()
init_input_dataset(paths)
# input_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/input.rds")

##### 

# 2. Initialize final output 

init_final_output_dataset <- function(paths) {
  # Define the full output file path
  output_path <- file.path(paths$output, "final_data.rds")
  
  # Check if final_data.rds already exists to avoid overwriting
  if (file.exists(output_path)) {
    message("Output dataset already exists at ", output_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Create an empty data.table with the specified columns and types:
  # id: integer (to match input id)
  # domain: character
  # url: character
  # timestamp_scraped: POSIXct (date-time of scraping)
  # date_time: character (published date/time string)
  # author: character
  # headline: character
  # text: character
  # paywall: logical (TRUE/FALSE)
  
  final_data <- data.table(
    id = integer(),
    domain = character(),
    url = character(),
    timestamp_scraped = as.POSIXct(character()),
    date_time = character(),
    author = character(),
    headline = character(),
    text = character(),
    paywall = logical()
  )
  
  # Save the empty dataset as RDS file to the output path
  saveRDS(final_data, output_path)
  
  message("Empty output dataset 'final_data.rds' created and saved to ", output_path)
  
  return(invisible(TRUE))
}

# initialize, create and load 
init_final_output_dataset(paths)
# final_output_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/output/final_data.rds")

###### 


