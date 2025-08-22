# ==============================================================================
# MODULE: PARSE ERROR POST-PROCESSOR
# ==============================================================================
#
# This module is designed to process the main 'parse_error.rds' dataset.
# Its primary function is to split the aggregated error log into smaller,
# domain-specific RDS files. This allows for targeted re-parsing and rule
# adjustment for each domain.
#
# The pipeline is as follows:
# 1. Load the main 'parse_error.rds' file.
# 2. Load 'final_data.rds' to get a list of already successful URLs.
# 3. Split the error data by the 'domain' column.
# 4. For each domain, append new entries to an existing domain-specific
#    'domain_name_parse_error.rds' file or create a new one if it doesn't exist.
# 5. Remove any duplicate entries based on the 'url' column.
# 6. Remove any entries that are already present in 'final_data.rds'.
# 7. Once all entries have been successfully distributed, clear the main
#    'parse_error.rds' file to prevent reprocessing.
#
# RECEIVES FROM:
# - /data/input/parse_error.rds
# - /data/output/final_data.rds (for cross-checking)
#
# OUTPUTS TO:
# - /data/input/parse_error/ (domain-specific RDS files)
#
# ==============================================================================

# Load required packages
library(data.table)

# 1: Main function to process and split the parse_error.rds file
func_12_process_parse_errors <- function() {
  
  # --- 1. SETUP AND PATH DEFINITION ---
  message("--- Starting Parse Error Post-Processing ---")
  paths <- get_module_paths()
  
  # Define paths for the main input file and the target directory
  main_parse_error_file <- file.path(paths$input, "parse_error.rds")
  domain_error_dir <- file.path(paths$input, "parse_error")
  final_data_file <- file.path(paths$output, "final_data.rds")
  
  # Ensure the target directory for domain-specific files exists
  if (!dir.exists(domain_error_dir)) {
    message(sprintf("Creating directory for domain-specific parse errors: %s", domain_error_dir))
    dir.create(domain_error_dir, recursive = TRUE)
  }
  
  # --- 2. LOAD FINAL DATA FOR DUPLICATE CHECKING ---
  if (file.exists(final_data_file)) {
    message("Loading 'final_data.rds' to cross-reference successfully parsed URLs.")
    final_data_dt <- readRDS(final_data_file)
    # Create a simple vector of unique URLs for efficient lookup
    successful_urls <- unique(final_data_dt$url)
  } else {
    message("Warning: 'final_data.rds' not found. Skipping check against final output.")
    successful_urls <- character(0) # Create an empty vector to prevent errors
  }
  
  # --- 3. LOAD MAIN PARSE ERROR FILE ---
  if (!file.exists(main_parse_error_file)) {
    stop("Main 'parse_error.rds' file not found. Aborting.")
  }
  
  message(sprintf("Loading main parse error file from: %s", main_parse_error_file))
  parse_error_dt <- readRDS(main_parse_error_file)
  setDT(parse_error_dt)
  
  # Check if there is anything to process
  if (nrow(parse_error_dt) == 0) {
    message("Main 'parse_error.rds' is empty. No processing needed.")
    message("--- Parse Error Post-Processing Finished ---")
    return(invisible(TRUE))
  }
  
  message(sprintf("Found %d entries to process.", nrow(parse_error_dt)))
  
  # --- 4. SPLIT DATA BY DOMAIN AND PROCESS EACH ---
  domain_list <- split(parse_error_dt, by = "domain")
  total_domains <- length(domain_list)
  message(sprintf("Splitting data into %d unique domains.", total_domains))
  
  for (i in seq_along(domain_list)) {
    domain_name <- names(domain_list)[i]
    domain_dt <- domain_list[[i]]
    
    # Define the path for the domain-specific RDS file
    domain_file_path <- file.path(domain_error_dir, paste0(domain_name, "_parse_error.rds"))
    
    message(sprintf("\n[%d/%d] Processing domain: '%s'", i, total_domains, domain_name))
    
    # --- 5. APPEND NEW ENTRIES TO EXISTING FILE OR CREATE NEW ---
    if (file.exists(domain_file_path)) {
      # If file exists, load it and append new data
      message(sprintf("  -> Found existing file. Appending %d new entries.", nrow(domain_dt)))
      existing_dt <- readRDS(domain_file_path)
      setDT(existing_dt)
      combined_dt <- rbindlist(list(existing_dt, domain_dt), use.names = TRUE, fill = TRUE)
    } else {
      # If file does not exist, use the current data as the starting point
      message(sprintf("  -> No existing file found. Creating new file with %d entries.", nrow(domain_dt)))
      combined_dt <- domain_dt
    }
    
    # --- 6. REMOVE DUPLICATES WITHIN THE PARSE ERROR LIST ---
    initial_rows <- nrow(combined_dt)
    deduplicated_dt <- unique(combined_dt, by = "url")
    
    duplicates_removed <- initial_rows - nrow(deduplicated_dt)
    if (duplicates_removed > 0) {
      message(sprintf("  -> Removed %d duplicate entries based on 'url'.", duplicates_removed))
    } else {
      message("  -> No duplicates found.")
    }
    
    # --- 7. REMOVE ENTRIES ALREADY IN FINAL_DATA.RDS ---
    # This step ensures I don't keep error logs for URLs that were eventually
    # processed successfully, which for some reason made it into the parse error ds by accident. 
    if (length(successful_urls) > 0) {
      deduplicated_dt <- deduplicated_dt[!url %in% successful_urls]
    }
    
    # --- 8. SAVE THE CLEANED DATA ---
    final_rows <- nrow(deduplicated_dt)
    message(sprintf("  -> Saving %d unique, unresolved entries to: %s", final_rows, basename(domain_file_path)))
    saveRDS(deduplicated_dt, domain_file_path)
  }
  
  # --- 9. CLEAR THE MAIN PARSE ERROR FILE ---
  message("\nAll entries have been processed and distributed.")
  message("Clearing the main 'parse_error.rds' file.")
  
  # Create an empty data.table with the same structure
  empty_dt <- parse_error_dt[0, ]
  
  # Overwrite the main file with the empty table
  saveRDS(empty_dt, main_parse_error_file)
  
  message("--- Parse Error Post-Processing Finished Successfully ---")
  
  return(invisible(TRUE))
}

# Call function if needed: 
# func_12_process_parse_errors()