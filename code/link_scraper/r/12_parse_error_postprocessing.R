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
# 2. Split the data by the 'domain' column.
# 3. For each domain, append the new entries to an existing domain-specific
#    'domain_name_parse_error.rds' file or create a new one if it doesn't exist.
# 4. After appending, remove any duplicate entries based on the 'url' column
#    within each domain-specific file.
# 5. Once all entries have been successfully distributed, clear the main
#    'parse_error.rds' file to prevent reprocessing.
#
# RECEIVES FROM:
# - /data/input/parse_error.rds
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
  
  # Ensure the target directory for domain-specific files exists
  if (!dir.exists(domain_error_dir)) {
    message(sprintf("Creating directory for domain-specific parse errors: %s", domain_error_dir))
    dir.create(domain_error_dir, recursive = TRUE)
  }
  
  # --- 2. LOAD MAIN PARSE ERROR FILE ---
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
  
  # --- 3. SPLIT DATA BY DOMAIN AND PROCESS EACH ---
  domain_list <- split(parse_error_dt, by = "domain")
  total_domains <- length(domain_list)
  message(sprintf("Splitting data into %d unique domains.", total_domains))
  
  for (i in seq_along(domain_list)) {
    domain_name <- names(domain_list)[i]
    domain_dt <- domain_list[[i]]
    
    # Define the path for the domain-specific RDS file
    domain_file_path <- file.path(domain_error_dir, paste0(domain_name, "_parse_error.rds"))
    
    message(sprintf("\n[%d/%d] Processing domain: '%s'", i, total_domains, domain_name))
    
    # --- 4. APPEND NEW ENTRIES TO EXISTING FILE OR CREATE NEW ---
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
    
    # --- 5. REMOVE DUPLICATES ---
    initial_rows <- nrow(combined_dt)
    # The 'unique' function in data.table is highly optimized
    deduplicated_dt <- unique(combined_dt, by = "url")
    final_rows <- nrow(deduplicated_dt)
    
    duplicates_removed <- initial_rows - final_rows
    if (duplicates_removed > 0) {
      message(sprintf("  -> Removed %d duplicate entries based on 'url'.", duplicates_removed))
    } else {
      message("  -> No duplicates found.")
    }
    
    # --- 6. SAVE THE CLEANED DATA ---
    message(sprintf("  -> Saving %d unique entries to: %s", final_rows, basename(domain_file_path)))
    saveRDS(deduplicated_dt, domain_file_path)
  }
  
  # --- 7. CLEAR THE MAIN PARSE ERROR FILE ---
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

