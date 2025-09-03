# ==============================================================================
# SCRIPT: create_unprocessed_url_lists.R
#
# PURPOSE:
# This script loads the main 'input.rds' file, finds all unique domains
# within it, and extracts all URLs that have not yet been processed
# (i.e., where the 'processed' column is FALSE).
#
# It then creates a separate data.table in the global environment for each
# of these domains. Each data.table contains a single 'url' column with the
# list of unprocessed links for that specific domain.
#
# The naming convention for the output data.tables is:
# missing_urls_[DOMAINNAME] (e.g., missing_urls_welt_de)
#
# ==============================================================================

# --- 1. SETUP ---

# Load the required library for data manipulation
library(data.table)
library(paperboy)

# Helper function to define all necessary paths for the project
# This ensures the script can find the 'input.rds' file correctly.
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input          = file.path(base_path, "data", "input"),
    output         = file.path(base_path, "data", "output"),
    config         = file.path(base_path, "data", "config"),
    parsing_config = file.path(base_path, "data", "config", "06_parsing_config"),
    state          = file.path(base_path, "data", "state"),
    logs           = file.path(base_path, "data", "logs"),
    chunk_outputs  = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs   = file.path(base_path, "data", "input",  "chunk_inputs"),
    parse_error    = file.path(base_path, "data", "input", "parse_error")
  )
}


# --- 2. LOAD AND FILTER DATA ---

# Get the project paths
paths <- get_module_paths()
input_file_path <- file.path(paths$input, "input.rds")

# Check if the input file exists before trying to read it
if (!file.exists(input_file_path)) {
  stop("FATAL: The main input file was not found at: ", input_file_path)
}

message("Loading 'input.rds'. This may take a moment...")
input_dt <- readRDS(input_file_path)
setDT(input_dt) # Ensure it's a data.table

message("Filtering for all unprocessed links from all available domains...")

# Filter the data.table to get only the rows where the 'processed' flag is FALSE.
# We select only the 'domain' and 'url' columns as they are all we need.
unprocessed_links_dt <- input_dt[processed == FALSE, .(domain, url)]

# Get a list of all unique domains that have unprocessed links
all_unprocessed_domains <- unique(unprocessed_links_dt$domain)

if (nrow(unprocessed_links_dt) == 0) {
  message("No unprocessed links were found. Exiting.")
} else {
  message(sprintf("Found %d unprocessed links across %d domains to distribute.", 
                  nrow(unprocessed_links_dt), 
                  length(all_unprocessed_domains)))
  
  # --- 3. CREATE DOMAIN-SPECIFIC DATA TABLES ---
  
  message("Creating a separate data.table for each domain...")
  
  # Loop through each unique domain found in our unprocessed links
  for (current_domain in all_unprocessed_domains) {
    
    # Filter the `unprocessed_links_dt` to get URLs only for the current domain
    domain_specific_urls <- unprocessed_links_dt[domain == current_domain, .(url)]
    
    # Clean the domain name to create a valid R variable name.
    # This replaces dots (.) and hyphens (-) with underscores (_).
    # The name is now in the format "missing_urls_DOMAINNAME"
    dt_name <- paste0("missing_urls_", gsub("[.-]", "_", current_domain))
    
    # Use the `assign` function to create a new data.table in the global environment
    # with the name we just constructed.
    assign(dt_name, domain_specific_urls, envir = .GlobalEnv)
    
    message(sprintf(" -> Created '%s' with %d URLs.", dt_name, nrow(domain_specific_urls)))
  }
  
  # --- 4. VERIFICATION ---
  
  message("\n--- Verification ---")
  message("The following data.tables have been created in your R environment:")
  
  # List all objects that match the new naming pattern to confirm creation
  created_objects <- ls(pattern = "^missing_urls_")
  print(created_objects)
  
  # Show a sample of the first created data.table as an example
  if (length(created_objects) > 0) {
    message("\nHere is a sample from the first created table ('", created_objects[1], "'):")
    print(head(get(created_objects[1]), 5))
  }
  
  message("\nScript finished successfully.")
}
