# ==============================================================================
# FINAL WEB SCRAPER PRODUCTION RUN SCRIPT
# ==============================================================================
# This script orchestrates the entire scraping process from start to finish.
# It performs the following steps:
# 1. Sets the working directory and defines file paths.
# 2. Sources all necessary functional modules.
# 3. Creates a new, dynamic chunk of links to be processed.
# 4. Initializes all necessary components: VPN connection, session pools,
#    and in-memory data structures for the current chunk.
# 5. Iterates through every link in the chunk, executing the full
#    request -> response -> analysis -> parsing pipeline.
# 6. At the end of the run, it saves all collected data from the chunk
#    (output, errors, logs, etc.) by appending it to the final RDS master files.
# ==============================================================================

# --- 1. SETUP & CONFIGURATION ---
setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")

# Helper function to define all necessary paths for the project
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input          = file.path(base_path, "data", "input"),
    output         = file.path(base_path, "data", "output"),
    config         = file.path(base_path, "data", "config"),
    parsing_config = file.path(base_path, "data", "config", "06_parsing_config"),
    state          = file.path(base_path, "data", "state"),
    logs           = file.path(base_path, "data", "logs"),
    chunk_logs     = file.path(base_path, "data", "logs",  "chunk_logs"),
    chunk_outputs  = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs   = file.path(base_path, "data", "input",  "chunk_inputs")
  )
}

# --- 2. LOAD ALL MODULES ---

message("Loading all required modules...")
source("01_init.R")
source("02_chunk_manager.R")
source("03_identity_manager.R")
source("04_request_orchestrator.R")
source("05_request_executor.R")
source("06_html_parser.R")
source("07_response_analyzer.R")
source("09_storage_manager.R")
source("10_log_manager.R")
message("All modules loaded successfully.")

# --- 3. CREATE A NEW CHUNK ---

message("\nCreating a new chunk of links to process...")
# This creates a new chunk with default settings (10% of remaining links)
# The name of the chunk (e.g., "chunk_001") is returned and stored.

func_09_update_input()
current_chunk <- func_02_build_chunk(absolute_links = 5000) 
message(sprintf("Successfully created chunk: %s", current_chunk))

# --- 4. INITIALIZE SYSTEMS FOR THE RUN ---

message("\nInitializing systems for the new run...")

# Log the current VPN/IP address connection
func_03_initialzie_vpn_connnection()

# Create the in-memory data.tables for the current chunk (e.g., chunk_001_output)
func_09_init_data_structures(current_chunk)

# Create the pool of browser sessions for all domains
func_04_initialize_session_pool()

message("Systems initialized.")

# --- 5. MAIN PROCESSING LOOP ---

# Get the data for the currently created chunk from the global environment
chunk_dt <- get(current_chunk, envir = .GlobalEnv)
total_links <- nrow(chunk_dt)

message(sprintf("\nStarting to process %d links from chunk %s...", total_links, current_chunk))
message(paste(rep("=", 70), collapse = ""))

# Initialize counters to track run statistics
successful_count <- 0
retry_count <- 0
error_count <- 0
parse_error_count <- 0

# Process each link in the chunk data.table
for (i in 1:nrow(chunk_dt)) {
  # Extract all information for the current link
  link_info <- chunk_dt[i]
  
  message(sprintf("\n[%d/%d] Processing: %s", i, total_links, link_info$url))
  message(sprintf("Domain: %s", link_info$domain))
  
  # Step 1: Prepare the request package
  request_package <- func_04_prepare_request(
    id = link_info$id,
    url = link_info$url,
    domain = link_info$domain,
    chunk_dt = chunk_dt,
    aggressiveness_level = 1
  )
  
  # Check if a session was available and the package was created
  if (!request_package$success) {
    message(sprintf("Failed to prepare request: %s", request_package$message))
    # Log this as a general error
    func_10_append_error(
      error_reason = request_package$message,
      processed_dt = data.table(id = link_info$id, domain = link_info$domain, url = link_info$url),
      chunk_name = current_chunk
    )
    error_count <- error_count + 1
    next
  }
  
  # Step 2: Execute the HTTP request
  message("Executing HTTP request...")
  response_result <- func_05_execute_request(
    request_package = request_package,
    chunk_name = current_chunk,
    worker_id = 1 # Using a single worker for this script
  )
  
  # Step 3: Analyze the response
  message("Analyzing response...")
  analysis_result <- func_07_analyze_response(
    response_result = response_result,
    chunk_name = current_chunk
  )
  
  # Step 4: Handle the result of the analysis
  if (analysis_result$action == "parse") {
    if (isTRUE(analysis_result$parse_result$success)) {
      successful_count <- successful_count + 1
      message("--> Status: SUCCESS")
    } else {
      parse_error_count <- parse_error_count + 1
      message(sprintf("--> Status: PARSE_ERROR (%s)", analysis_result$parse_result$reason))
    }
  } else if (analysis_result$action == "retry") {
    retry_count <- retry_count + 1
    message(sprintf("--> Status: RETRY (%s)", analysis_result$response_analysis))
  } else { # "error"
    error_count <- error_count + 1
    message(sprintf("--> Status: ERROR (%s)", analysis_result$message))
  }
  
  # Add a small, random delay to mimic human behavior
  Sys.sleep(runif(1, 0.5, 1.5))
}

message(paste(rep("=", 70), collapse = ""))
message("CHUNK PROCESSING COMPLETE.")

# --- 6. PERSIST RESULTS ---

message("\nAppending all collected data to final RDS files...")
# This function handles all appending operations safely
func_09_fill_output(current_chunk)

message("\nRun finished successfully!")
