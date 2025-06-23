# ==============================================================================
# PRELIMINARY WEB SCRAPER RUN SCRIPT – FIXED CHUNK, RESETTABLE TESTING
# ==============================================================================

setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")

# Configuration Function
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input         = file.path(base_path, "data", "input"),
    output        = file.path(base_path, "data", "output"),
    config        = file.path(base_path, "data", "config"),
    parsing_config = file.path(base_path, "data", "config", "06_parsing_config"),
    state         = file.path(base_path, "data", "state"),
    logs          = file.path(base_path, "data", "logs"),
    chunk_logs    = file.path(base_path, "data", "logs",  "chunk_logs"),
    chunk_outputs = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs  = file.path(base_path, "data", "input",  "chunk_inputs")
  )
}


# Load all required modules
message("Loading modules...")
source("01_init.R")
source("02_chunk_manager.R")
source("03_identity_manager.R")
source("04_request_orchestrator.R")
source("05_request_executor.R")
source("06_html_parser.R")
source("07_response_analyzer.R")
source("09_storage_manager.R")
source("10_log_manager.R")

paths <- get_module_paths()
chunk_dir <- file.path(paths$input, "chunks")
chunk_path <- file.path(chunk_dir, "chunk_999.rds")  # fixed chunk name 
chunk_obj_name <- "chunk_999"

# init chunk once 
if (!file.exists(chunk_path)) {
  message("Creating fixed chunk (chunk_999)...")
  func_02_build_chunk(absolute_links = 50)  
  # Rename and move to fixed path
  last_chunk <- ls(pattern = "^chunk_\\d{3}$", envir = .GlobalEnv)
  last_created <- last_chunk[which.max(as.integer(sub("chunk_", "", last_chunk)))]
  chunk_dt <- get(last_created, envir = .GlobalEnv)
  saveRDS(chunk_dt, chunk_path)
  rm(list = last_created, envir = .GlobalEnv)
}
# relaod chunk 
chunk_dt <- readRDS(chunk_path)
assign(chunk_obj_name, chunk_dt, envir = .GlobalEnv)
current_chunk <- chunk_obj_name
message(sprintf("Using fixed chunk: %s", current_chunk))


# ==============================================================================
# RESET CONTROL STRUCTURES
# ==============================================================================

message("Resetting chunk-related data structures...")
func_09_init_data_structures(current_chunk)
func_04_initialize_session_pool()

# ==============================================================================
# MAIN PROCESSING LOOP
# ==============================================================================

# Get the chunk data
chunk_dt <- get(current_chunk, envir = .GlobalEnv)
total_links <- nrow(chunk_dt)

message(sprintf("\nStarting to process %d links from chunk %s", total_links, current_chunk))
message(paste(rep("=", 70), collapse = ""))

# Initialize counters
successful_count <- 0
retry_count <- 0
error_count <- 0
parse_error_count <- 0

# Process each link in the chunk
for (i in 1:nrow(chunk_dt)) {
  # Extract link information
  link_info <- chunk_dt[i]
  
  message(sprintf("\n[%d/%d] Processing: %s", i, total_links, link_info$url))
  message(sprintf("Domain: %s", link_info$domain))
  
  # Step 1: Prepare request package using module 04
  request_package <- func_04_prepare_request(
    url = link_info$url,
    domain = link_info$domain,
    chunk_dt = chunk_dt,
    aggressiveness_level = 1  # Fixed aggressiveness level
  )
  
  # Check if request preparation was successful
  if (!request_package$success) {
    message(sprintf("Failed to prepare request: %s", request_package$message))
    
    # Log as error
    func_10_append_error(
      error_reason = request_package$message,
      input_info = link_info,
      chunk_name = current_chunk
    )
    error_count <- error_count + 1
    next
  }
  
  # Add link ID to request package for tracking
  request_package$request_params$id <- link_info$id
  
  # Step 2: Execute HTTP request using module 05
  message("Executing HTTP request...")
  response_result <- func_05_execute_request(
    request_package = request_package,
    chunk_name = current_chunk,
    worker_id = 1  # Single worker for test run
  )
  
  # Step 3: Analyze response using module 07
  message("Analyzing response...")
  analysis_result <- func_07_analyze_response(
    response_result = response_result,
    chunk_name = current_chunk
  )
  
  # Handle response based on analysis
  if (analysis_result$action == "parse") {
    
    if (isTRUE(analysis_result$parse_result$success)) {
      successful_count  <- successful_count + 1
    } else {
      parse_error_count <- parse_error_count + 1
    }
    
  } else if (analysis_result$action == "retry") {
    
    retry_count <- retry_count + 1
    
  } else {  # "error"
    
    error_count <- error_count + 1

  }
  
  # Add small delay between requests
  Sys.sleep(runif(1, 0.5, 1.5))
  
  # Progress update every 10 links
  if (i %% 10 == 0) {
    message(sprintf("\nProgress: %d/%d links processed", i, total_links))
    message(sprintf("Success: %d, Retry: %d, Error: %d, Parse Error: %d", 
                    successful_count, retry_count, error_count, parse_error_count))
  }
}

# ==============================================================================
# SUMMARY REPORT
# ==============================================================================

message("\n" %+% paste(rep("=", 70), collapse = ""))
message("CHUNK PROCESSING COMPLETE")
message(paste(rep("=", 70), collapse = ""))

message(sprintf("Total links processed: %d", total_links))
message(sprintf("Successful: %d (%.1f%%)", successful_count, successful_count/total_links*100))
message(sprintf("Retry needed: %d (%.1f%%)", retry_count, retry_count/total_links*100))
message(sprintf("Errors: %d (%.1f%%)", error_count, error_count/total_links*100))
message(sprintf("Parse errors: %d (%.1f%%)", parse_error_count, parse_error_count/total_links*100))

# Display data structure sizes
message("\nData structure summary:")
output_dt <- get(paste0(current_chunk, "_output"), envir = .GlobalEnv)
retry_dt <- get(paste0(current_chunk, "_retry"), envir = .GlobalEnv)
error_dt <- get(paste0(current_chunk, "_error"), envir = .GlobalEnv)
parse_error_dt <- get(paste0(current_chunk, "_parse_error"), envir = .GlobalEnv)

message(sprintf("Output entries: %d", nrow(output_dt)))
message(sprintf("Retry entries: %d", nrow(retry_dt)))
message(sprintf("Error entries: %d", nrow(error_dt)))
message(sprintf("Parse error entries: %d", nrow(parse_error_dt)))

# Status code distribution: 
message("\nHTTP status code distribution:")
status_vector <- get(paste0(current_chunk, "_response_log"), envir = .GlobalEnv)$status_code
status_table <- sort(table(status_vector, useNA = "ifany"), decreasing = TRUE)

for (i in seq_along(status_table)) {
  code <- names(status_table)[i]
  code_label <- ifelse(code == "NA", "NA (no response)", code)
  count <- as.integer(status_table[i])
  message(sprintf("Status %s: %d", code_label, count))
}

# Helper operator for string concatenation
`%+%` <- function(a, b) paste0(a, b)

message("\nTest run completed successfully!")
