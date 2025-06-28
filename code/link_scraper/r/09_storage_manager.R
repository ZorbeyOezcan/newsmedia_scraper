# ==============================================================================
# MODULE: DATA STORAGE & CHECKPOINT MANAGEMENT
# ==============================================================================
# 
# This module handles all persistent storage operations and maintains system
# checkpoints. It saves successfully parsed articles to structured output files,
# creates checkpoint saves every 1000 processed links, manages the retry queue
# persistence, stores and updates system state between runs, and merges chunk
# outputs into final datasets. The module ensures data integrity and enables
# the system to resume from any point in case of interruption.
#
# RECEIVES FROM:
# - 06_html_parser: Successfully parsed articles
# - 07_response_analyzer: Failed requests for retry queue
# - 04_request_orchestrator: Session states and progress data
# 
# OUTPUTS TO:
# - File system: Persistent storage of all data structures
# - 08_domain_state_manager: Chunk results for analysis
#
# ==============================================================================

# Load required packages
library(data.table)

# Configuration Function

# 1: Function to initialize chunk-specific data structures with validation
func_09_init_data_structures <- function(chunk_name) {
  # Validate chunk name format
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("Invalid chunk name. Expected format: 'chunk_001', 'chunk_002', etc.")
  }
  
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  paths        <- get_module_paths()
  
  # Ensure folders exist
  dir.create(paths$chunk_logs,    showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$chunk_outputs, showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$chunk_inputs,  showWarnings = FALSE, recursive = TRUE)
  
  # Initialize output container
  chunk_output <- data.table(
    id               = integer(),
    domain           = character(),
    url              = character(),
    timestamp_scraped= as.POSIXct(character()),
    date_time        = as.POSIXct(character()),
    author           = character(),
    headline         = character(),
    text             = character(),
    paywall          = logical()
  )
  
  # Initialize retry container
  chunk_retry <- data.table(
    request_id          = integer(),
    response_id         = integer(),
    id                  = integer(),
    domain              = character(),
    url                 = character(),
    timestamp_scraped   = as.POSIXct(character()),
    from_chunk          = integer(),
    status_code         = integer(),
    response_headers    = list(),
    response_body       = character(),
    response_time       = numeric(),
    retry_reason        = character(),
    server_date         = as.POSIXct(character()),
    content_type        = character(),
    content_length      = integer(),
    server              = character(),
    user_agent_id       = integer(),
    ip_address          = character(),
    dns_time            = numeric(),
    connect_time        = numeric(),
    total_time          = numeric(),
    curl_error_code     = integer(),
    ssl_verify_result   = integer(),
    redirect_count      = integer(),
    rate_limit_remaining= integer(),
    rate_limit_reset    = as.POSIXct(character()),
    retry_after         = integer()
  )
  
  # Initialize error container
  chunk_error <- data.table(
    id           = integer(),
    domain       = character(),
    url          = character(),
    error_reason = character()
  )
  
  # Initialize parse-error container
  chunk_parse_error <- data.table(
    id                 = integer(),
    domain             = character(),
    url                = character(),
    timestamp_scraped  = as.POSIXct(character()),
    date_time          = as.POSIXct(character()),
    author             = character(),
    headline           = character(),
    text               = character(),
    paywall            = logical(),
    html_content       = character()
  )
  
  # Initialize request log
  chunk_request_log <- data.table(
    request_id          = integer(),
    id                  = integer(),
    domain              = character(),
    url                 = character(),
    timestamp_scraped   = as.POSIXct(character()),
    from_chunk          = integer(),
    session_id          = character(),
    worker_id           = integer(),
    user_agent_id       = integer(),
    ip_address          = character(),
    user_agent          = character(),
    accept              = character(),
    accept_language     = character(),
    accept_encoding     = character(),
    connection          = character(),
    referer             = character(),
    host                = character(),
    upgrade_insecure_requests = character(),
    sec_fetch_dest      = character(),
    sec_fetch_mode      = character(),
    sec_fetch_site      = character(),
    aggressiveness_level= integer(),
    browser_type        = character(),
    is_mobile           = logical(),
    is_first_request    = logical(),
    session_request_count = integer(),
    cookie_jar_path     = character()
  )
  
  # Initialize response log
  chunk_response_log <- data.table(
    request_id           = integer(),
    response_id          = integer(),
    id                   = integer(),
    domain               = character(),
    url                  = character(),
    timestamp_scraped    = as.POSIXct(character()),
    from_chunk           = integer(),
    status_code          = integer(),
    response_headers     = list(),
    response_time        = numeric(),
    server_date          = as.POSIXct(character()),
    content_type         = character(),
    content_length       = integer(),
    server               = character(),
    user_agent_id        = integer(),
    ip_address           = character(),
    dns_time             = numeric(),
    connect_time         = numeric(),
    total_time           = numeric(),
    curl_error_code      = integer(),
    ssl_verify_result    = integer(),
    redirect_count       = integer(),
    rate_limit_remaining = integer(),
    rate_limit_reset     = as.POSIXct(character()),
    retry_after          = integer(),
    response_analysis    = character()
  )
  
  # Assign to global environment
  assign(paste0(chunk_name, "_output"),        chunk_output,      envir = .GlobalEnv)
  assign(paste0(chunk_name, "_retry"),         chunk_retry,       envir = .GlobalEnv)
  assign(paste0(chunk_name, "_error"),         chunk_error,       envir = .GlobalEnv)
  assign(paste0(chunk_name, "_parse_error"),   chunk_parse_error, envir = .GlobalEnv)
  assign(paste0(chunk_name, "_request_log"),   chunk_request_log, envir = .GlobalEnv)
  assign(paste0(chunk_name, "_response_log"),  chunk_response_log,envir = .GlobalEnv)
  
  # Console feedback
  message(sprintf("Chunk data structures initialized for: %s", chunk_name))
  
  # Validation function
  validate_structure <- function(chunk_dt_name, master_path, structure_name) {
    cat("\n", paste(rep("-", 60), collapse = ""), "\n")
    cat("Validating:", structure_name, "\n")
    cat("Chunk structure:", chunk_dt_name, "\n")
    cat("Master file:", master_path, "\n\n")
    
    # Check if master file exists
    if (!file.exists(master_path)) {
      cat("VALIDATION FAILED: Master file not found\n")
      return(FALSE)
    }
    
    # Load data structures
    chunk_dt <- get(chunk_dt_name, envir = .GlobalEnv)
    master_dt <- readRDS(master_path)
    
    # Get column names
    chunk_cols <- names(chunk_dt)
    master_cols <- names(master_dt)
    
    # Initialize validation results
    col_name_valid <- TRUE
    col_type_valid <- TRUE
    
    # Check column count
    cat("Column count:\n")
    if (length(chunk_cols) != length(master_cols)) {
      cat(sprintf("  MISMATCH: Chunk has %d columns, Master has %d columns\n", 
                  length(chunk_cols), length(master_cols)))
      col_name_valid <- FALSE
    } else {
      cat(sprintf("  OK: Both have %d columns\n", length(chunk_cols)))
    }
    
    # Check column names and order
    cat("\nColumn names and order:\n")
    if (!identical(chunk_cols, master_cols)) {
      col_name_valid <- FALSE
      # Find differences
      for (i in seq_len(max(length(chunk_cols), length(master_cols)))) {
        if (i > length(chunk_cols)) {
          cat(sprintf("  Column %d: MISSING in chunk (expected: %s)\n", i, master_cols[i]))
        } else if (i > length(master_cols)) {
          cat(sprintf("  Column %d: EXTRA in chunk (found: %s)\n", i, chunk_cols[i]))
        } else if (chunk_cols[i] != master_cols[i]) {
          cat(sprintf("  Column %d: expected '%s' but found '%s'\n", i, master_cols[i], chunk_cols[i]))
        }
      }
    } else {
      cat("  OK: All column names match in correct order\n")
    }
    
    # Check variable types
    cat("\nVariable types:\n")
    type_mismatches <- 0
    for (col in intersect(chunk_cols, master_cols)) {
      chunk_type <- class(chunk_dt[[col]])[1]
      master_type <- class(master_dt[[col]])[1]
      
      # Handle POSIXct variations
      if ((chunk_type %in% c("POSIXct", "POSIXt") && master_type %in% c("POSIXct", "POSIXt"))) {
        # Consider these equivalent
      } else if (chunk_type != master_type) {
        cat(sprintf("  Column '%s': expected type '%s' but found '%s'\n", 
                    col, master_type, chunk_type))
        type_mismatches <- type_mismatches + 1
        col_type_valid <- FALSE
      }
    }
    
    if (type_mismatches == 0) {
      cat("  OK: All variable types match\n")
    }
    
    # Summary
    cat("\nValidation summary:\n")
    if (col_name_valid && col_type_valid) {
      cat("  PASSED: Structure matches master file\n")
      return(TRUE)
    } else {
      cat("  FAILED: Structure does not match master file\n")
      return(FALSE)
    }
  }
  
  # Perform validations
  cat("\n")
  cat("==============================================================================\n")
  cat("VALIDATING CHUNK DATA STRUCTURES AGAINST MASTER FILES\n")
  cat("==============================================================================\n")
  
  validation_results <- list()
  
  # Validate each structure
  validation_results$output <- validate_structure(
    paste0(chunk_name, "_output"),
    file.path(paths$output, "final_data.rds"),
    "OUTPUT"
  )
  
  validation_results$retry <- validate_structure(
    paste0(chunk_name, "_retry"),
    file.path(paths$input, "retry.rds"),
    "RETRY"
  )
  
  validation_results$error <- validate_structure(
    paste0(chunk_name, "_error"),
    file.path(paths$output, "error.rds"),
    "ERROR"
  )
  
  validation_results$parse_error <- validate_structure(
    paste0(chunk_name, "_parse_error"),
    file.path(paths$input, "parse_error.rds"),
    "PARSE ERROR"
  )
  
  validation_results$request_log <- validate_structure(
    paste0(chunk_name, "_request_log"),
    file.path(paths$logs, "request_log.rds"),
    "REQUEST LOG"
  )
  
  validation_results$response_log <- validate_structure(
    paste0(chunk_name, "_response_log"),
    file.path(paths$logs, "response_log.rds"),
    "RESPONSE LOG"
  )
  
  # Final summary
  cat("\n")
  cat("==============================================================================\n")
  cat("OVERALL VALIDATION SUMMARY\n")
  cat("==============================================================================\n")
  
  passed_count <- sum(unlist(validation_results))
  total_count <- length(validation_results)
  
  cat(sprintf("Total structures validated: %d\n", total_count))
  cat(sprintf("Passed: %d\n", passed_count))
  cat(sprintf("Failed: %d\n", total_count - passed_count))
  
  if (passed_count == total_count) {
    cat("\nALL VALIDATIONS PASSED ✓\n")
    message(sprintf("All chunk data structures for %s validated successfully", chunk_name))
  } else {
    cat("\nVALIDATION ERRORS FOUND ✗\n")
    warning(sprintf("Some chunk data structures for %s failed validation", chunk_name))
  }
  
  cat("==============================================================================\n\n")
  
  invisible(validation_results)
}


# 2. Function to append chunk results to final RDS files
func_09_fill_output <- function(chunk_name) {
  
  # Helper function to safely append data from a source data.table to a target RDS file.
  .append_to_rds <- function(source_dt_name, target_rds_path) {
    
    if (!exists(source_dt_name, envir = .GlobalEnv)) {
      warning(sprintf("Source object '%s' not found. Cannot append.", source_dt_name))
      return(invisible(FALSE))
    }
    
    source_dt <- get(source_dt_name, envir = .GlobalEnv)
    
    if (!is.data.table(source_dt) || nrow(source_dt) == 0) {
      message(sprintf("Source '%s' is empty. Nothing to append to '%s'.", source_dt_name, basename(target_rds_path)))
      return(invisible(TRUE))
    }
    
    if (!file.exists(target_rds_path)) {
      warning(sprintf("Target RDS file '%s' not found. Cannot append.", basename(target_rds_path)))
      return(invisible(FALSE))
    }
    
    target_dt <- readRDS(target_rds_path)
    
    # This block ensures that the data types in the source table match the target table before appending.
    
    for(col_name in names(target_dt)){
      # Check if the column exists in the source table to avoid errors
      if(col_name %in% names(source_dt)){
        
        target_class <- class(target_dt[[col_name]])[1]
        source_class <- class(source_dt[[col_name]])[1]
        
        # If the classes do not match, attempt to convert the source column
        if(target_class != source_class){
          message(sprintf("Type mismatch for column '%s' in '%s'. Target: '%s', Source: '%s'. Forcing conversion.", 
                          col_name, source_dt_name, target_class, source_class))
          
          # Use a robust switch to get the correct conversion function
          converter <- switch(target_class,
                              "character" = as.character,
                              "integer"   = as.integer,
                              "numeric"   = as.numeric,
                              "logical"   = as.logical,
                              "POSIXct"   = as.POSIXct,
                              "POSIXt"    = as.POSIXct, # Handle POSIXt as well
                              "list"      = as.list,
                              NULL) # Default for unknown types
          
          if(!is.null(converter)){
            # Use tryCatch to handle potential conversion errors gracefully
            tryCatch({
              # Use data.table's set() for efficiency
              set(source_dt, j = col_name, value = converter(source_dt[[col_name]]))
            }, error = function(e) {
              warning(sprintf("Could not convert column '%s' to type '%s'. Error: %s", col_name, target_class, e$message))
            })
          } else {
            warning(sprintf("No standard converter found for type '%s' in column '%s'. Column not converted.", target_class, col_name))
          }
        }
      }
    }
    
    
    # Harmonize columns (add missing ones) and set order
    target_cols <- names(target_dt)
    
    missing_in_source <- setdiff(target_cols, names(source_dt))
    if (length(missing_in_source) > 0) {
      source_dt[, (missing_in_source) := NA]
    }
    
    missing_in_target <- setdiff(names(source_dt), target_cols)
    if (length(missing_in_target) > 0) {
      target_dt[, (missing_in_target) := NA]
    }
    
    setcolorder(source_dt, names(target_dt))
    
    # Append the data
    combined_dt <- rbindlist(list(target_dt, source_dt), use.names = TRUE)
    
    # Save the combined data back to the RDS file
    saveRDS(combined_dt, target_rds_path)
    
    message(sprintf("Successfully appended %d rows from '%s' to '%s'.", 
                    nrow(source_dt), source_dt_name, basename(target_rds_path)))
    
    return(invisible(TRUE))
  }
  
  # --- Main execution block for func_09_fill_output ---
  
  message(sprintf("\n--- Starting to append results from chunk '%s' to final RDS files ---", chunk_name))
  
  paths <- get_module_paths()
  
  # Define mappings from chunk data objects to final RDS files
  mappings <- list(
    output = list(
      source = paste0(chunk_name, "_output"),
      target = file.path(paths$output, "final_data.rds")
    ),
    error = list(
      source = paste0(chunk_name, "_error"),
      target = file.path(paths$output, "error.rds")
    ),
    parse_error = list(
      source = paste0(chunk_name, "_parse_error"),
      target = file.path(paths$input, "parse_error.rds")
    ),
    request_log = list(
      source = paste0(chunk_name, "_request_log"),
      target = file.path(paths$logs, "request_log.rds")
    ),
    response_log = list(
      source = paste0(chunk_name, "_response_log"),
      target = file.path(paths$logs, "response_log.rds")
    ),
    retry = list(
      source = paste0(chunk_name, "_retry"),
      target = file.path(paths$input, "retry.rds")
    )
  )
  
  # Iterate through mappings and append data for each
  for (type in names(mappings)) {
    .append_to_rds(
      source_dt_name = mappings[[type]]$source,
      target_rds_path = mappings[[type]]$target
    )
  }
  
  message(sprintf("--- Finished appending results for chunk '%s' ---\n", chunk_name))
  
  invisible(TRUE)
}
