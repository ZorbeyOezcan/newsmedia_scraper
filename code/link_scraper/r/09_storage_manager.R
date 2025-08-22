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
library(ggplot2)

# Configggplot2# Configuration Function

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



#####



# 3. Function to update the main input.rds file with the latest status
func_09_update_input <- function() {
  
  message("\n--- Updating 'input.rds' with latest processing status ---")
  
  paths <- get_module_paths()
  
  # --- Define file paths ---
  input_file <- file.path(paths$input, "input.rds")
  processed_file <- file.path(paths$output, "final_data.rds")
  retry_file <- file.path(paths$input, "retry.rds")
  error_file <- file.path(paths$output, "error.rds")
  
  # Paths for parse errors (main file and domain-specific directory)
  main_parse_error_file <- file.path(paths$input, "parse_error.rds")
  domain_parse_error_dir <- file.path(paths$input, "parse_error")
  
  # --- Load all necessary data files ---
  
  # Load master input file
  if (!file.exists(input_file)) stop("Master 'input.rds' not found.")
  input_dt <- readRDS(input_file)
  
  # Helper function to safely load a file and return an empty data.table if it doesn't exist
  .load_safe <- function(path) {
    if (file.exists(path)) {
      return(as.data.table(readRDS(path)))
    } else {
      warning(paste("File not found, creating empty table:", basename(path)))
      return(data.table(url = character())) # Return empty table with a 'url' column
    }
  }
  
  processed_dt <- .load_safe(processed_file)
  retry_dt <- .load_safe(retry_file)
  error_dt <- .load_safe(error_file)
  
  # --- Create hash sets for URL lookups ---
  
  processed_urls <- unique(processed_dt$url)
  retry_urls <- unique(retry_dt$url)
  error_urls <- unique(error_dt$url)
  
  # --- Collect all parse error URLs from both main file and domain-specific files ---
  
  # 1. Get URLs from the main parse_error.rds file
  main_parse_error_dt <- .load_safe(main_parse_error_file)
  all_parse_error_urls <- unique(main_parse_error_dt$url)
  
  # 2. Get URLs from all files in the domain-specific parse_error directory
  if (dir.exists(domain_parse_error_dir)) {
    domain_files <- list.files(domain_parse_error_dir, pattern = "\\.rds$", full.names = TRUE)
    
    if (length(domain_files) > 0) {
      # Use lapply to read all files and extract URLs, then combine them
      domain_urls_list <- lapply(domain_files, function(file) {
        dt <- readRDS(file)
        return(dt$url)
      })
      # Combine all URL vectors into one
      all_domain_urls <- unlist(domain_urls_list)
      # Append to the main list
      all_parse_error_urls <- c(all_parse_error_urls, all_domain_urls)
    }
  }
  
  # 3. Create a final, unique set of all parse error URLs
  parse_error_urls <- unique(all_parse_error_urls)
  
  # --- Update the master input data.table by reference ---
  
  # 1. Update 'processed' flag
  processed_updates <- sum(input_dt[url %in% processed_urls, processed] == FALSE, na.rm = TRUE)
  input_dt[url %in% processed_urls, processed := TRUE]
  message(sprintf("Marked %d links as 'processed'.", processed_updates))
  
  # 2. Update 'retry' flag
  retry_updates <- sum(input_dt[url %in% retry_urls, retry] == FALSE, na.rm = TRUE)
  input_dt[url %in% retry_urls, retry := TRUE]
  message(sprintf("Marked %d links for 'retry'.", retry_updates))
  
  # 3. Update 'error' flag
  error_updates <- sum(input_dt[url %in% error_urls, error] == FALSE, na.rm = TRUE)
  input_dt[url %in% error_urls, error := TRUE]
  message(sprintf("Marked %d links as 'error'.", error_updates))
  
  # 4. Update 'parse_error' flag using the comprehensive URL list
  parse_error_updates <- sum(input_dt[url %in% parse_error_urls, parse_error] == FALSE, na.rm = TRUE)
  input_dt[url %in% parse_error_urls, parse_error := TRUE]
  message(sprintf("Marked %d links as 'parse_error' (checked main file and domain-specific files).", parse_error_updates))
  
  # --- Save the updated data.table back to input.rds ---
  saveRDS(input_dt, input_file)
  
  message("\n--- Master 'input.rds' has been successfully updated. ---")
  
  invisible(TRUE)
}



#####



# 4. Function to generate a comprehensive overview report of the scraping status
func_09_generate_overview_report <- function(plot_summary = FALSE) {
  
  message("\n--- Generating scraping overview report ---")
  
  paths <- get_module_paths()
  
  # Helper function to safely load RDS files
  .load_safe <- function(path, required_cols = "domain") {
    if (!file.exists(path)) {
      warning(sprintf("File not found: '%s'. Returning empty table.", basename(path)))
      empty_dt <- data.table()
      # Create empty columns of type character to avoid binding issues
      for (col in required_cols) empty_dt[, (col) := character()]
      return(empty_dt)
    }
    dt <- as.data.table(readRDS(path))
    # Ensure required columns exist, adding them if they don't
    for (col in required_cols) {
      if (!col %in% names(dt)) dt[, (col) := NA_character_]
    }
    return(dt)
  }
  
  # Load all necessary permanent data files
  input_dt <- .load_safe(file.path(paths$input, "input.rds"), "domain")
  output_dt <- .load_safe(file.path(paths$output, "final_data.rds"), "domain")
  request_log_dt <- .load_safe(file.path(paths$logs, "request_log.rds"), "domain")
  response_log_dt <- .load_safe(file.path(paths$logs, "response_log.rds"), "domain")
  retry_dt <- .load_safe(file.path(paths$input, "retry.rds"), c("domain", "retry_reason"))
  error_dt <- .load_safe(file.path(paths$output, "error.rds"), "domain")
  parse_error_dt <- .load_safe(file.path(paths$input, "parse_error.rds"), "domain")
  
  # Calculate counts per domain
  input_counts <- input_dt[, .(total_links_input = .N), by = domain]
  output_counts <- output_dt[, .(total_processed = .N), by = domain]
  request_counts <- request_log_dt[, .(total_requests = .N), by = domain]
  response_counts <- response_log_dt[, .(total_responses = .N), by = domain]
  retry_counts <- retry_dt[, .(total_retries = .N), by = domain]
  error_counts <- error_dt[, .(total_error_links = .N), by = domain]
  parse_error_counts <- parse_error_dt[, .(total_parse_error_links = .N), by = domain]
  bot_detection_counts <- retry_dt[retry_reason == "bot_detected", .(total_bot_detections = .N), by = domain]
  
  # Get a list of all unique domains from the input file
  all_domains <- unique(input_dt[, .(domain)])
  
  # Merge all counts into a single overview table
  overview_dt <- merge(all_domains, input_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, output_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, request_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, response_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, retry_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, error_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, parse_error_counts, by = "domain", all.x = TRUE)
  overview_dt <- merge(overview_dt, bot_detection_counts, by = "domain", all.x = TRUE)
  
  # Replace NA values with 0 for all numeric columns
  numeric_cols <- names(which(sapply(overview_dt, is.numeric)))
  for (col in numeric_cols) {
    set(overview_dt, which(is.na(overview_dt[[col]])), col, 0)
  }
  
  # Calculate success and error rates
  overview_dt[, processing_success_rate := ifelse(total_links_input > 0, total_processed / total_links_input, 0)]
  
  # Calculate successful requests (responses that did not lead to a retry)
  overview_dt[, total_successful_requests := total_responses - total_retries]
  overview_dt[total_successful_requests < 0, total_successful_requests := 0] # Ensure non-negative
  
  overview_dt[, request_success_rate := ifelse(total_requests > 0, total_successful_requests / total_requests, 0)]
  overview_dt[, error_rate := ifelse(total_requests > 0, total_error_links / total_requests, 0)]
  overview_dt[, parse_error_rate := ifelse(total_requests > 0, total_parse_error_links / total_requests, 0)]
  overview_dt[, bot_detection_rate := ifelse(total_requests > 0, total_bot_detections / total_requests, 0)]
  
  # Format rates as percentages for better readability in the table
  rate_cols <- names(overview_dt)[grep("_rate$", names(overview_dt))]
  for (col in rate_cols) {
    overview_dt[, (col) := paste0(round(get(col) * 100, 2), "%")]
  }
  
  message("Successfully generated overview table.")
  
  # Plotting logic
  if (plot_summary) {
    message("Generating summary plot...")
    
    # Calculate overall totals
    totals <- overview_dt[, .(
      Processed = sum(total_processed),
      Errors = sum(total_error_links),
      "Parse Errors" = sum(total_parse_error_links),
      "Bot Detections" = sum(total_bot_detections),
      "Other Retries" = sum(total_retries) - sum(total_bot_detections)
    )]
    
    # Melt data for plotting
    plot_data <- melt(totals, measure.vars = names(totals), variable.name = "Category", value.name = "Count")
    
    # Create the plot
    p <- ggplot(plot_data, aes(x = reorder(Category, -Count), y = Count, fill = Category)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = scales::comma(Count)), vjust = -0.5, size = 4) +
      scale_y_continuous(labels = scales::comma) +
      labs(
        title = "Overall Scraping Status",
        subtitle = "Total counts across all domains",
        x = "",
        y = "Number of Links"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    print(p)
  }
  
  return(overview_dt)
}



#####


# 5. Function to clean duplicates in parse errors, input and output - just to make sure. 
func_09_clean_dupes <- function() {
  
  message(paste(rep("=", 80), collapse = ""))
  message("--- Starting Data Cleaning and Deduplication Process ---")
  message(paste(rep("=", 80), collapse = ""))
  
  paths <- get_module_paths()
  
  # --- 1. special DEDUPLICATION FOR final_data.rds ---
  
  final_data_path <- file.path(paths$output, "final_data.rds")
  if (file.exists(final_data_path)) {
    message("\n--- Processing: final_data.rds ---")
    dt <- readRDS(final_data_path)
    initial_rows <- nrow(dt)
    
    # Find URLs that have duplicates
    dupe_urls <- dt[, .N, by = url][N > 1, url]
    
    if (length(dupe_urls) > 0) {
      # Separate unique entries from duplicates
      unique_dt <- dt[!url %in% dupe_urls]
      dupes_dt <- dt[url %in% dupe_urls]
      
      # Define columns to check for NAs and merge
      merge_cols <- c("date_time", "author", "headline", "text")
      
      # Process each group of duplicates
      merged_dupes <- dupes_dt[, {
        # Count NAs in the key columns for each row
        na_counts <- rowSums(is.na(.SD[, ..merge_cols]))
        
        # If NA counts are different, keep the row with the fewest NAs
        if (length(unique(na_counts)) > 1) {
          .SD[which.min(na_counts)]
        } else {
          # If NA counts are the same, merge them
          # For each column, take the first non-NA value found
          merged_row <- lapply(.SD, function(col) {
            first_valid <- first(na.omit(col))
            if (is.null(first_valid)) NA else first_valid
          })
          as.data.table(merged_row)
        }
      }, by = url]
      
      # Combine the unique entries with the newly merged duplicates
      dt <- rbindlist(list(unique_dt, merged_dupes), use.names = TRUE, fill = TRUE)
    }
    
    duplicates_removed <- initial_rows - nrow(dt)
    message(sprintf("  -> Found and resolved %d duplicate entries.", duplicates_removed))
    saveRDS(dt, final_data_path)
  } else {
    message("\n--- Skipping final_data.rds: File not found. ---")
  }
  
  # --- 2. SIMPLE DEDUPLICATION FOR input.rds - this really shouldn't happen but just in case: 
  
  input_path <- file.path(paths$input, "input.rds")
  if (file.exists(input_path)) {
    message("\n--- Processing: input.rds ---")
    dt <- readRDS(input_path)
    initial_rows <- nrow(dt)
    
    # Keep only the first entry for each unique URL
    dt <- unique(dt, by = "url")
    
    duplicates_removed <- initial_rows - nrow(dt)
    message(sprintf("  -> Found and removed %d duplicate entries.", duplicates_removed))
    saveRDS(dt, input_path)
  } else {
    message("\n--- Skipping input.rds: File not found. ---")
  }
  
  # --- 3. PROCESS ALL PARSE_ERROR FILES - this is a redundancy to module 12 but just in case 
  
  parse_error_dir <- file.path(paths$input, "parse_error")
  if (dir.exists(parse_error_dir)) {
    message("\n--- Processing domain-specific parse_error files ---")
    parse_error_files <- list.files(parse_error_dir, pattern = "\\.rds$", full.names = TRUE)
    
    # Load the cleaned final_data.rds to get a list of successful URLs
    successful_urls <- if (file.exists(final_data_path)) {
      readRDS(final_data_path)$url
    } else {
      character(0)
    }
    
    for (file_path in parse_error_files) {
      message(sprintf("  -> Processing: %s", basename(file_path)))
      dt <- readRDS(file_path)
      initial_rows <- nrow(dt)
      
      # Step 3a: Internal deduplication
      dt <- unique(dt, by = "url")
      internal_dupes_removed <- initial_rows - nrow(dt)
      if (internal_dupes_removed > 0) {
        message(sprintf("     - Removed %d internal duplicates.", internal_dupes_removed))
      }
      
      # Step 3b: Cross-file deduplication against final_data.rds
      if (length(successful_urls) > 0) {
        rows_before_cross_check <- nrow(dt)
        dt <- dt[!url %in% successful_urls]
        cross_dupes_removed <- rows_before_cross_check - nrow(dt)
        if (cross_dupes_removed > 0) {
          message(sprintf("     - Removed %d entries already present in final_data.rds.", cross_dupes_removed))
        }
      }
      
      saveRDS(dt, file_path)
    }
  } else {
    message("\n--- Skipping parse_error directory: Directory not found. ---")
  }
  
  message(paste(rep("=", 80), collapse = ""))
  message("--- Data Cleaning and Deduplication Process Finished ---")
  message(paste(rep("=", 80), collapse = ""))
  
  return(invisible(TRUE))
}


####



# function to process and split the parse_error.rds file
func_09_process_parse_errors <- function() {
  
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
# func_09_process_parse_errors()