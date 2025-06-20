# ==============================================================================
# MODULE: CENTRALIZED LOGGING SYSTEM
# ==============================================================================
# 
# This module provides unified logging functionality for the entire system.
# It captures all events, errors, and metrics from every module, maintains
# structured log files with consistent formatting, provides real-time console
# output for monitoring, aggregates statistics for performance analysis, and
# ensures that all system behavior is traceable for debugging. The logger is
# essential for monitoring system health and troubleshooting issues.
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr2)

# Configuration Function
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input         = file.path(base_path, "data", "input"),
    output        = file.path(base_path, "data", "output"),
    config        = file.path(base_path, "data", "config"),
    state         = file.path(base_path, "data", "state"),
    logs          = file.path(base_path, "data", "logs"),
    chunk_logs    = file.path(base_path, "data", "logs",  "chunk_logs"),
    chunk_outputs = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs  = file.path(base_path, "data", "input",  "chunk_inputs")
  )
}


# 1. Function to log HTTP request parameters: 
func_10_log_request <- function(request_package, session, httr2_request = NULL, chunk_name = "chunk_01", worker_id = 1) {
  
  # Validate inputs
  if (!is.list(request_package) || !request_package$success) {
    warning("Invalid request package provided to logger")
    return(invisible(FALSE))
  }
  
  # Define directories
  master_log_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs"
  chunk_logs_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/chunk_logs"
  
  # Create chunk logs directory if it doesn't exist
  if (!dir.exists(chunk_logs_dir)) {
    dir.create(chunk_logs_dir, recursive = TRUE)
  }
  
  # Define file paths
  master_request_log_path <- file.path(master_log_dir, "request_log.rds")
  chunk_request_log_path <- file.path(chunk_logs_dir, paste0(chunk_name, "_request_log.rds"))
  
  # Get current VPN IP address
  vpn_log_path <- file.path(master_log_dir, "vpn_log.rds")
  current_ip <- if (file.exists(vpn_log_path)) {
    vpn_log <- readRDS(vpn_log_path)
    # Get the most recently used VPN IP
    vpn_log[order(last_used, decreasing = TRUE)][1]$ip_address
  } else {
    NA_character_
  }
  
  # Generate unique request_id based on master log
  request_id <- 1L  # Default for first request ever
  
  if (file.exists(master_request_log_path)) {
    master_log <- readRDS(master_request_log_path)
    if (nrow(master_log) > 0) {
      # Get the highest request_id and increment
      request_id <- max(master_log$request_id, na.rm = TRUE) + 1L
    }
  }
  
  # If chunk log exists, check its highest ID too
  if (file.exists(chunk_request_log_path)) {
    chunk_log <- readRDS(chunk_request_log_path)
    if (nrow(chunk_log) > 0) {
      chunk_max_id <- max(chunk_log$request_id, na.rm = TRUE)
      # Use the higher of the two
      request_id <- max(request_id, chunk_max_id + 1L)
    }
  }
  
  # Extract chunk number from chunk name
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # Extract all header information from session
  headers <- session$headers
  
  # Create the log entry with all request parameters
  log_entry <- data.table(
    # Core identification
    request_id = request_id,
    id = request_package$request_params$id,
    domain = request_package$request_params$domain,
    url = request_package$request_params$url,
    timestamp_scraped = Sys.time(),
    from_chunk = chunk_number,
    
    # Session and worker info
    session_id = session$id,
    worker_id = worker_id,
    user_agent_id = session$user_agent_id,
    ip_address = current_ip,
    
    # All header information as separate columns
    user_agent = headers$user_agent,
    accept = headers$accept,
    accept_language = headers$accept_language,
    accept_encoding = headers$accept_encoding,
    connection = headers$connection,
    referer = headers$referer,
    host = headers$host,
    upgrade_insecure_requests = ifelse(is.null(headers$upgrade_insecure_requests), 
                                       NA_character_, 
                                       headers$upgrade_insecure_requests),
    sec_fetch_dest = headers$sec_fetch_dest,
    sec_fetch_mode = headers$sec_fetch_mode,
    sec_fetch_site = headers$sec_fetch_site,
    
    # Additional request metadata
    aggressiveness_level = request_package$request_params$aggressiveness,
    browser_type = session$browser_type,
    is_mobile = session$is_mobile,
    is_first_request = ifelse(is.null(session$first_request), 
                              FALSE, 
                              session$first_request),
    session_request_count = ifelse(is.null(session$request_count), 
                                   0L, 
                                   session$request_count),
    cookie_jar_path = session$cookie_jar
  )
  
  # Load existing chunk log or create new
  if (file.exists(chunk_request_log_path)) {
    chunk_log <- readRDS(chunk_request_log_path)
    # Append new entry
    chunk_log <- rbind(chunk_log, log_entry, fill = TRUE)
  } else {
    # Create new chunk log
    chunk_log <- log_entry
  }
  
  # Save updated chunk log
  saveRDS(chunk_log, chunk_request_log_path)
  
  # Log to console for monitoring
  message(sprintf("[REQUEST LOG] ID: %d | Domain: %s | Session: %s | UA: %d", 
                  request_id, 
                  request_package$request_params$domain,
                  session$id,
                  session$user_agent_id))
  
  # Return the request_id for reference
  return(invisible(request_id))
}

# Function to merge chunk request log into master log
func_10_merge_request_logs <- function(chunk_name) {
  
  # Define directories
  master_log_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs"
  chunk_logs_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/chunk_logs"
  
  # Define file paths
  master_request_log_path <- file.path(master_log_dir, "request_log.rds")
  chunk_request_log_path <- file.path(chunk_logs_dir, paste0(chunk_name, "_request_log.rds"))
  
  # Check if chunk log exists
  if (!file.exists(chunk_request_log_path)) {
    warning(sprintf("Chunk request log not found: %s", chunk_request_log_path))
    return(invisible(FALSE))
  }
  
  # Read chunk log
  chunk_log <- readRDS(chunk_request_log_path)
  
  if (nrow(chunk_log) == 0) {
    message("Chunk request log is empty, nothing to merge")
    return(invisible(TRUE))
  }
  
  # Load existing master log
  if (file.exists(master_request_log_path)) {
    master_log <- readRDS(master_request_log_path)
    
    # Check for duplicate request_ids before merging
    duplicate_ids <- intersect(master_log$request_id, chunk_log$request_id)
    if (length(duplicate_ids) > 0) {
      warning(sprintf("Found %d duplicate request_ids, removing from chunk log", 
                      length(duplicate_ids)))
      chunk_log <- chunk_log[!request_id %in% duplicate_ids]
    }
    
    # Merge logs
    master_log <- rbind(master_log, chunk_log, fill = TRUE)
  } else {
    # This should not happen as master log is created in module 01
    warning("Master request log not found, creating new one")
    master_log <- chunk_log
  }
  
  # Sort by request_id for consistency
  setorder(master_log, request_id)
  
  # Save updated master log
  saveRDS(master_log, master_request_log_path)
  
  # Print summary
  message(sprintf("Merged %d requests from %s into master log", 
                  nrow(chunk_log), 
                  chunk_name))
  message(sprintf("Master log now contains %d total requests", 
                  nrow(master_log)))
  
  # Keep chunk log in place for analysis
  
  return(invisible(TRUE))
}



#####



# 2. Function to log HTTP response parameters
func_10_log_response <- function(response_result, chunk_name = "chunk_01", response_analysis = NA_character_) {
  
  # Validate inputs
  if (!is.list(response_result)) {
    warning("Invalid response result provided to logger")
    return(invisible(FALSE))
  }
  
  # Define directories
  master_log_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs"
  chunk_logs_dir <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/chunk_logs"
  
  # Create chunk logs directory if it doesn't exist
  if (!dir.exists(chunk_logs_dir)) {
    dir.create(chunk_logs_dir, recursive = TRUE)
  }
  
  # Define file paths
  master_response_log_path <- file.path(master_log_dir, "response_log.rds")
  chunk_response_log_path <- file.path(chunk_logs_dir, paste0(chunk_name, "_response_log.rds"))
  
  # Extract request info
  request_info <- response_result$request_info
  
  # Get request_id from request_info
  request_id <- ifelse(is.null(request_info$request_id), 
                       NA_integer_, 
                       request_info$request_id)
  
  # Generate unique response_id based on master log
  response_id <- 1L  # Default for first response ever
  
  if (file.exists(master_response_log_path)) {
    master_log <- readRDS(master_response_log_path)
    if (nrow(master_log) > 0) {
      # Get the highest response_id and increment
      response_id <- max(master_log$response_id, na.rm = TRUE) + 1L
    }
  }
  
  # If chunk log exists, check its highest ID too
  if (file.exists(chunk_response_log_path)) {
    chunk_log <- readRDS(chunk_response_log_path)
    if (nrow(chunk_log) > 0) {
      chunk_max_id <- max(chunk_log$response_id, na.rm = TRUE)
      # Use the higher of the two
      response_id <- max(response_id, chunk_max_id + 1L)
    }
  }
  
  # Extract chunk number from chunk name
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # Initialize variables with default values
  status_code <- NA_integer_
  response_headers <- list()
  response_time <- NA_real_
  server_date <- as.POSIXct(NA)
  content_type <- NA_character_
  content_length <- NA_integer_
  server <- NA_character_
  rate_limit_remaining <- NA_integer_
  rate_limit_reset <- as.POSIXct(NA)
  retry_after <- NA_integer_
  
  # Extract response data if request was successful
  if (response_result$success && !is.null(response_result$httr2_response)) {
    resp <- response_result$httr2_response
    
    # Get status code
    status_code <- tryCatch(resp_status(resp), error = function(e) NA_integer_)
    
    # Get all response headers
    response_headers <- tryCatch(resp_headers(resp), error = function(e) list())
    
    # Extract specific headers
    content_type <- tryCatch(resp_content_type(resp), error = function(e) NA_character_)
    
    # Get content length from headers or calculate
    content_length <- tryCatch({
      cl <- resp_header(resp, "content-length")
      if (!is.null(cl)) as.integer(cl) else NA_integer_
    }, error = function(e) NA_integer_)
    
    # Get server header
    server <- tryCatch({
      srv <- resp_header(resp, "server")
      if (!is.null(srv)) srv else NA_character_
    }, error = function(e) NA_character_)
    
    # Get server date
    server_date <- tryCatch({
      date_str <- resp_header(resp, "date")
      if (!is.null(date_str)) {
        parse_http_date(date_str)
      } else {
        as.POSIXct(NA)
      }
    }, error = function(e) as.POSIXct(NA))
    
    # Get rate limit headers if present
    rate_limit_remaining <- tryCatch({
      rl <- resp_header(resp, "x-ratelimit-remaining")
      if (!is.null(rl)) as.integer(rl) else NA_integer_
    }, error = function(e) NA_integer_)
    
    rate_limit_reset <- tryCatch({
      reset <- resp_header(resp, "x-ratelimit-reset")
      if (!is.null(reset)) {
        as.POSIXct(as.integer(reset), origin = "1970-01-01")
      } else {
        as.POSIXct(NA)
      }
    }, error = function(e) as.POSIXct(NA))
    
    # Get retry-after header if present
    retry_after <- tryCatch({
      ra <- resp_header(resp, "retry-after")
      if (!is.null(ra)) as.integer(ra) else NA_integer_
    }, error = function(e) NA_integer_)
    
    # Extract timing information from httr2 response
    # Note: httr2 doesn't provide detailed timing like curl, so we estimate
    response_time <- tryCatch({
      # If response has timing info, use it
      if (!is.null(resp$times)) {
        as.numeric(resp$times$total)
      } else {
        NA_real_
      }
    }, error = function(e) NA_real_)
  }
  
  # Get current VPN IP address
  vpn_log_path <- file.path(master_log_dir, "vpn_log.rds")
  current_ip <- if (file.exists(vpn_log_path)) {
    vpn_log <- readRDS(vpn_log_path)
    # Get the most recently used VPN IP
    vpn_log[order(last_used, decreasing = TRUE)][1]$ip_address
  } else {
    NA_character_
  }
  
  # Create the log entry with all response parameters
  log_entry <- data.table(
    # IDs and core info
    request_id = request_id,
    response_id = response_id,
    id = ifelse(is.null(request_info$id), NA_integer_, request_info$id),
    domain = ifelse(is.null(request_info$domain), NA_character_, request_info$domain),
    url = ifelse(is.null(request_info$url), NA_character_, request_info$url),
    timestamp_scraped = Sys.time(),
    from_chunk = chunk_number,
    
    # Response details
    status_code = status_code,
    response_headers = list(response_headers),  # Store as list column
    response_time = response_time,
    server_date = server_date,
    content_type = content_type,
    content_length = content_length,
    server = server,
    
    # Request context
    user_agent_id = ifelse(is.null(request_info$user_agent_id), 
                           NA_integer_, 
                           request_info$user_agent_id),
    ip_address = current_ip,
    
    # Timing details (simplified for httr2)
    dns_time = NA_real_,        # httr2 doesn't provide this level of detail
    connect_time = NA_real_,    # httr2 doesn't provide this level of detail
    total_time = response_time, # Use response_time as total_time
    
    # Error and SSL info
    curl_error_code = ifelse(!response_result$success, 1L, 0L),
    ssl_verify_result = NA_integer_,  # httr2 handles SSL automatically
    redirect_count = NA_integer_,     # Could be extracted from response history
    
    # Rate limiting
    rate_limit_remaining = rate_limit_remaining,
    rate_limit_reset = rate_limit_reset,
    retry_after = retry_after,
    
    # Response analysis result from module 07
    response_analysis = response_analysis
  )
  
  # Load existing chunk log or create new
  if (file.exists(chunk_response_log_path)) {
    chunk_log <- readRDS(chunk_response_log_path)
    # Append new entry
    chunk_log <- rbind(chunk_log, log_entry, fill = TRUE)
  } else {
    # Create new chunk log
    chunk_log <- log_entry
  }
  
  # Save updated chunk log
  saveRDS(chunk_log, chunk_response_log_path)
  
  # Log to console for monitoring
  message(sprintf("[RESPONSE LOG] Req: %d | Resp: %d | Status: %s | Domain: %s", 
                  request_id,
                  response_id,
                  ifelse(is.na(status_code), "ERROR", as.character(status_code)),
                  request_info$domain))
  
  # Return the response_id for reference
  return(invisible(response_id))
}

# Helper function to parse HTTP date headers
parse_http_date <- function(date_string) {
  # HTTP date format: "Wed, 21 Oct 2015 07:28:00 GMT"
  tryCatch({
    as.POSIXct(date_string, format = "%a, %d %b %Y %H:%M:%S", tz = "GMT")
  }, error = function(e) {
    as.POSIXct(NA)
  })
}



#####



# 3. 