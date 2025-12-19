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


# 1. Function to log requests
func_10_log_request <- function(request_package,
                                session,
                                httr2_request = NULL,
                                chunk_name    = current_chunk,
                                worker_id     = 1) {
  # verify
  if (!is.list(request_package) || !request_package$success) {
    warning("Invalid request package provided to logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001' etc.")
  }
  
  # locate / create the chunk-specific data.table
  dt_name <- paste0(chunk_name, "_request_log")
  if (exists(dt_name, envir = .GlobalEnv)) {
    chunk_log <- get(dt_name, envir = .GlobalEnv)
  } else {
    chunk_log <- data.table()
  }
  
  # More robust way to derive the next request_id
  last_id <- if (nrow(chunk_log) > 0) {
    max(chunk_log$request_id, na.rm = TRUE)
  } else {
    0L
  }
  if (!is.finite(last_id)) {
    last_id <- 0L
  }
  request_id <- last_id + 1L
  
  # obtain IP from VPN log 
  vpn_log_path <- file.path(get_module_paths()$logs, "03_vpn_log.rds")
  current_ip   <- if (file.exists(vpn_log_path)) {
    vpn_log <- readRDS(vpn_log_path)
    vpn_log[order(last_used, decreasing = TRUE)][1]$ip_address
  } else NA_character_
  
  headers      <- if (is.null(session) || is.null(session$headers)) list() else session$headers
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # create new log entry
  log_entry <- data.table(
    request_id              = as.integer(request_id),
    id                      = ifelse(is.null(request_package$request_params$id), NA_integer_, as.integer(request_package$request_params$id)),
    domain                  = ifelse(is.null(request_package$request_params$domain), NA_character_, as.character(request_package$request_params$domain)),
    url                     = ifelse(is.null(request_package$request_params$url), NA_character_, as.character(request_package$request_params$url)),
    timestamp_scraped       = Sys.time(),
    from_chunk              = as.integer(chunk_number),
    session_id              = ifelse(is.null(session$id), NA_character_, as.character(session$id)),
    worker_id               = as.integer(worker_id),
    user_agent_id           = ifelse(is.null(session$user_agent_id), NA_integer_, as.integer(session$user_agent_id)),
    ip_address              = ifelse(is.null(current_ip), NA_character_, as.character(current_ip)),
    user_agent              = ifelse(is.null(headers$user_agent), NA_character_, as.character(headers$user_agent)),
    accept                  = ifelse(is.null(headers$accept), NA_character_, as.character(headers$accept)),
    accept_language         = ifelse(is.null(headers$accept_language), NA_character_, as.character(headers$accept_language)),
    accept_encoding         = ifelse(is.null(headers$accept_encoding), NA_character_, as.character(headers$accept_encoding)),
    connection              = ifelse(is.null(headers$connection), NA_character_, as.character(headers$connection)),
    referer                 = ifelse(is.null(headers$referer), NA_character_, as.character(headers$referer)),
    host                    = ifelse(is.null(headers$host), NA_character_, as.character(headers$host)),
    upgrade_insecure_requests = ifelse(is.null(headers$upgrade_insecure_requests), NA_character_, as.character(headers$upgrade_insecure_requests)),
    sec_fetch_dest          = ifelse(is.null(headers$sec_fetch_dest), NA_character_, as.character(headers$sec_fetch_dest)),
    sec_fetch_mode          = ifelse(is.null(headers$sec_fetch_mode), NA_character_, as.character(headers$sec_fetch_mode)),
    sec_fetch_site          = ifelse(is.null(headers$sec_fetch_site), NA_character_, as.character(headers$sec_fetch_site)),
    aggressiveness_level    = ifelse(is.null(request_package$request_params$aggressiveness), NA_integer_, as.integer(request_package$request_params$aggressiveness)),
    browser_type            = ifelse(is.null(session$browser_type), NA_character_, as.character(session$browser_type)),
    is_mobile               = ifelse(is.null(session$is_mobile), NA, as.logical(session$is_mobile)),
    is_first_request        = ifelse(is.null(session$first_request), FALSE, as.logical(session$first_request)),
    session_request_count   = ifelse(is.null(session$request_count), 0L, as.integer(session$request_count)),
    cookie_jar_path         = ifelse(is.null(session$cookie_jar), NA_character_, as.character(session$cookie_jar))
  )
  
  # Harmonize column structure
  all_cols <- union(names(chunk_log), names(log_entry))
  for (col in setdiff(all_cols, names(chunk_log)))  chunk_log[, (col) := NA]
  for (col in setdiff(all_cols, names(log_entry)))  log_entry[, (col) := NA]
  setcolorder(chunk_log, all_cols)
  setcolorder(log_entry, all_cols)
  
  # Append and re-assign
  chunk_log <- rbindlist(list(chunk_log, log_entry), use.names = TRUE, fill = TRUE)
  assign(dt_name, chunk_log, envir = .GlobalEnv)
  
  invisible(request_id)
}


# 2. Function to log HTTP response parameters
func_10_log_response <- function(response_result,
                                 chunk_name        = current_chunk,
                                 response_analysis = NA_character_) {
  if (!is.list(response_result)) {
    warning("Invalid response result provided to logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_123', …")
  }
  
  # Helper function to ensure values are length 1, defaulting to NA.
  .sanitize_log_value <- function(value, default_na) {
    if (is.null(value) || length(value) == 0) {
      return(default_na)
    }
    return(value[1]) # Return only the first element
  }
  
  dt_name   <- paste0(chunk_name, "_response_log")
  chunk_log <- if (exists(dt_name, envir = .GlobalEnv)) get(dt_name, envir = .GlobalEnv) else data.table()
  
  last_id <- if (nrow(chunk_log) > 0) max(chunk_log$response_id, na.rm = TRUE) else 0L
  if (!is.finite(last_id)) last_id <- 0L
  response_id <- last_id + 1L
  
  request_info <- response_result$request_info
  request_id   <- ifelse(is.null(request_info$request_id), NA_integer_, request_info$request_id)
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # Initialize all variables with NA defaults
  status_code <- NA_integer_; response_headers <- list(); server_date <- as.POSIXct(NA)
  content_type <- NA_character_; content_length <- NA_integer_; server <- NA_character_
  dns_time <- NA_real_; connect_time <- NA_real_; total_time <- NA_real_
  
  if (response_result$success && !is.null(response_result$httr2_response)) {
    resp <- response_result$httr2_response
    
    # Use the sanitizer for every value extracted from the response object
    status_code    <- .sanitize_log_value(tryCatch(resp_status(resp), error = function(e) NULL), NA_integer_)
    response_headers <- tryCatch(as.list(resp_headers(resp)), error = function(e) list(error = e$message))
    content_type   <- .sanitize_log_value(tryCatch(resp_content_type(resp), error = function(e) NULL), NA_character_)
    content_length <- .sanitize_log_value(tryCatch(as.integer(resp_header(resp, "content-length")), error = function(e) NULL), NA_integer_)
    server         <- .sanitize_log_value(tryCatch(as.character(resp_header(resp, "server")), error = function(e) NULL), NA_character_)
    server_date    <- .sanitize_log_value(tryCatch(httr::parse_http_date(resp_header(resp, "date")), error = function(e) NULL), as.POSIXct(NA))
    
    if (!is.null(resp$times) && is.numeric(resp$times)) {
      dns_time     <- .sanitize_log_value(resp$times[["namelookup"]], NA_real_)
      connect_time <- .sanitize_log_value(resp$times[["connect"]], NA_real_)
      total_time   <- .sanitize_log_value(resp$times[["total"]], NA_real_)
    }
  }
  
  vpn_log_path <- file.path(get_module_paths()$logs, "03_vpn_log.rds")
  current_ip   <- if (file.exists(vpn_log_path)) {
    readRDS(vpn_log_path)[order(last_used, decreasing = TRUE)][1]$ip_address
  } else NA_character_
  
  log_entry <- data.table(
    request_id, response_id,
    id = request_info$id %||% NA_integer_,
    domain = request_info$domain %||% NA_character_,
    url = request_info$url %||% NA_character_,
    timestamp_scraped = Sys.time(),
    from_chunk = chunk_number,
    status_code, response_headers = list(response_headers), response_time = total_time,
    server_date, content_type, content_length, server,
    user_agent_id = request_info$user_agent_id %||% NA_integer_,
    ip_address = current_ip, dns_time, connect_time, total_time,
    curl_error_code = if(!response_result$success) 1L else 0L,
    ssl_verify_result = NA_integer_, redirect_count = NA_integer_,
    rate_limit_remaining = NA_integer_, rate_limit_reset = as.POSIXct(NA),
    retry_after = NA_integer_, response_analysis
  )
  
  all_cols <- union(names(chunk_log), names(log_entry))
  for (col in setdiff(all_cols, names(chunk_log))) chunk_log[, (col) := NA]
  for (col in setdiff(all_cols, names(log_entry))) log_entry[, (col) := NA]
  setcolorder(chunk_log, all_cols); setcolorder(log_entry, all_cols)
  
  chunk_log <- rbindlist(list(chunk_log, log_entry), use.names = TRUE, fill = TRUE)
  assign(dt_name, chunk_log, envir = .GlobalEnv)
  
  invisible(response_id)
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# 3. Function to append successfully parsed articles
func_10_append_output <- function(processed_dt, chunk_name = current_chunk) {
  if (!is.data.table(processed_dt) || nrow(processed_dt) == 0) return(invisible(FALSE))
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) stop("Invalid chunk_name format")
  
  dt_name <- paste0(chunk_name, "_output")
  chunk_output <- if (exists(dt_name, envir = .GlobalEnv)) get(dt_name, envir = .GlobalEnv) else data.table()
  
  output_entry <- processed_dt[, .(id, domain, url, timestamp_scraped, date_time, author, headline, text, paywall)]
  
  chunk_output <- rbind(chunk_output, output_entry, fill = TRUE)
  assign(dt_name, chunk_output, envir = .GlobalEnv)
  
  invisible(TRUE)
}

# 4. Function to append retry entry 
func_10_append_retry <- function(retry_reason, request_info, chunk_name = current_chunk) {
  if (is.null(retry_reason) || !is.list(request_info)) return(invisible(FALSE))
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) stop("Invalid chunk_name format")
  
  dt_name <- paste0(chunk_name, "_retry")
  chunk_retry <- if (exists(dt_name, envir = .GlobalEnv)) get(dt_name, envir = .GlobalEnv) else data.table()
  
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  response_log_name <- paste0(chunk_name, "_response_log")
  
  response_entry <- if (!is.na(request_info$request_id) && exists(response_log_name, envir = .GlobalEnv)) {
    get(response_log_name, envir = .GlobalEnv)[request_id == request_info$request_id][.N]
  } else { NULL }
  
  retry_entry <- data.table(
    request_id = request_info$request_id %||% NA_integer_,
    response_id = if (!is.null(response_entry)) response_entry$response_id else NA_integer_,
    id = request_info$id %||% NA_integer_,
    domain = request_info$domain %||% NA_character_,
    url = request_info$url %||% NA_character_,
    timestamp_scraped = request_info$timestamp_scraped %||% Sys.time(),
    from_chunk = chunk_number,
    retry_reason = retry_reason
    # All other columns will be NA by default, which is acceptable for a retry log.
  )
  
  chunk_retry <- rbind(chunk_retry, retry_entry, fill = TRUE)
  assign(dt_name, chunk_retry, envir = .GlobalEnv)
  
  invisible(TRUE)
}

# 5. Function to append errors
func_10_append_error <- function(error_reason, processed_dt, chunk_name = current_chunk) {
  if (is.null(error_reason) || !is.data.table(processed_dt) || nrow(processed_dt) == 0) return(invisible(FALSE))
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) stop("Invalid chunk_name format")
  
  dt_name <- paste0(chunk_name, "_error")
  chunk_error <- if (exists(dt_name, envir = .GlobalEnv)) get(dt_name, envir = .GlobalEnv) else data.table()
  
  error_entry <- processed_dt[, .(id, domain, url)]
  error_entry[, error_reason := error_reason]
  
  chunk_error <- rbind(chunk_error, error_entry, fill = TRUE)
  assign(dt_name, chunk_error, envir = .GlobalEnv)
  
  invisible(TRUE)
}

# 6. Function to append parse errors
func_10_append_parse_error <- function(processed_dt, html_content, chunk_name = current_chunk) {
  if (!is.data.table(processed_dt) || is.null(html_content) || nrow(processed_dt) == 0) return(invisible(FALSE))
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) stop("Invalid chunk_name format")
  
  dt_name <- paste0(chunk_name, "_parse_error")
  chunk_parse_error <- if (exists(dt_name, envir = .GlobalEnv)) get(dt_name, envir = .GlobalEnv) else data.table()
  
  parse_error_entry <- processed_dt[, .(id, domain, url, timestamp_scraped, date_time, author, headline, text, paywall)]
  parse_error_entry[, html_content := as.character(html_content)]
  
  chunk_parse_error <- rbind(chunk_parse_error, parse_error_entry, fill = TRUE)
  assign(dt_name, chunk_parse_error, envir = .GlobalEnv)
  
  invisible(TRUE)
}

