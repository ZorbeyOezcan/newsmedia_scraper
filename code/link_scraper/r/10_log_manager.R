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
    chunk_log <- data.table()  # empty → columns will be created on first bind
  }
  
  # derive new request_id 
  request_id <- if (nrow(chunk_log)) max(chunk_log$request_id, na.rm = TRUE) + 1L else 1L
  
  # obtain IP from VPN log (optional) 
  vpn_log_path <- file.path(get_module_paths()$logs, "vpn_log.rds")
  current_ip   <- if (file.exists(vpn_log_path)) {
    vpn_log <- readRDS(vpn_log_path)
    vpn_log[order(last_used, decreasing = TRUE)][1]$ip_address
  } else NA_character_
  
  # convenience  
  headers      <- if (is.null(session) || is.null(session$headers)) list() else session$headers
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # create new log entry  
  log_entry <- data.table(
    request_id              = as.integer(request_id),
    id                      = ifelse(is.null(request_package$request_params$id),
                                     NA_character_, as.character(request_package$request_params$id)),
    domain                  = ifelse(is.null(request_package$request_params$domain),
                                     NA_character_, as.character(request_package$request_params$domain)),
    url                     = ifelse(is.null(request_package$request_params$url),
                                     NA_character_, as.character(request_package$request_params$url)),
    timestamp_scraped       = Sys.time(),
    from_chunk              = as.integer(chunk_number),
    session_id              = ifelse(is.null(session$id),
                                     NA_character_, as.character(session$id)),
    worker_id               = as.integer(worker_id),
    user_agent_id           = ifelse(is.null(session$user_agent_id),
                                     NA_integer_, as.integer(session$user_agent_id)),
    ip_address              = ifelse(is.null(current_ip),
                                     NA_character_, as.character(current_ip)),
    user_agent              = ifelse(is.null(headers$user_agent),
                                     NA_character_, as.character(headers$user_agent)),
    accept                  = ifelse(is.null(headers$accept),
                                     NA_character_, as.character(headers$accept)),
    accept_language         = ifelse(is.null(headers$accept_language),
                                     NA_character_, as.character(headers$accept_language)),
    accept_encoding         = ifelse(is.null(headers$accept_encoding),
                                     NA_character_, as.character(headers$accept_encoding)),
    connection              = ifelse(is.null(headers$connection),
                                     NA_character_, as.character(headers$connection)),
    referer                 = ifelse(is.null(headers$referer),
                                     NA_character_, as.character(headers$referer)),
    host                    = ifelse(is.null(headers$host),
                                     NA_character_, as.character(headers$host)),
    upgrade_insecure_requests =
      ifelse(is.null(headers$upgrade_insecure_requests),
             NA_character_, as.character(headers$upgrade_insecure_requests)),
    sec_fetch_dest          = ifelse(is.null(headers$sec_fetch_dest),
                                     NA_character_, as.character(headers$sec_fetch_dest)),
    sec_fetch_mode          = ifelse(is.null(headers$sec_fetch_mode),
                                     NA_character_, as.character(headers$sec_fetch_mode)),
    sec_fetch_site          = ifelse(is.null(headers$sec_fetch_site),
                                     NA_character_, as.character(headers$sec_fetch_site)),
    aggressiveness_level    = ifelse(is.null(request_package$request_params$aggressiveness),
                                     NA_integer_, as.integer(request_package$request_params$aggressiveness)),
    browser_type            = ifelse(is.null(session$browser_type),
                                     NA_character_, as.character(session$browser_type)),
    is_mobile               = ifelse(is.null(session$is_mobile),
                                     NA, as.logical(session$is_mobile)),
    is_first_request        = ifelse(is.null(session$first_request),
                                     FALSE, as.logical(session$first_request)),
    session_request_count   = ifelse(is.null(session$request_count),
                                     0L, as.integer(session$request_count)),
    cookie_jar_path         = ifelse(is.null(session$cookie_jar),
                                     NA_character_, as.character(session$cookie_jar))
  )
  
  ## harmonise column structure & classes 
  all_cols <- union(names(chunk_log), names(log_entry))
  
  # add missing columns to each data.table
  for (col in setdiff(all_cols, names(chunk_log)))  chunk_log[,  (col) := NA_character_]
  for (col in setdiff(all_cols, names(log_entry)))  log_entry[, (col) := NA_character_]
  
  # bring both tables to identical column order
  data.table::setcolorder(chunk_log, all_cols)
  data.table::setcolorder(log_entry, all_cols)
  
  # unify classes (fallback: character)  
  for (col in all_cols) {
    if (!identical(class(chunk_log[[col]]), class(log_entry[[col]]))) {
      chunk_log[, (col) := as.character(get(col))]
      log_entry[, (col) := as.character(get(col))]
    }
  }

  
  # append and re-assign  
  chunk_log <- data.table::rbindlist(list(chunk_log, log_entry),
                                     use.names = TRUE, fill = TRUE)
  assign(dt_name, chunk_log, envir = .GlobalEnv)
  
  invisible(request_id)
}



#####



# 2. Function to log HTTP response parameters
func_10_log_response <- function(response_result,
                                 chunk_name       = current_chunk,
                                 response_analysis = NA_character_) {
  # validation 
  if (!is.list(response_result)) {
    warning("Invalid response result provided to logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_123', …")
  }
  
  # locate current log table 
  dt_name   <- paste0(chunk_name, "_response_log")
  chunk_log <- if (exists(dt_name, envir = .GlobalEnv))
    get(dt_name, envir = .GlobalEnv) else data.table()
  
  # next response_id
  response_id <- if (nrow(chunk_log)) max(chunk_log$response_id, na.rm = TRUE) + 1L else 1L
  
  # request context 
  request_info <- response_result$request_info
  request_id   <- ifelse(is.null(request_info$request_id), NA_integer_, request_info$request_id)
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # defaults 
  status_code           <- NA_integer_
  response_headers      <- list()
  response_time         <- NA_real_
  server_date           <- as.POSIXct(NA)
  content_type          <- NA_character_
  content_length        <- NA_integer_
  server                <- NA_character_
  rate_limit_remaining  <- NA_integer_
  rate_limit_reset      <- as.POSIXct(NA)
  retry_after           <- NA_integer_
  
  # extract data when successful 
  if (response_result$success && !is.null(response_result$httr2_response)) {
    resp <- response_result$httr2_response
    
    status_code      <- tryCatch(resp_status(resp),       error = function(e) NA_integer_)
    response_headers <- tryCatch(resp_headers(resp),      error = function(e) list())
    content_type     <- tryCatch(resp_content_type(resp), error = function(e) NA_character_)
    content_length   <- tryCatch({ cl <- resp_header(resp, "content-length");
    if (!is.null(cl)) as.integer(cl) else NA_integer_ },
    error = function(e) NA_integer_)
    server           <- tryCatch({ srv <- resp_header(resp, "server");
    if (!is.null(srv)) srv else NA_character_ },
    error = function(e) NA_character_)
    server_date      <- tryCatch({ date_str <- resp_header(resp, "date");
    if (!is.null(date_str)) parse_http_date(date_str) else as.POSIXct(NA) },
    error = function(e) as.POSIXct(NA))
    rate_limit_remaining <- tryCatch({ rl <- resp_header(resp, "x-ratelimit-remaining");
    if (!is.null(rl)) as.integer(rl) else NA_integer_ },
    error = function(e) NA_integer_)
    rate_limit_reset     <- tryCatch({ rst <- resp_header(resp, "x-ratelimit-reset");
    if (!is.null(rst)) as.POSIXct(as.integer(rst), origin="1970-01-01") else as.POSIXct(NA) },
    error = function(e) as.POSIXct(NA))
    retry_after          <- tryCatch({ ra <- resp_header(resp, "retry-after");
    if (!is.null(ra)) as.integer(ra) else NA_integer_ },
    error = function(e) NA_integer_)
    response_time        <- tryCatch(if (!is.null(resp$times)) as.numeric(resp$times$total) else NA_real_,
                                     error = function(e) NA_real_)
  }
  
  # VPN IP lookup 
  vpn_log_path <- file.path(get_module_paths()$logs, "vpn_log.rds")
  current_ip   <- if (file.exists(vpn_log_path)) {
    vpn_log <- readRDS(vpn_log_path)
    vpn_log[order(last_used, decreasing = TRUE)][1]$ip_address
  } else NA_character_
  
  # assemble entry 
  log_entry <- data.table(
    request_id           = request_id,
    response_id          = response_id,
    id                   = ifelse(is.null(request_info$id), NA_integer_, request_info$id),
    domain               = ifelse(is.null(request_info$domain), NA_character_, request_info$domain),
    url                  = ifelse(is.null(request_info$url), NA_character_, request_info$url),
    timestamp_scraped    = Sys.time(),
    from_chunk           = chunk_number,
    status_code          = status_code,
    response_headers     = list(response_headers),
    response_time        = response_time,
    server_date          = server_date,
    content_type         = content_type,
    content_length       = content_length,
    server               = server,
    user_agent_id        = ifelse(is.null(request_info$user_agent_id), NA_integer_, request_info$user_agent_id),
    ip_address           = current_ip,
    dns_time             = NA_real_,
    connect_time         = NA_real_,
    total_time           = response_time,
    curl_error_code      = ifelse(!response_result$success, 1L, 0L),
    ssl_verify_result    = NA_integer_,
    redirect_count       = NA_integer_,
    rate_limit_remaining = rate_limit_remaining,
    rate_limit_reset     = rate_limit_reset,
    retry_after          = retry_after,
    response_analysis    = response_analysis
  )
  
  # harmonise
  all_cols <- union(names(chunk_log), names(log_entry))
  
  # add missing
  for (col in setdiff(all_cols, names(chunk_log)))  chunk_log[,  (col) := NA_character_]
  for (col in setdiff(all_cols, names(log_entry)))  log_entry[, (col) := NA_character_]
  
  # same order 
  data.table::setcolorder(chunk_log, all_cols)
  data.table::setcolorder(log_entry, all_cols)
  
  # use same var type 
  for (col in all_cols) {
    if (!identical(class(chunk_log[[col]]), class(log_entry[[col]]))) {
      chunk_log[,  (col) := as.character(get(col))]
      log_entry[, (col) := as.character(get(col))]
    }
  }
  
  # rbind 
  chunk_log <- data.table::rbindlist(
    list(chunk_log, log_entry),
    use.names = TRUE, fill = TRUE
  )
  
  assign(dt_name, chunk_log, envir = .GlobalEnv)
  
  invisible(response_id)
}



#####



# 3. Function to append successfully parsed articles to chunk output
func_10_append_output <- function(parse_result,
                                  input_info,
                                  response_info,
                                  chunk_name = current_chunk) {
  # Validate inputs
  if (!is.list(parse_result)) {
    warning("Invalid parse result provided to output logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_002', etc.")
  }
  
  # Locate chunk-specific output data.table
  dt_name <- paste0(chunk_name, "_output")
  if (exists(dt_name, envir = .GlobalEnv)) {
    chunk_output <- get(dt_name, envir = .GlobalEnv)
  } else {
    chunk_output <- data.table()  # Empty table, columns will be created on first bind
  }
  
  # Extract timestamp from response info or use current time
  timestamp_scraped <- if (!is.null(response_info$server_date) && !is.na(response_info$server_date)) {
    response_info$server_date
  } else {
    Sys.time()
  }
  
  # Create new output entry
  output_entry <- data.table(
    id                = input_info$id,
    domain            = input_info$domain,
    url               = input_info$url,
    timestamp_scraped = timestamp_scraped,
    date_time         = parse_result$date_time,
    author            = parse_result$author,
    headline          = parse_result$headline,
    text              = parse_result$text,
    paywall           = parse_result$paywall
  )
  
  # Append and reassign to global environment
  chunk_output <- rbind(chunk_output, output_entry, fill = TRUE)
  assign(dt_name, chunk_output, envir = .GlobalEnv)
  
  invisible(TRUE)
}



##### 



# 4. Function to append retry entry 
func_10_append_retry <- function(retry_reason,
                                 url,
                                 chunk_name = current_chunk) {
  # Validate inputs
  if (is.null(retry_reason) || is.null(url)) {
    warning("Invalid retry reason or URL provided to retry logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_002', etc.")
  }
  
  # Locate chunk-specific retry data.table
  dt_name <- paste0(chunk_name, "_retry")
  if (exists(dt_name, envir = .GlobalEnv)) {
    chunk_retry <- get(dt_name, envir = .GlobalEnv)
  } else {
    chunk_retry <- data.table()  # Empty table, columns will be created on first bind
  }
  
  # Extract chunk number from chunk name
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  
  # Get request log and response log data.tables
  request_log_name <- paste0(chunk_name, "_request_log")
  response_log_name <- paste0(chunk_name, "_response_log")
  
  # Find matching entries in logs by URL
  request_entry <- if (exists(request_log_name, envir = .GlobalEnv)) {
    request_log <- get(request_log_name, envir = .GlobalEnv)
    request_log[url == url][.N]  # Get last matching entry
  } else {
    NULL
  }
  
  response_entry <- if (exists(response_log_name, envir = .GlobalEnv)) {
    response_log <- get(response_log_name, envir = .GlobalEnv)
    response_log[url == url][.N]  # Get last matching entry
  } else {
    NULL
  }
  
  # Extract values with safe defaults
  request_id <- ifelse(!is.null(request_entry) && nrow(request_entry) > 0, 
                       request_entry$request_id, NA_integer_)
  response_id <- ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                        response_entry$response_id, NA_integer_)
  
  # Create new retry entry
  retry_entry <- data.table(
    request_id           = request_id,       
    response_id          = response_id,      
    id                   = ifelse(!is.null(request_entry) && nrow(request_entry) > 0, 
                                  request_entry$id, NA_integer_),
    domain               = ifelse(!is.null(request_entry) && nrow(request_entry) > 0, 
                                  request_entry$domain, NA_character_),
    url                  = url,
    timestamp_scraped    = ifelse(!is.null(request_entry) && nrow(request_entry) > 0, 
                                  request_entry$timestamp_scraped, Sys.time()),
    from_chunk           = chunk_number,
    status_code          = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$status_code, NA_integer_),
    response_headers     = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$response_headers, list()),
    response_body        = NA_character_,  # Not stored in response log
    response_time        = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$response_time, NA_real_),
    retry_reason         = retry_reason,
    server_date          = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$server_date, as.POSIXct(NA)),
    content_type         = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$content_type, NA_character_),
    content_length       = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$content_length, NA_integer_),
    server               = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$server, NA_character_),
    user_agent_id        = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$user_agent_id, NA_integer_),
    ip_address           = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$ip_address, NA_character_),
    dns_time             = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$dns_time, NA_real_),
    connect_time         = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$connect_time, NA_real_),
    total_time           = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$total_time, NA_real_),
    curl_error_code      = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$curl_error_code, NA_integer_),
    ssl_verify_result    = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$ssl_verify_result, NA_integer_),
    redirect_count       = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$redirect_count, NA_integer_),
    rate_limit_remaining = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$rate_limit_remaining, NA_integer_),
    rate_limit_reset     = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$rate_limit_reset, as.POSIXct(NA)),
    retry_after          = ifelse(!is.null(response_entry) && nrow(response_entry) > 0, 
                                  response_entry$retry_after, NA_integer_)
  )
  
  # harmonize 
  all_cols <- union(names(chunk_retry), names(retry_entry))
  
  for (col in setdiff(all_cols, names(chunk_retry)))  chunk_retry[,  (col) := NA_character_]
  for (col in setdiff(all_cols, names(retry_entry)))  retry_entry[, (col) := NA_character_]
  
  data.table::setcolorder(chunk_retry,  all_cols)
  data.table::setcolorder(retry_entry, all_cols)
  
  for (col in all_cols) {
    if (!identical(class(chunk_retry[[col]]), class(retry_entry[[col]]))) {
      chunk_retry[,  (col) := as.character(get(col))]
      retry_entry[, (col) := as.character(get(col))]
    }
  }
  
  # bind 
  chunk_retry <- data.table::rbindlist(
    list(chunk_retry, retry_entry),
    use.names = TRUE, fill = TRUE
  )
  
  # assign to global
  assign(dt_name, chunk_retry, envir = .GlobalEnv)
  
  invisible(TRUE)
}



#####



# 5. Function to append non-article links to error dataset
func_10_append_error <- function(error_reason,
                                 input_info,
                                 chunk_name = current_chunk) {
  # Validate inputs
  if (is.null(error_reason) || !is.list(input_info)) {
    warning("Invalid error reason or input info provided to error logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_002', etc.")
  }
  
  # Locate chunk-specific error data.table
  dt_name <- paste0(chunk_name, "_error")
  if (exists(dt_name, envir = .GlobalEnv)) {
    chunk_error <- get(dt_name, envir = .GlobalEnv)
  } else {
    chunk_error <- data.table()  # Empty table, columns will be created on first bind
  }
  
  # Create new error entry
  error_entry <- data.table(
    id           = input_info$id,
    domain       = input_info$domain,
    url          = input_info$url,
    error_reason = error_reason
  )
  
  # Append and reassign to global environment
  chunk_error <- rbind(chunk_error, error_entry, fill = TRUE)
  assign(dt_name, chunk_error, envir = .GlobalEnv)
  

  invisible(TRUE)
}



##### 



# 6. Function to append articles with parsing errors to parse_error dataset
func_10_append_parse_error <- function(parse_result,
                                       input_info,
                                       html_content,
                                       chunk_name = current_chunk) {
  # Validate inputs
  if (!is.list(parse_result) || !is.list(input_info) || is.null(html_content)) {
    warning("Invalid parse result, input info, or HTML content provided to parse error logger")
    return(invisible(FALSE))
  }
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("chunk_name must look like 'chunk_001', 'chunk_002', etc.")
  }
  
  # Locate chunk-specific parse_error data.table
  dt_name <- paste0(chunk_name, "_parse_error")
  if (exists(dt_name, envir = .GlobalEnv)) {
    chunk_parse_error <- get(dt_name, envir = .GlobalEnv)
  } else {
    chunk_parse_error <- data.table()  # Empty table, columns will be created on first bind
  }
  
  # Create new parse error entry
  parse_error_entry <- data.table(
    id                = input_info$id,
    domain            = input_info$domain,
    url               = input_info$url,
    timestamp_scraped = Sys.time(),
    date_time         = ifelse(is.null(parse_result$date_time), NA_character_, parse_result$date_time),
    author            = ifelse(is.null(parse_result$author), NA_character_, parse_result$author),
    headline          = ifelse(is.null(parse_result$headline), NA_character_, parse_result$headline),
    text              = ifelse(is.null(parse_result$text), NA_character_, parse_result$text),
    paywall           = ifelse(is.null(parse_result$paywall), NA, parse_result$paywall),
    html_content      = as.character(html_content)  # Store complete HTML for later reprocessing
  )
  
  # Append and reassign to global environment
  chunk_parse_error <- rbind(chunk_parse_error, parse_error_entry, fill = TRUE)
  assign(dt_name, chunk_parse_error, envir = .GlobalEnv)
  
  # Determine which fields are missing for console feedback
  missing_fields <- c()
  if (is.na(parse_error_entry$date_time)) missing_fields <- c(missing_fields, "date_time")
  if (is.na(parse_error_entry$author)) missing_fields <- c(missing_fields, "author")
  if (is.na(parse_error_entry$headline)) missing_fields <- c(missing_fields, "headline")
  if (is.na(parse_error_entry$text)) missing_fields <- c(missing_fields, "text")
  
  invisible(TRUE)
}



##### 



