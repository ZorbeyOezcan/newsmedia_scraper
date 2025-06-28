# ==============================================================================
# MODULE: HTTP REQUEST EXECUTION
# ==============================================================================
# 
# This module handles the actual HTTP request execution with all specified
# parameters. It sends HTTP requests using the provided identity parameters
# (user agent, headers, cookies), implements proper timeout and error handling,
# captures complete response data including headers and status codes, and
# measures request performance metrics. The module acts as the interface
# between the orchestration logic and the actual network communication,
# ensuring that all requests are executed exactly as specified by the
# orchestrator.
#
# RECEIVES FROM:
# - 04_request_orchestrator: Request package with URL, session, and parameters
# 
# OUTPUTS TO:
# - 07_response_analyzer: Complete response object with all metrics
# - 10_log_manager: Request log entries
#
# ==============================================================================

# Load required packages
library(httr2)
library(data.table)
library(lubridate)


# 1: Main function to execute HTTP request from request package
func_05_execute_request <- function(request_package, chunk_name = "chunk_01", worker_id = 1) {
  # Validate input request package
  if (!is.list(request_package) || !request_package$success) {
    # Even in this initial validation failure, we return the standardized list.
    # We construct a minimal request_info for logging purposes.
    return(list(
      success = FALSE,
      error_message = "Invalid or failed request package provided",
      httr2_response = NULL,
      session_updated = request_package$request_params$session %||% list(), # Safely get session
      request_info = list(
        request_id = NA_integer_,
        id = request_package$request_params$id %||% NA_integer_,
        url = request_package$request_params$url %||% NA_character_,
        domain = request_package$request_params$domain %||% NA_character_,
        session_id = request_package$request_params$session$id %||% NA_character_,
        user_agent_id = request_package$request_params$session$user_agent_id %||% NA_integer_,
        aggressiveness_level = request_package$request_params$aggressiveness %||% NA_integer_,
        request_timestamp = Sys.time()
      )
    ))
  }
  
  # Extract request parameters from package
  url <- request_package$request_params$url
  domain <- request_package$request_params$domain
  session <- request_package$request_params$session
  aggressiveness <- request_package$request_params$aggressiveness
  request_timestamp <- request_package$request_params$timestamp
  
  # Validate essential parameters
  if (is.null(url) || is.null(session) || is.null(session$headers)) {
    return(list(
      success = FALSE,
      error_message = "Missing essential request parameters (url, session, or headers)",
      httr2_response = NULL,
      session_updated = session,
      request_info = list(
        request_id = NA_integer_,
        id = request_package$request_params$id %||% NA_integer_,
        url = url %||% NA_character_,
        domain = domain %||% NA_character_,
        session_id = session$id %||% NA_character_,
        user_agent_id = session$user_agent_id %||% NA_integer_,
        aggressiveness_level = aggressiveness,
        request_timestamp = request_timestamp
      )
    ))
  }
  
  # Log request initiation
  message(sprintf("Executing request: %s", url))
  
  tryCatch({
    # Create httr2 request object
    req <- request(url)
    
    # Add user agent header
    req <- req_user_agent(req, session$headers$user_agent)
    
    # Add custom headers from session
    custom_headers <- list()
    
    # Add Accept header
    if (!is.null(session$headers$accept)) {
      custom_headers[["Accept"]] <- session$headers$accept
    }
    
    # Add Accept-Language header
    if (!is.null(session$headers$accept_language)) {
      custom_headers[["Accept-Language"]] <- session$headers$accept_language
    }
    
    # Add Accept-Encoding header
    if (!is.null(session$headers$accept_encoding)) {
      custom_headers[["Accept-Encoding"]] <- session$headers$accept_encoding
    }
    
    # Add Connection header
    if (!is.null(session$headers$connection)) {
      custom_headers[["Connection"]] <- session$headers$connection
    }
    
    # Add Referer header
    if (!is.null(session$headers$referer)) {
      custom_headers[["Referer"]] <- session$headers$referer
    }
    
    # Add Host header
    if (!is.null(session$headers$host)) {
      custom_headers[["Host"]] <- session$headers$host
    }
    
    # Add Upgrade-Insecure-Requests header (if present)
    if (!is.null(session$headers$upgrade_insecure_requests)) {
      custom_headers[["Upgrade-Insecure-Requests"]] <- session$headers$upgrade_insecure_requests
    }
    
    # Add Sec-Fetch headers
    if (!is.null(session$headers$sec_fetch_dest)) {
      custom_headers[["Sec-Fetch-Dest"]] <- session$headers$sec_fetch_dest
    }
    
    if (!is.null(session$headers$sec_fetch_mode)) {
      custom_headers[["Sec-Fetch-Mode"]] <- session$headers$sec_fetch_mode
    }
    
    if (!is.null(session$headers$sec_fetch_site)) {
      custom_headers[["Sec-Fetch-Site"]] <- session$headers$sec_fetch_site
    }
    
    # Apply all custom headers
    req <- req_headers(req, !!!custom_headers)
    
    # Configure cookie handling if cookie jar exists
    if (!is.null(session$cookie_jar)) {
      req <- req_cookie_preserve(req, session$cookie_jar)
    }
    
    # Disable automatic error handling for 4xx/5xx status codes
    # We want to handle all responses manually in module 07
    req <- req_error(req, is_error = ~ FALSE)
    
    # Log the request before execution
    request_id <- func_10_log_request(
      request_package = request_package,
      session = session,
      httr2_request = req,
      chunk_name = chunk_name,
      worker_id = worker_id
    )
    
    # Perform the actual HTTP request
    httr2_response <- req_perform(req)
    
    # Log successful execution
    message(sprintf("Request executed: %s (Status: %d)", 
                    url, resp_status(httr2_response)))
    
    return(list(
      success = TRUE,
      httr2_response = httr2_response,
      session_updated = session,
      request_info = list(
        request_id = request_id,
        id = request_package$request_params$id,
        url = url,
        domain = domain,
        session_id = session$id,
        user_agent_id = session$user_agent_id,
        aggressiveness_level = aggressiveness,
        request_timestamp = request_timestamp
      ),
      error_message = NA_character_ # Add NA error message on success
    ))
    
  }, error = function(e) {
    # Handle request errors (network failures, timeouts, etc.)
    message(sprintf("Request failed: %s (Error: %s)", url, e$message))
    
    # Log the failed request attempt
    request_id <- tryCatch({
      func_10_log_request(
        request_package = request_package,
        session = session,
        httr2_request = NULL,
        chunk_name = chunk_name,
        worker_id = worker_id
      )
    }, error = function(log_error) {
      warning(sprintf("Failed to log request: %s", log_error$message))
      NA_integer_
    })
    
    return(list(
      success = FALSE,
      error_message = as.character(e$message), # Rename 'error' to 'error_message'
      httr2_response = NULL,
      session_updated = session,
      request_info = list(
        request_id = request_id,
        id = request_package$request_params$id,
        url = url,
        domain = domain,
        session_id = session$id,
        user_agent_id = session$user_agent_id,
        aggressiveness_level = aggressiveness,
        request_timestamp = request_timestamp
      )
    ))
  })
}

# Helper operator for safe NULL fallbacks
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}
