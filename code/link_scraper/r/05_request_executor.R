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
#
# ==============================================================================

# Load required packages
library(httr2)
library(data.table)
library(lubridate)

# Configuration Function
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input = file.path(base_path, "data", "input"),
    output = file.path(base_path, "data", "output"),
    config = file.path(base_path, "data", "config"),
    state = file.path(base_path, "data", "state"),
    logs = file.path(base_path, "data", "logs")
  )
}

# 1: Main function to execute HTTP request from request package
func_05_execute_request <- function(request_package) {
  # Validate input request package
  if (!is.list(request_package) || !request_package$success) {
    return(list(
      success = FALSE,
      error = "Invalid or failed request package",
      httr2_response = NULL
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
      error = "Missing essential request parameters",
      httr2_response = NULL
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
    
    # Perform the actual HTTP request
    httr2_response <- req_perform(req)
    
    # Log successful execution
    message(sprintf("Request executed: %s (Status: %d)", 
                    url, resp_status(httr2_response)))
    
    # Return complete httr2 response object for module 07
    return(list(
      success = TRUE,
      httr2_response = httr2_response,
      session_updated = session,
      request_info = list(
        url = url,
        domain = domain,
        session_id = session$id,
        user_agent_id = session$user_agent_id,
        aggressiveness_level = aggressiveness,
        request_timestamp = request_timestamp
      )
    ))
    
  }, error = function(e) {
    # Handle request errors (network failures, timeouts, etc.)
    message(sprintf("Request failed: %s (Error: %s)", url, e$message))
    
    return(list(
      success = FALSE,
      error = as.character(e$message),
      httr2_response = NULL,
      session_updated = session,
      request_info = list(
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