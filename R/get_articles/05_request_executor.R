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
    return(list(
      success = FALSE,
      error_message = "Invalid or failed request package provided",
      httr2_response = NULL,
      session_updated = request_package$request_params$session %||% list(),
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
  
  message(sprintf("Executing request: %s", url))
  
  # Log the request BEFORE the attempt. The result of this is the definitive request_id.
  request_id <- func_10_log_request(
    request_package = request_package,
    session = session,
    httr2_request = NULL, # We don't have the full httr2_request object here yet, pass NULL
    chunk_name = chunk_name,
    worker_id = worker_id
  )
  
  tryCatch({
    # Create httr2 request object
    req <- request(url)
    
    # Add user agent header
    req <- req_user_agent(req, session$headers$user_agent)
    
    # ... (header assembly code remains the same) ...
    custom_headers <- list()
    if (!is.null(session$headers$accept)) custom_headers[["Accept"]] <- session$headers$accept
    if (!is.null(session$headers$accept_language)) custom_headers[["Accept-Language"]] <- session$headers$accept_language
    if (!is.null(session$headers$accept_encoding)) custom_headers[["Accept-Encoding"]] <- session$headers$accept_encoding
    if (!is.null(session$headers$connection)) custom_headers[["Connection"]] <- session$headers$connection
    if (!is.null(session$headers$referer)) custom_headers[["Referer"]] <- session$headers$referer
    if (!is.null(session$headers$host)) custom_headers[["Host"]] <- session$headers$host
    if (!is.null(session$headers$upgrade_insecure_requests)) custom_headers[["Upgrade-Insecure-Requests"]] <- session$headers$upgrade_insecure_requests
    if (!is.null(session$headers$sec_fetch_dest)) custom_headers[["Sec-Fetch-Dest"]] <- session$headers$sec_fetch_dest
    if (!is.null(session$headers$sec_fetch_mode)) custom_headers[["Sec-Fetch-Mode"]] <- session$headers$sec_fetch_mode
    if (!is.null(session$headers$sec_fetch_site)) custom_headers[["Sec-Fetch-Site"]] <- session$headers$sec_fetch_site
    req <- req_headers(req, !!!custom_headers)
    
    if (!is.null(session$cookie_jar)) {
      req <- req_cookie_preserve(req, session$cookie_jar)
    }
    
    req <- req_error(req, is_error = ~ FALSE)
    
    # Perform the actual HTTP request
    httr2_response <- req_perform(req)
    
    message(sprintf("Request executed: %s (Status: %d)", 
                    url, resp_status(httr2_response)))
    
    return(list(
      success = TRUE,
      httr2_response = httr2_response,
      session_updated = session,
      request_info = list(
        request_id = request_id, # Use the request_id generated before the tryCatch
        id = request_package$request_params$id,
        url = url,
        domain = domain,
        session_id = session$id,
        user_agent_id = session$user_agent_id,
        aggressiveness_level = aggressiveness,
        request_timestamp = request_timestamp
      ),
      error_message = NA_character_
    ))
    
  }, error = function(e) {
    # Handle request errors (network failures, timeouts, etc.)
    message(sprintf("Request failed: %s (Error: %s)", url, e$message))
    
    # FIX: The request has already been logged. We just need to return the failure package.
    # The request_id from the parent environment is automatically available here.
    return(list(
      success = FALSE,
      error_message = as.character(e$message),
      httr2_response = NULL,
      session_updated = session,
      request_info = list(
        request_id = request_id, # Use the request_id generated before the tryCatch
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
