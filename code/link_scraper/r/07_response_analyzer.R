# ==============================================================================
# MODULE: RESPONSE ANALYSIS & PATTERN RECOGNITION
# ==============================================================================
#
# This module analyzes HTTP responses to detect blocking patterns and classify
# response types. It identifies different types of blocks (soft blocks, hard
# blocks, rate limits), detects CAPTCHAs and JavaScript challenges, recognizes
# CloudFlare and other bot protection services, and analyzes response patterns
# across multiple requests. The analyzer provides intelligence about server
# behavior that helps the system adapt its approach and avoid detection by
# learning from response patterns.
#
# RECEIVES FROM:
# - 05_request_executor: Complete response object
#
# OUTPUTS TO:
# - 06_html_parser: Valid responses for parsing
# - 08_domain_state_manager: Updates the domain state after each analysis.
# - 10_log_manager: Response logs
# - retry queue: Failed responses for retry
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr2)

# Source the domain state manager 
source("08_domain_state_manager.R")


# Main response analyzer function
func_07_analyze_response <- function(response_result, chunk_name = current_chunk) {
  
  # Validate input
  if (!is.list(response_result)) {
    stop("Invalid response_result. Expected list from module 05.")
  }
  
  # Extract response object and request info
  httr2_response <- response_result$httr2_response
  request_info <- response_result$request_info
  
  # Initialize a variable to hold the final result of this function
  final_analysis_result <- list()
  
  # Check if request was successful at network level
  if (!response_result$success || is.null(httr2_response)) {
    # Case 1: Network error or timeout
    response_analysis <- "network_error"
    
    # Log the failed response
    func_10_log_response(
      response_result = response_result,
      chunk_name = chunk_name,
      response_analysis = response_analysis
    )
    
    # Add the failed request to the retry queue
    func_10_append_retry(
      retry_reason = response_analysis,
      request_info = request_info,
      chunk_name = chunk_name
    )
    
    # Prepare the final result object for this function
    final_analysis_result <- list(
      success = FALSE,
      action = "retry",
      response_analysis = response_analysis,
      message = "Network error occurred"
    )
    
    # Update the domain state tracker with the analysis result
    func_08_update_domain_state(request_info$domain, final_analysis_result)
    
    return(final_analysis_result)
  }
  
  # If we have a response, get the HTTP status code
  status_code <- tryCatch({
    resp_status(httr2_response)
  }, error = function(e) {
    NA_integer_
  })
  
  # Analyze based on status code
  response_analysis <- if (is.na(status_code)) {
    "invalid_response"
  } else if (status_code == 200) {
    "valid"
  } else {
    "non_200"
  }
  
  # Log the response with the initial analysis
  func_10_log_response(
    response_result = response_result,
    chunk_name = chunk_name,
    response_analysis = response_analysis
  )
  
  # Handle based on the response analysis
  if (response_analysis == "valid") {
    # Case 2: Valid 200 OK response, forward to parser
    parse_result <- func_06_parse_html(
      response_result = response_result,
      chunk_name      = chunk_name
    )
    
    # The parser itself can detect issues like bot pages, which are retryable.
    # We check the parser's output to make a final decision.
    if (isTRUE(parse_result$success)) {
      final_analysis_result <- list(
        success = TRUE,
        action = "parse",
        response_analysis = response_analysis,
        parse_result = parse_result,
        message = "Response successfully analyzed and parsed."
      )
    } else if (parse_result$reason == "bot_detected") {
      # If the parser found a bot page, we override the action to "retry".
      final_analysis_result <- list(
        success = FALSE,
        action = "retry",
        response_analysis = "bot_detected", # More specific reason
        parse_result = parse_result,
        message = "Parser detected a bot page."
      )
    } else {
      # For other parsing failures, the action remains "parse" but success is FALSE.
      final_analysis_result <- list(
        success = FALSE,
        action = "parse", # It was attempted, but failed. Not a network retry.
        response_analysis = response_analysis,
        parse_result = parse_result,
        message = "Response analyzed, but parsing failed."
      )
    }
    
  } else {
    # Case 3: Non-200 or invalid response, add to retry queue
    func_10_append_retry(
      retry_reason = response_analysis,
      request_info = request_info,
      chunk_name = chunk_name
    )
    
    # Prepare the final result object
    final_analysis_result <- list(
      success = FALSE,
      action = "retry",
      response_analysis = response_analysis,
      status_code = status_code,
      message = sprintf("Response analysis: %s (Status: %s)",
                        response_analysis,
                        ifelse(is.na(status_code), "NA", as.character(status_code)))
    )
  }
  
  # Update the domain state tracker with the final analysis result before returning
  func_08_update_domain_state(request_info$domain, final_analysis_result)
  
  return(final_analysis_result)
}
