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
  
  # --- STEP 1: Determine the final status of the request FIRST ---
  
  if (!response_result$success || is.null(httr2_response)) {
    # Case 1: Network error or timeout. This is a final decision.
    final_analysis_result <- list(
      success = FALSE,
      action = "retry",
      response_analysis = "network_error",
      message = "Network error occurred"
    )
    
  } else {
    # Case 2: We have a response from the server.
    status_code <- tryCatch(resp_status(httr2_response), error = function(e) NA_integer_)
    
    if (is.na(status_code)) {
      # The response object is invalid.
      final_analysis_result <- list(
        success = FALSE,
        action = "retry",
        response_analysis = "invalid_response",
        message = "Could not read status code from response."
      )
      
    } else if (status_code == 200) {
      # Case 2a: Status is 200 OK. Now we must parse it to get the TRUE final status.
      parse_result <- func_06_parse_html(
        response_result = response_result,
        chunk_name      = chunk_name
      )
      
      if (isTRUE(parse_result$success)) {
        # The parser succeeded. This is a genuine success.
        final_analysis_result <- list(
          success = TRUE,
          action = "parse",
          response_analysis = "valid",
          parse_result = parse_result,
          message = "Response successfully analyzed and parsed."
        )
      } else if (parse_result$reason == "bot_detected") {
        # The parser found a bot page. The final status is "bot_detected".
        final_analysis_result <- list(
          success = FALSE,
          action = "retry",
          response_analysis = "bot_detected",
          parse_result = parse_result,
          message = "Parser detected a bot page."
        )
      } else {
        # Any other parser failure.
        final_analysis_result <- list(
          success = FALSE,
          action = "parse", # It was attempted, but failed.
          response_analysis = parse_result$reason %||% "parsing_failed", # Use specific reason from parser
          parse_result = parse_result,
          message = "Response analyzed, but parsing failed."
        )
      }
      
    } else {
      # Case 2b: Status is not 200 (e.g., 403, 404, 500). This is a retryable error.
      final_analysis_result <- list(
        success = FALSE,
        action = "retry",
        response_analysis = paste0("http_error_", status_code), # More specific reason
        status_code = status_code,
        message = sprintf("Received non-200 status code: %d", status_code)
      )
    }
  }
  
  # --- STEP 2: Log and store based on the FINAL decision ---
  
  # Log every response with its final, accurate analysis result.
  func_10_log_response(
    response_result = response_result,
    chunk_name = chunk_name,
    response_analysis = final_analysis_result$response_analysis
  )
  
  # If the final action is 'retry', add it to the retry queue.
  if (final_analysis_result$action == "retry") {
    func_10_append_retry(
      retry_reason = final_analysis_result$response_analysis,
      request_info = request_info,
      chunk_name = chunk_name
    )
  }
  
  # --- STEP 3: Update domain state and return ---
  
  # Update the domain state tracker with the final, consolidated result.
  func_08_update_domain_state(request_info$domain, final_analysis_result)
  
  return(final_analysis_result)
}

# Helper operator for safe NULL fallbacks
`%||%` <- function(a, b) {
  if (is.null(a)) b else a
}

