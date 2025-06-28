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
# - 10_log_manager: Response logs
# - retry queue: Failed responses for retry
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr2)


# Main response analyzer function
func_07_analyze_response <- function(response_result, chunk_name = current_chunk) {
  
  # Validate input
  if (!is.list(response_result)) {
    stop("Invalid response_result. Expected list from module 05.")
  }
  
  # Extract response object and request info
  httr2_response <- response_result$httr2_response
  request_info <- response_result$request_info
  
  # Initialize response analysis
  response_analysis <- NA_character_
  
  # Check if request was successful at network level
  if (!response_result$success || is.null(httr2_response)) {
    # Network error or timeout
    response_analysis <- "network_error"
    
    # Log the failed response
    func_10_log_response(
      response_result = response_result,
      chunk_name = chunk_name,
      response_analysis = response_analysis
    )
    
    # This aligns the function call with the updated signature in 10_log_manager.R.
    func_10_append_retry(
      retry_reason = response_analysis,
      request_info = request_info,
      chunk_name = chunk_name
    )
    
    # Return failure result
    return(list(
      success = FALSE,
      action = "retry",
      response_analysis = response_analysis,
      message = "Network error occurred"
    ))
  }
  
  # Get HTTP status code
  status_code <- tryCatch({
    resp_status(httr2_response)
  }, error = function(e) {
    NA_integer_
  })
  
  # Analyze based on status code
  if (is.na(status_code)) {
    # Could not extract status code
    response_analysis <- "invalid_response"
    
  } else if (status_code == 200) {
    # Success response
    response_analysis <- "valid"
    
  } else {
    # Non-200 response
    response_analysis <- "non_200"
  }
  
  # Log the response with analysis
  func_10_log_response(
    response_result = response_result,
    chunk_name = chunk_name,
    response_analysis = response_analysis
  )
  
  # Handle based on response analysis
  if (response_analysis == "valid") {
    # Extract HTML content for parser
    html_content <- tryCatch({
      resp_body_string(httr2_response)
    }, error = function(e) {
      NULL
    })
    
    # Check if HTML content was extracted successfully
    if (is.null(html_content) || nchar(html_content) == 0) {
      # Empty response body
      response_analysis <- "empty_response"
      
      # Update log with new analysis
      # (Note: This re-logs the response, which might be intended or could be optimized later)
      # For now, we keep the logic but fix the subsequent call.
      
      func_10_append_retry(
        retry_reason = response_analysis,
        request_info = request_info,
        chunk_name = chunk_name
      )
      
      return(list(
        success = FALSE,
        action = "retry",
        response_analysis = response_analysis,
        message = "Empty response body"
      ))
    }
    
    # Valid response with content - forward to HTML parser
    parse_result <- func_06_parse_html(
      response_result = response_result,   
      chunk_name      = chunk_name         
    )
    
    # Return success result with parse data
    return(list(
      success = TRUE,
      action = "parse",
      response_analysis = response_analysis,
      parse_result = parse_result,
      message = "Response successfully analyzed and forwarded to parser"
    ))
    
  } else {
    # Non-200 or invalid response - add to retry queue
    func_10_append_retry(
      retry_reason = response_analysis,
      request_info = request_info,
      chunk_name = chunk_name
    )
    
    # Return failure result
    return(list(
      success = FALSE,
      action = "retry",
      response_analysis = response_analysis,
      status_code = status_code,
      message = sprintf("Response analysis: %s (Status: %s)", 
                        response_analysis, 
                        ifelse(is.na(status_code), "NA", as.character(status_code)))
    ))
  }
}
