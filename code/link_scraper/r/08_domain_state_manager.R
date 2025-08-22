# ==============================================================================
# MODULE: DOMAIN STATE MANAGER
# ==============================================================================
#
# This module acts as the central state memory for the scraper during a chunk
# run. It initializes, updates, and provides access to a tracker that monitors
# the real-time performance and status of each domain.
#
# The primary data structure, 'domain_tracker', holds information such as
# consecutive error counts, the reason for the last error, and how many
# sessions have been terminated (killed) for a domain. This allows other
# modules, particularly the Request Orchestrator (04), to make intelligent,
# state-based decisions.
#
# RECEIVES FROM:
# - 02_chunk_manager: A list of unique domains for the current chunk.
# - 07_response_analyzer: The analysis result of each HTTP response.
# - 04_request_orchestrator: Signals to update state (e.g., increment kill count).
#
# OUTPUTS TO:
# - Global Environment: Manages the 'domain_tracker' data.table.
# - 04_request_orchestrator: Provides domain state information on request.
#
# ==============================================================================

# Load required packages
library(data.table)

# 1. Function to initialize the domain state tracker for a new chunk run
func_08_initialize_tracker <- function(chunk_domains) {
  # This function creates the master data.table that will hold the state
  # for every unique domain within the current processing chunk. It is called
  # once at the beginning of a run.
  
  # Validate input: ensure chunk_domains is a character vector with actual domains
  if (!is.character(chunk_domains) || length(chunk_domains) == 0) {
    stop("Invalid input: 'chunk_domains' must be a character vector of domain names.")
  }
  
  # Create the data.table structure with initial default values
  domain_tracker_dt <- data.table(
    domain = chunk_domains,
    consecutive_error_count = 0L,           # Counter for identical, sequential errors. Resets on success or different error.
    last_error_reason = NA_character_,      # Stores the reason of the last error to check for consecutive failures.
    session_kill_count = 0L                 # Counter for how many sessions have been killed for this domain.
  )
  
  # Assign the newly created tracker to the global environment for access by other modules.
  assign("domain_tracker", domain_tracker_dt, envir = .GlobalEnv)
  
  message(sprintf("Domain state tracker initialized for %d domains.", length(chunk_domains)))
  invisible(TRUE)
}


# 2. Function to update a domain's state after a response is analyzed
func_08_update_domain_state <- function(domain_name, analysis_result) {
  # This function is the core of the state management. It is called by the
  # Response Analyzer (07) after every request. It implements the logic
  # for counting consecutive errors.
  
  # Check if the tracker exists in the global environment
  if (!exists("domain_tracker", envir = .GlobalEnv)) {
    warning("Domain tracker is not initialized. Skipping state update.")
    return(invisible(FALSE))
  }
  
  # Define which response statuses are considered errors for our counting logic.
  is_error <- analysis_result$action == "retry"
  current_error_reason <- analysis_result$response_analysis
  
  # Get the current state for the specific domain using the unambiguous argument name.
  last_reason <- domain_tracker[domain == domain_name, last_error_reason]
  
  if (is_error) {
    # --- Handle Error Case ---
    if (!is.na(last_reason) && last_reason == current_error_reason) {
      # If the last error is the same as the current one, increment the counter.
      domain_tracker[domain == domain_name, consecutive_error_count := consecutive_error_count + 1L]
    } else {
      # If it's a new type of error (or the first error), reset counter to 1 and update the reason.
      domain_tracker[domain == domain_name, consecutive_error_count := 1L]
      domain_tracker[domain == domain_name, last_error_reason := current_error_reason]
    }
  } else {
    # --- Handle Success Case ---
    # If the response was not an error, reset the error tracking state.
    domain_tracker[domain == domain_name, consecutive_error_count := 0L]
    domain_tracker[domain == domain_name, last_error_reason := NA_character_]
  }
  
  invisible(TRUE)
}


# 3. Function to retrieve the current state of a specific domain
func_08_get_domain_state <- function(domain_name) {
  # A simple getter function that provides the current state row for a given
  # domain. This is used by the Orchestrator (04) to make decisions.
  
  if (!exists("domain_tracker", envir = .GlobalEnv)) {
    stop("Domain tracker is not initialized. Cannot get domain state.")
  }
  
  # Retrieve the single, correct row for the requested domain.
  state_info <- domain_tracker[domain == domain_name]
  
  if (nrow(state_info) == 0) {
    warning(sprintf("Domain '%s' not found in tracker.", domain_name))
    return(NULL)
  }
  
  return(state_info)
}


# 4. Function to increment the session kill counter for a domain
func_08_increment_kill_count <- function(domain_name) {
  # This function is called by the Orchestrator (04) right after it decides
  # to kill a session, ensuring the state is consistent.
  
  if (!exists("domain_tracker", envir = .GlobalEnv)) {
    warning("Domain tracker is not initialized. Skipping kill count increment.")
    return(invisible(FALSE))
  }
  
  # Increment the session_kill_count by 1 for the specified domain.
  domain_tracker[domain == domain_name, session_kill_count := session_kill_count + 1L]
  
  # After killing a session, we must reset the error counter to give the new session a fresh start.
  domain_tracker[domain == domain_name, consecutive_error_count := 0L]
  domain_tracker[domain == domain_name, last_error_reason := NA_character_]
  
  message(sprintf("Session kill count for '%s' incremented.", domain_name))
  invisible(TRUE)
}
