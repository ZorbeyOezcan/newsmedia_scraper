# ==============================================================================
# MODULE: REQUEST ORCHESTRATION & STATE-BASED DECISION MAKING
# ==============================================================================
#
# This module serves as the central control system for all scraping requests.
# It has been upgraded to be the primary decision-maker, using state information
# from the Domain State Manager (08) to dynamically adapt its strategy.
#
# Before each request, it queries the domain's current status (error counts,
# blocks) and decides the appropriate action:
#   1. PROCEED: Prepare and dispatch the request with the current session.
#   2. ROTATE: Kill the current session due to repeated failures and switch to a new one.
#   3. BLOCK: Halt all further requests to a domain for the remainder of the chunk run.
#
# ==============================================================================

# Load required packages
library(data.table)

# Source dependencies from other modules to ensure all required functions are available.
# This makes the module self-contained in terms of its dependencies, assuming the
# sourced files are in the correct working directory.


# 1. Function to initialize session pool for all domains
func_04_initialize_session_pool <- function() {
  paths <- get_module_paths()
  input_path <- file.path(paths$input, "input.rds")
  if (!file.exists(input_path)) stop("Input file not found: ", input_path)
  input_dt <- readRDS(input_path)
  unique_domains <- unique(input_dt$domain)
  unique_domains <- unique_domains[!is.na(unique_domains) & unique_domains != ""]
  
  set.seed(Sys.time())
  selected_ua_ids <- sample(1:100, 10, replace = FALSE)
  
  local_session_pool <- list()
  local_session_indices <- list()
  local_session_status <- list()
  
  for (domain in unique_domains) {
    local_session_pool[[domain]] <- list()
    local_session_indices[[domain]] <- 1
    
    for (i in 1:10) {
      session_name <- sprintf("session_%s_%02d", domain, i)
      session_obj <- func_03_create_session(domain, selected_ua_ids[i])
      session_obj$id <- session_name
      local_session_pool[[domain]][[session_name]] <- session_obj
      local_session_status[[session_name]] <- "active"
    }
  }
  
  assign("session_pool", local_session_pool, envir = .GlobalEnv)
  assign("session_indices", local_session_indices, envir = .GlobalEnv)
  assign("session_status", local_session_status, envir = .GlobalEnv)
  
  message(sprintf("%d sessions created for %d domains.", length(unique_domains) * 10, length(unique_domains)))
}


# 2. Function to update session headers
func_04_update_session_headers <- function(session, is_first_request = TRUE, chunk_dt = NULL, current_domain = NULL) {
  updated_headers <- session$headers
  updated_headers$connection <- ifelse(runif(1) < 0.05, "close", "keep-alive")
  
  if (is_first_request) {
    updated_headers$sec_fetch_site <- "same-site"
    updated_headers$referer        <- session$same_site_referer
    session$first_request <- FALSE
  } else {
    updated_headers$sec_fetch_site <- "same-site"
    if (runif(1) < 0.20 || is.null(chunk_dt)) {
      updated_headers$referer <- session$same_site_referer
    } else {
      same_domain_rows <- chunk_dt[domain == current_domain, ]
      if (nrow(same_domain_rows) > 0) {
        updated_headers$referer <- same_domain_rows[sample(.N, 1), url]
      } else {
        updated_headers$referer <- session$same_site_referer
      }
    }
  }
  session$headers <- updated_headers
  return(session)
}


# 3. Function to get the active session for a domain
func_04_get_active_session <- function(domain) {
  if (!exists("session_pool", envir = .GlobalEnv)) {
    stop("Session pool not initialized.")
  }
  
  pool <- get("session_pool", envir = .GlobalEnv)
  indices <- get("session_indices", envir = .GlobalEnv)
  status <- get("session_status", envir = .GlobalEnv)
  
  if (!domain %in% names(pool)) {
    stop(sprintf("Domain '%s' not found in session pool", domain))
  }
  
  current_index <- indices[[domain]]
  domain_sessions <- names(pool[[domain]])
  
  if (current_index > length(domain_sessions)) {
    return(list(success = FALSE, session = NULL, message = sprintf("All sessions exhausted for domain '%s'", domain)))
  }
  
  current_session_name <- domain_sessions[current_index]
  
  if (status[[current_session_name]] != "active") {
    return(list(success = FALSE, session = NULL, message = "Current session is not active."))
  }
  
  return(list(success = TRUE, session = pool[[domain]][[current_session_name]]))
}


# 4. Function to kill current session and move to next
func_04_kill_session <- function(domain, reason = "session_block") {
  indices <- get("session_indices", envir = .GlobalEnv)
  status <- get("session_status", envir = .GlobalEnv)
  pool <- get("session_pool", envir = .GlobalEnv)
  
  current_index <- indices[[domain]]
  domain_sessions <- names(pool[[domain]])
  
  if (current_index <= length(domain_sessions)) {
    current_session_name <- domain_sessions[current_index]
    status[[current_session_name]] <- "killed"
    message(sprintf("Session '%s' killed. Reason: %s", current_session_name, reason))
    
    indices[[domain]] <- current_index + 1
    
    assign("session_indices", indices, envir = .GlobalEnv)
    assign("session_status", status, envir = .GlobalEnv)
    
    return(TRUE)
  }
  return(FALSE)
}


# 5. Main function to prepare request package (REVISED FOR AGGRESSIVENESS)
func_04_prepare_request <- function(id, url, domain, chunk_dt = NULL, aggressiveness_level = 2) {
  
  # Step 0: Define thresholds based on the aggressiveness level
  thresholds <- switch(as.character(aggressiveness_level),
                       "1" = list(error_limit = 3,  kill_limit = 3),
                       "2" = list(error_limit = 5,  kill_limit = 5),
                       "3" = list(error_limit = 10, kill_limit = 5),
                       "4" = list(error_limit = 20, kill_limit = 5),
                       "5" = list(error_limit = 30, kill_limit = Inf), # Inf means never block
                       # Default case if an invalid level is provided
                       list(error_limit = 5, kill_limit = 5)
  )
  
  # Step 1: Query the current state of the domain
  domain_state <- func_08_get_domain_state(domain)
  if (is.null(domain_state)) {
    return(list(success = FALSE, message = sprintf("Domain '%s' not found in tracker.", domain)))
  }
  
  # Step 2: Check if the domain is permanently blocked for this chunk
  if (domain_state$session_kill_count >= thresholds$kill_limit) {
    message(sprintf("Domain '%s' is blocked for this chunk run (%d sessions killed). Skipping request.", domain, domain_state$session_kill_count))
    return(list(success = FALSE, message = "Domain blocked"))
  }
  
  # Step 3: Check if the current session needs to be killed
  session_was_rotated <- FALSE
  if (domain_state$consecutive_error_count >= thresholds$error_limit) {
    message(sprintf("Detected %d consecutive errors for domain '%s'. Killing current session.", domain_state$consecutive_error_count, domain))
    
    func_04_kill_session(domain)
    func_08_increment_kill_count(domain)
    session_was_rotated <- TRUE
  }
  
  # Step 4: Get the currently active session
  session_result <- func_04_get_active_session(domain)
  if (!session_result$success) {
    return(list(success = FALSE, message = session_result$message))
  }
  active_session <- session_result$session
  
  # Step 5: Update session state and headers
  if (is.null(active_session$request_count)) active_session$request_count <- 0L
  active_session$request_count <- active_session$request_count + 1L
  
  is_first_request_for_session <- active_session$request_count == 1L || session_was_rotated
  
  active_session <- func_04_update_session_headers(
    session = active_session,
    is_first_request = is_first_request_for_session,
    chunk_dt = chunk_dt,
    current_domain = domain
  )
  
  # Step 6: Persist the updated session object
  pool <- get("session_pool", envir = .GlobalEnv)
  pool[[domain]][[active_session$id]] <- active_session
  assign("session_pool", pool, envir = .GlobalEnv)
  
  # Step 7: Assemble and return the final request package
  request_params <- list(
    id = id,
    url = url,
    domain = domain,
    session = active_session,
    aggressiveness = aggressiveness_level, # Pass the level for logging
    timestamp = Sys.time()
  )
  
  return(list(
    success = TRUE,
    request_params = request_params,
    message = sprintf("Request prepared for %s using %s", domain, active_session$id)
  ))
}