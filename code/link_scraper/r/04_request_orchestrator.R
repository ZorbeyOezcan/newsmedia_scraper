# ==============================================================================
# MODULE: REQUEST ORCHESTRATION & WORKER MANAGEMENT
# ==============================================================================
# 
# This module serves as the central control system for all scraping requests.
# It decides which links should be processed by which workers, determines the
# optimal request parameters (timing, aggressiveness level) based on domain
# history, manages the request queue and worker pool, and dynamically adjusts
# scraping aggressiveness based on server responses. The orchestrator makes
# real-time decisions before each request, balancing speed with safety to
# maximize successful scrapes while avoiding detection.
#
# RECEIVES FROM:
# - 02_chunk_manager: Chunk data with links to process
# - 03_identity_manager: Session creation function
# - 07_response_analyzer: Signals for session management
# 
# OUTPUTS TO:
# - 05_request_executor: URL, session object, and aggressiveness parameters
# - 08_domain_state_manager: Performance metrics and optimization data
#
# ==============================================================================

# Load required packages
library(data.table)

# Source Functions 
source("03_identity_manager.R")

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



# Helper function to clean domain names
.extract_clean_domain <- function(domain) {
  # Remove protocol and www.
  domain <- sub("^https?://(?:www\\.)?", "", domain)
  # Remove everything after first slash
  domain <- sub("/.*$", "", domain)
  # Remove TLD by cutting from last dot to end
  domain <- sub("\\.[^.]+$", "", domain)
  domain
}

# 1. Function to initialize session pool for all domains
func_04_initialize_session_pool <- function() {
  # Get paths configuration
  paths <- get_module_paths()
  
  # Load input data to get unique domains
  input_path <- file.path(paths$input, "input.rds")
  if (!file.exists(input_path)) {
    stop("Input file not found at: ", input_path)
  }
  
  # Read input data
  input_dt <- readRDS(input_path)
  
  # Get unique domains from the domain column
  unique_domains <- unique(input_dt$domain)
  unique_domains <- unique_domains[!is.na(unique_domains) & unique_domains != ""]
  
  # Select 10 random user agent IDs (between 1 and 100)
  set.seed(Sys.time()) # Use current time as seed for randomness
  selected_ua_ids <- sample(1:100, 10, replace = FALSE)
  
  # Sort the UA IDs for display
  selected_ua_ids_sorted <- sort(selected_ua_ids)
  
  # Initialize local variables that will be assigned globally later
  local_session_pool <- list()
  local_session_indices <- list()
  local_session_status <- list()
  
  # Initialize counters
  total_sessions_created <- 0
  domains_processed <- 0
  
  # Create sessions for each domain
  for (domain in unique_domains) {
    # Clean domain name for session naming
    clean_domain <- .extract_clean_domain(domain)
    
    # Initialize domain entry in session pool
    local_session_pool[[domain]] <- list()
    
    # Initialize session index for this domain (starts at 1)
    local_session_indices[[domain]] <- 1
    
    # Create 10 sessions for this domain using the selected user agents
    for (i in 1:10) {
      # Generate session name
      session_name <- sprintf("session_%s_%02d", clean_domain, i)
      
      # Create session using the function from module 03
      session_obj <- func_03_create_session(domain, selected_ua_ids[i])
      
      # Update session ID to match our naming convention
      session_obj$id <- session_name
      
      # Store session in the pool
      local_session_pool[[domain]][[session_name]] <- session_obj
      
      # Initialize session status as active
      local_session_status[[session_name]] <- "active"
      
      total_sessions_created <- total_sessions_created + 1
    }
    
    domains_processed <- domains_processed + 1
  }
  
  # Assign all variables to global environment at once
  assign("session_pool", local_session_pool, envir = .GlobalEnv)
  assign("session_indices", local_session_indices, envir = .GlobalEnv)
  assign("session_status", local_session_status, envir = .GlobalEnv)
  
  # Print summary
  message(sprintf("%d sessions created for %d domains.", 
                  total_sessions_created, 
                  domains_processed))
  message(sprintf("User Agent IDs: %s", 
                  paste(selected_ua_ids_sorted, collapse = ", ")))
  
  # Return summary information invisibly
  invisible(list(
    total_sessions = total_sessions_created,
    total_domains = domains_processed,
    user_agent_ids = selected_ua_ids_sorted
  ))
}

# Call function (for run script)
# func_04_initialize_session_pool()



######



# 2. Function to update session headers based on request context
func_04_update_session_headers <- function(session, is_first_request = TRUE, chunk_dt = NULL, current_domain = NULL) {
  # Validate inputs
  if (!is.list(session) || !"headers" %in% names(session)) {
    stop("Invalid session object: missing headers")
  }
  
  # Create a copy of headers to modify
  updated_headers <- session$headers
  
  # Update Connection header (5% chance of "close", 95% "keep-alive")
  connection_random <- runif(1)
  updated_headers$connection <- ifelse(connection_random < 0.05, "close", "keep-alive")
  
  # Handle first request of a session
  #if (is_first_request) {
    # First request uses cross-site referer (already set in session creation)
    # Ensure Sec-Fetch-Site is set to cross-site
    #updated_headers$sec_fetch_site <- "cross-site"
    
    # Mark session as no longer on first request
    #session$first_request <- FALSE
    
  # test 2 - same site referrer 
  if (is_first_request) {
    updated_headers$sec_fetch_site <- "same-site"
    updated_headers$referer        <- session$same_site_referer
    session$first_request <- FALSE
    
  } else {
    # Subsequent requests - always use same-site
    updated_headers$sec_fetch_site <- "same-site"
    
    # Update referer based on probability
    referer_random <- runif(1)
    
    if (referer_random < 0.20) {
      # 20% chance: use same-site referer from session
      updated_headers$referer <- session$same_site_referer
      
    } else {
      # 80% chance: use a random URL from the same domain
      if (!is.null(chunk_dt) && !is.null(current_domain)) {
        # Filter chunk for same domain URLs
        same_domain_rows <- chunk_dt[chunk_dt$domain == current_domain, ]
        
        if (nrow(same_domain_rows) > 0) {
          # Select random URL from same domain
          random_row <- same_domain_rows[sample(nrow(same_domain_rows), 1), ]
          updated_headers$referer <- random_row$url
        } else {
          # Fallback to same-site referer if no other URLs found
          updated_headers$referer <- session$same_site_referer
        }
      } else {
        # Fallback if chunk or domain not provided
        updated_headers$referer <- session$same_site_referer
      }
    }
  }
  
  # Update the session headers
  session$headers <- updated_headers
  
  # Log header update for debugging
  if (is_first_request) {
    message(sprintf("Headers updated for first request of %s", session$id))
  } else {
    message(sprintf("Headers updated for subsequent request of %s (Referer: %s)", 
                    session$id, 
                    substr(updated_headers$referer, 1, 50)))
  }
  
  # Return the updated session
  return(session)
}



#####



# 3. Function to get the active session for a domain
func_04_get_active_session <- function(domain) {
  # Check if session pool exists
  if (!exists("session_pool", envir = .GlobalEnv)) {
    stop("Session pool not initialized. Run func_04_initialize_session_pool() first.")
  }
  
  # Get session pool from global environment
  pool <- get("session_pool", envir = .GlobalEnv)
  indices <- get("session_indices", envir = .GlobalEnv)
  status <- get("session_status", envir = .GlobalEnv)
  
  # Check if domain exists in pool
  if (!domain %in% names(pool)) {
    stop(sprintf("Domain '%s' not found in session pool", domain))
  }
  
  # Get current session index for domain
  current_index <- indices[[domain]]
  
  # Get all session names for this domain
  domain_sessions <- names(pool[[domain]])
  
  # Check if index is within bounds
  if (current_index > length(domain_sessions)) {
    return(list(
      success = FALSE,
      session = NULL,
      message = sprintf("All sessions exhausted for domain '%s'", domain)
    ))
  }
  
  # Get current session name
  current_session_name <- domain_sessions[current_index]
  
  # Check if session is active
  if (status[[current_session_name]] != "active") {
    # Try to find next active session
    for (i in (current_index + 1):length(domain_sessions)) {
      if (i > length(domain_sessions)) break
      
      next_session_name <- domain_sessions[i]
      if (status[[next_session_name]] == "active") {
        # Update index and return this session
        indices[[domain]] <- i
        assign("session_indices", indices, envir = .GlobalEnv)
        
        return(list(
          success = TRUE,
          session = pool[[domain]][[next_session_name]],
          message = sprintf("Switched to session %d for domain '%s'", i, domain)
        ))
      }
    }
    
    # No active sessions found
    return(list(
      success = FALSE,
      session = NULL,
      message = sprintf("No active sessions remaining for domain '%s'", domain)
    ))
  }
  
  # Return the active session
  return(list(
    success = TRUE,
    session = pool[[domain]][[current_session_name]],
    message = sprintf("Using session %d for domain '%s'", current_index, domain)
  ))
}



#####



# 4. Function to kill current session and move to next
func_04_kill_session <- function(domain, reason = "session_block") {
  # Get global variables
  indices <- get("session_indices", envir = .GlobalEnv)
  status <- get("session_status", envir = .GlobalEnv)
  pool <- get("session_pool", envir = .GlobalEnv)
  
  # Get current session index
  current_index <- indices[[domain]]
  domain_sessions <- names(pool[[domain]])
  
  if (current_index <= length(domain_sessions)) {
    # Mark current session as killed
    current_session_name <- domain_sessions[current_index]
    status[[current_session_name]] <- "killed"
    
    # Log the kill
    message(sprintf("Session '%s' killed. Reason: %s", current_session_name, reason))
    
    # Move to next session
    indices[[domain]] <- current_index + 1
    
    # Update global variables
    assign("session_indices", indices, envir = .GlobalEnv)
    assign("session_status", status, envir = .GlobalEnv)
    
    # Check if there are remaining sessions
    if (indices[[domain]] <= length(domain_sessions)) {
      return(list(
        success = TRUE,
        sessions_remaining = length(domain_sessions) - current_index,
        message = sprintf("Moved to session %d for domain '%s'", indices[[domain]], domain)
      ))
    } else {
      return(list(
        success = FALSE,
        sessions_remaining = 0,
        message = sprintf("All sessions exhausted for domain '%s'", domain)
      ))
    }
  } else {
    return(list(
      success = FALSE,
      sessions_remaining = 0,
      message = sprintf("No active session to kill for domain '%s'", domain)
    ))
  }
}



#####



# 5. Function to check if domain has exhausted all sessions
func_04_check_domain_exhausted <- function(domain) {
  # Get global variables
  indices <- get("session_indices", envir = .GlobalEnv)
  pool <- get("session_pool", envir = .GlobalEnv)
  
  # Check if domain exists
  if (!domain %in% names(pool)) {
    return(TRUE)  # Domain doesn't exist, consider it exhausted
  }
  
  # Check if current index exceeds available sessions
  current_index <- indices[[domain]]
  total_sessions <- length(pool[[domain]])
  
  return(current_index > total_sessions)
}



#####



# 6. Function to prepare request package for module 05 - main function 
func_04_prepare_request <- function(url, domain, chunk_dt = NULL, aggressiveness_level = 1) {
  # Get active session for domain
  session_result <- func_04_get_active_session(domain)
  
  if (!session_result$success) {
    return(list(
      success = FALSE,
      request_params = NULL,
      message = session_result$message
    ))
  }
  
  # Get the active session
  active_session <- session_result$session
  
  # Determine if this is the first request for this session
  is_first_request <- ifelse(is.null(active_session$first_request), TRUE, active_session$first_request)
  
  
  # Update session headers based on context
  active_session <- func_04_update_session_headers(
    session = active_session,
    is_first_request = is_first_request,
    chunk_dt = chunk_dt,
    current_domain = domain
  )
  
  # Update the session in the global pool with the modified headers
  pool <- get("session_pool", envir = .GlobalEnv)
  session_name <- active_session$id
  
  # Find which session this is for the domain
  domain_sessions <- names(pool[[domain]])
  for (s_name in domain_sessions) {
    if (pool[[domain]][[s_name]]$id == session_name) {
      pool[[domain]][[s_name]] <- active_session
      break
    }
  }
  assign("session_pool", pool, envir = .GlobalEnv)
  
  # Create request parameters package
  request_params <- list(
    url = url,
    domain = domain,
    session = active_session,
    aggressiveness = aggressiveness_level,
    timestamp = Sys.time()
  )
  
  # Return the request package
  return(list(
    success = TRUE,
    request_params = request_params,
    message = sprintf("Request prepared for %s using %s", domain, active_session$id)
  ))
}



