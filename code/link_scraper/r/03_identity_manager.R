# ==============================================================================
# MODULE: IDENTITY MANAGEMENT (VPN, USER-AGENTS, HEADERS)
# ==============================================================================
# 
# This module manages all aspects of request identity to maintain realistic
# browsing behavior. It first initializes a VPN log log file for security. 
# Than functions for updating the logs are defined. 
# It handles the rotation of user agents and headers while
# maintaining session consistency, ensures that identity combinations remain
# realistic for the current VPN, manages cookie jars per domain, and tracks
# which identity combinations have been used. The module prevents suspicious
# patterns like using 50 different user agents from the same IP address and
# ensures that identity changes follow realistic browsing patterns.
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
# 
# ==============================================================================

# Load required packages
library(data.table)
library(httr)

# Path Configuration Function
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




# 1: Function to add or update personal IP address entry in the vpn log data.table
## Execute without active VPN
func_03_update_personal_ip_in_vpn_log <- function(vpn_log_dt) {
  
  # Get current system time for timestamps
  current_time <- Sys.time()
  
  # Try to get personal IP address via https://ifconfig.me/ip
  # Use httr::GET to avoid issues and trim whitespace
  res <- httr::GET("https://ifconfig.me/ip")
  if (res$status_code == 200) {
    personal_ip_address <- trimws(httr::content(res, "text", encoding = "UTF-8"))
  } else {
    stop("Failed to retrieve personal IP address")
  }
  
  # Check if personal IP is already in vpn_log_dt
  existing_row_idx <- vpn_log_dt[ip_address == personal_ip_address, which = TRUE]
  
  if (length(existing_row_idx) == 0) {
    # IP not found - create new entry
    
    # Calculate new id: max existing id + 1, or 1 if empty
    new_id <- ifelse(nrow(vpn_log_dt) > 0, max(vpn_log_dt$id, na.rm = TRUE) + 1, 1)
    
    # Create new row as data.table
    new_row <- data.table(
      id = new_id,
      ip_address = personal_ip_address,
      type = "personal",
      first_used = current_time,
      last_used = current_time,
      total_requests = 0L,
      blocked_by_domain = list(character(0)) # empty list column
    )
    
    # Append new row to vpn_log_dt
    vpn_log_dt <- rbind(vpn_log_dt, new_row)
    
  } else {
    # IP exists - update last_used only
    vpn_log_dt[existing_row_idx, last_used := current_time]
  }
  
  paths <- get_module_paths()
  vpn_log_file <- file.path(paths$logs, "03_vpn_log.rds")
  saveRDS(vpn_log_dt, vpn_log_file)
  
  return(vpn_log_dt)
}

# vpn_log_dt <- func_03_update_personal_ip_in_vpn_log(vpn_log_dt)



######



# 2: Function to add or update VPN IP address entry in the vpn log data.table
## Execute with active VPN
func_03_initialzie_vpn_connnection <- function() {
  
  # Load required packages
  library(data.table)
  library(httr)
  
  # Retrieve paths configuration
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
  
  paths <- get_module_paths()
  vpn_log_file <- file.path(paths$logs, "03_vpn_log.rds")
  
  # Load existing vpn_log_dt or create empty if not exists
  if (file.exists(vpn_log_file)) {
    vpn_log_dt <- readRDS(vpn_log_file)
  } else {
    vpn_log_dt <- data.table(
      id = integer(),
      ip_address = character(),
      type = character(),
      first_used = as.POSIXct(character()),
      last_used = as.POSIXct(character()),
      total_requests = integer(),
      blocked_by_domain = list()
    )
  }
  
  # Get current system time for timestamps
  current_time <- Sys.time()
  
  # Retrieve VPN IP address via https://ifconfig.me/ip
  res <- httr::GET("https://ifconfig.me/ip")
  if (res$status_code == 200) {
    vpn_ip_address <- trimws(httr::content(res, "text", encoding = "UTF-8"))
  } else {
    stop("Failed to retrieve VPN IP address")
  }
  
  # Check if VPN IP is already in vpn_log_dt
  existing_row_idx <- vpn_log_dt[ip_address == vpn_ip_address, which = TRUE]
  
  if (length(existing_row_idx) == 0) {
    # IP not found - create new entry
    
    # Output new VPN message
    message(sprintf('New VPN "%s"', vpn_ip_address))
    
    # Calculate new id: max existing id + 1, or 1 if empty
    new_id <- ifelse(nrow(vpn_log_dt) > 0, max(vpn_log_dt$id, na.rm = TRUE) + 1, 1)
    
    # Create new row as data.table
    new_row <- data.table(
      id = new_id,
      ip_address = vpn_ip_address,
      type = "vpn",
      first_used = current_time,
      last_used = current_time,
      total_requests = 0L,
      blocked_by_domain = list(character(0))
    )
    
    # Append new row to vpn_log_dt
    vpn_log_dt <- rbind(vpn_log_dt, new_row)
    
  } else {
    # IP exists - check type and blocked status
    
    ip_type <- vpn_log_dt[existing_row_idx, type]
    total_requests <- vpn_log_dt[existing_row_idx, total_requests]
    blocked_domains <- vpn_log_dt[existing_row_idx, blocked_by_domain][[1]]
    
    if (ip_type == "personal") {
      warning("Warning: Personal VPN. Do not use this address.")
    } else if (ip_type == "vpn") {
      message(sprintf('Address "%s" Known. This address has been used for %d total requests.', vpn_ip_address, total_requests))
      
      if (length(blocked_domains) > 0) {
        warning(sprintf("VPN has been blocked by %d domains.", length(blocked_domains)))
      }
    }
    
    # Update last_used only
    vpn_log_dt[existing_row_idx, last_used := current_time]
  }
  
  # Save the updated vpn_log_dt back to the RDS file
  saveRDS(vpn_log_dt, vpn_log_file)
  
  # Return invisibly to avoid printing large tables
  invisible(vpn_log_dt)
}


# calling the function: 
# func_03_initialzie_vpn_connnection()



###### 



# to do: update vpn function, that takes total requests and blocked domains
# 3: update vpn function 



######



# 4: Function to create a new session for a domain
# 4: Function to create a new session for a domain
func_03_create_session <- function(domain, user_agent_id) {
  
  # Load required data
  paths <- get_module_paths()
  
  # Load user agents
  user_agents_path <- file.path(paths$input, "user_agents.rds")
  if (!file.exists(user_agents_path)) {
    stop("User agents file not found at: ", user_agents_path)
  }
  user_agents <- readRDS(user_agents_path)
  
  # Load header params
  header_params_path <- file.path(paths$input, "header_params.rds")
  if (!file.exists(header_params_path)) {
    stop("Header params file not found at: ", header_params_path)
  }
  header_params <- readRDS(header_params_path)
  
  # Validate user_agent_id
  if (!user_agent_id %in% user_agents$user_agent_id) {
    stop("Invalid user_agent_id: ", user_agent_id)
  }
  
  # Get user agent string
  ua_id <- user_agent_id  # Store in local variable to avoid scoping issues
  user_agent_row <- user_agents[user_agent_id == ua_id]
  if (nrow(user_agent_row) != 1) {
    stop("User agent ID not found or ambiguous: ", ua_id)
  }
  user_agent_string <- user_agent_row$user_agent_string
  
  # Detect browser type from user agent string
  browser_type <- if (grepl("Chrome", user_agent_string) && !grepl("Edg/", user_agent_string)) {
    "chrome"
  } else if (grepl("Firefox", user_agent_string)) {
    "firefox"
  } else if (grepl("Safari", user_agent_string) && !grepl("Chrome", user_agent_string)) {
    "safari"
  } else if (grepl("Edg/", user_agent_string)) {
    "edge"
  } else {
    "chrome"  # Default fallback
  }
  
  # Detect if mobile device
  is_mobile <- grepl("Mobile|Android|iPhone|iPad", user_agent_string)
  
  # Select appropriate Accept header based on browser type
  accept_options <- names(header_params$accept)
  browser_accept_options <- accept_options[grepl(browser_type, accept_options)]
  
  if (length(browser_accept_options) > 0) {
    accept_header <- header_params$accept[[sample(browser_accept_options, 1)]]
  } else {
    # Fallback to generic option
    accept_header <- header_params$accept[[sample(accept_options, 1)]]
  }
  
  # Select Accept-Language (prefer browser-specific if available)
  lang_options <- names(header_params$accept_language)
  browser_lang_options <- lang_options[grepl(browser_type, lang_options)]
  
  if (length(browser_lang_options) > 0) {
    accept_language <- header_params$accept_language[[sample(browser_lang_options, 1)]]
  } else {
    # Fallback to general German options
    accept_language <- header_params$accept_language[[sample(lang_options, 1)]]
  }
  
  # Select Accept-Encoding based on browser
  if (browser_type == "chrome" || browser_type == "edge") {
    # Chrome/Edge should always include Brotli
    encoding_options <- names(header_params$accept_encoding)
    br_options <- encoding_options[grepl("br", header_params$accept_encoding)]
    accept_encoding <- header_params$accept_encoding[[sample(br_options, 1)]]
  } else {
    # Other browsers - any encoding option
    accept_encoding <- header_params$accept_encoding[[sample(names(header_params$accept_encoding), 1)]]
  }
  
  # Select cross-site referer randomly
  cross_site_options <- names(header_params$cross_site_referer)
  selected_referer <- header_params$cross_site_referer[[sample(cross_site_options, 1)]]
  
  # Get host header for domain
  if (!domain %in% names(header_params$host)) {
    stop("Domain not found in header params: ", domain)
  }
  host_header <- header_params$host[[domain]]
  
  # Get same-site referer for later use
  if (!domain %in% names(header_params$same_site_referer)) {
    # Fallback construction if not found
    same_site_referer <- paste0("https://", domain, "/")
  } else {
    same_site_referer <- header_params$same_site_referer[[domain]]
  }
  
  # Create initial headers list
  headers <- list(
    user_agent = user_agent_string,
    accept = accept_header,
    accept_language = accept_language,
    accept_encoding = accept_encoding,
    connection = "keep-alive",
    referer = selected_referer,
    host = host_header
  )
  
  # Add Upgrade-Insecure-Requests conditionally
  # Chrome, Safari, Edge get it; Firefox and Mobile don't
  if ((browser_type %in% c("chrome", "safari", "edge")) && !is_mobile) {
    headers$upgrade_insecure_requests <- header_params$upgrade_insecure_requests$default
  }
  
  # Add Sec-Fetch headers
  headers$sec_fetch_dest <- header_params$sec_fetch_dest$default
  headers$sec_fetch_mode <- header_params$sec_fetch_mode$default
  headers$sec_fetch_site <- "cross-site"  # Initial request is cross-site
  
  # Create session ID
  session_id <- paste0("session_", domain, "_", user_agent_id, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  
  # Create httr2 cookie jar
  cookie_jar <- tempfile(pattern = "cookies_", fileext = ".txt")
  
  # Create session object
  session <- list(
    id = session_id,
    domain = domain,
    user_agent_id = user_agent_id,
    user_agent_string = user_agent_string,
    browser_type = browser_type,
    is_mobile = is_mobile,
    headers = headers,
    same_site_referer = same_site_referer,
    cookie_jar = cookie_jar,
    first_request = TRUE,
    created_at = Sys.time(),
    last_used = Sys.time(),
    request_count = 0L,
    status = "active"
  )
  
  # Log session creation
  message(sprintf("Session created: %s for domain %s with %s", 
                  session_id, domain, browser_type))
  
  return(session)
}

# Test call
# session <- func_03_create_session("spiegel.de" , 1 )


