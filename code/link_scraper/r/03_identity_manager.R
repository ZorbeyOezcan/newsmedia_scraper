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

# 1: Initializing the VPN log file as rds. 
create_vpn_log <- function() {
  # Retrieve paths configuration
  paths <- get_module_paths()
  
  # Define the filename for the vpn log RDS file
  vpn_log_file <- file.path(paths$logs, "03_vpn_log.rds")
  
  # Check if vpn log file already exists to load it
  if (file.exists(vpn_log_file)) {
    # Load existing vpn log data table
    vpn_log <- readRDS(vpn_log_file)
  } else {
    # Create new empty vpn log data.table with defined columns
    vpn_log <- data.table(
      id = integer(),               # unique identifier for VPN entry
      ip_address = character(),     # VPN IP address string
      type = character(),           # type/category of VPN (personal / vpn)
      first_used = as.POSIXct(character()), # timestamp of first log
      last_used = as.POSIXct(character()),  # timestamp of last update
      total_requests = integer(),   # counter of total requests made
      blocked_by_domain = list()    # list of domains that blocked this vpn. 
    )
    
    # Save the new empty vpn log data.table as RDS file
    saveRDS(vpn_log, vpn_log_file)
  }
  
  # Return the vpn log data.table for further use
  return(vpn_log)
}

vpn_log_dt <- create_vpn_log()

######

# 2: Function to add or update personal IP address entry in the vpn log data.table
## Execute without active VPN
update_personal_ip_in_vpn_log <- function(vpn_log_dt) {
  
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

vpn_log_dt <- update_personal_ip_in_vpn_log(vpn_log_dt)

######

# 3: Function to add or update VPN IP address entry in the vpn log data.table
## Execute with active VPN
update_vpn_ip_in_vpn_log <- function(vpn_log_dt) {
  
  # Get current system time for timestamps
  current_time <- Sys.time()
  
  # Try to get VPN IP address via https://ifconfig.me/ip
  # Use httr::GET to avoid issues and trim whitespace
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
      blocked_by_domain = list(character(0)) # empty list column
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
      # Basic info message
      message(sprintf('Address "%s" Known. This address has been used for %d total requests.', vpn_ip_address, total_requests))
      
      # If blocked domains not empty, print extra warning
      if (length(blocked_domains) > 0) {
        warning(sprintf("VPN has been blocked by %d domains.", length(blocked_domains)))
      }
    }
    
    # Update last_used only
    vpn_log_dt[existing_row_idx, last_used := current_time]
  }
  
  paths <- get_module_paths()
  vpn_log_file <- file.path(paths$logs, "03_vpn_log.rds")
  saveRDS(vpn_log_dt, vpn_log_file)
  
  return(vpn_log_dt)
}

vpn_log_dt <- update_vpn_ip_in_vpn_log(vpn_log_dt)

###### 











