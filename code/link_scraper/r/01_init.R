# ==============================================================================
# MODULE: INITIALIZATION & CONFIGURATION
# ==============================================================================
# 
# This module loads the Input rds. with all input links. It creates a separate input 
# to use in this project. It initializes the output data set, which will be the result
# of this project. 
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
#
# ==============================================================================

# Load required packages
library(data.table)
library(readxl)


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

paths <- get_module_paths()

# 00 Data structure check function 
test_data_structure <- function(dt_name) {
  codebook_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/code_book.xlsx"
  
  # Check if codebook exists
  if (!file.exists(codebook_path)) {
    stop(paste0("Codebook not found at: ", codebook_path))
  }
  
  # Read whole sheet with headers
  raw_codebook <- read_excel(path = codebook_path, sheet = dt_name, col_names = TRUE, trim_ws = TRUE)
  raw_codebook <- as.data.table(raw_codebook)
  
  # Transpose: 
  new_colnames <- raw_codebook[[1]]  # first column (col_name)
  mat <- as.matrix(raw_codebook[, -1, with = FALSE])
  mat_t <- t(mat)
  codebook_dt <- as.data.table(mat_t)
  setnames(codebook_dt, new_colnames)
  codebook_dt[, col_name := colnames(raw_codebook)[-1]]
  setcolorder(codebook_dt, c("col_name", setdiff(names(codebook_dt), "col_name")))
  
  # Convert columns to character and lowercase type for safe comparison
  codebook_dt[, variable_type := tolower(as.character(variable_type))]
  codebook_dt[, valid_values := as.character(valid_values)]
  
  # Extract file path from the "path" column in the "valid_values" row
  path_row <- codebook_dt[col_name == "path"]
  if (nrow(path_row) == 0) {
    stop("No 'path' column found in codebook")
  }
  
  file_path <- path_row$valid_values
  if (is.na(file_path) || file_path == "") {
    stop("File path is empty or NA in the 'path' column")
  }
  
  # Check if file exists
  if (!file.exists(file_path)) {
    stop(paste0("Data file not found at: ", file_path))
  }
  
  # Load the data file
  dt <- readRDS(file_path)
  
  if (!is.data.table(dt)) {
    stop("Loaded file is not a data.table")
  }
  
  # Columns to ignore in the check (now we don't ignore "path" since we use it)
  ignore_cols <- c("col_name")
  
  # Filter expected columns (exclude ignored and path)
  expected_cols_filtered <- codebook_dt[!col_name %in% c(ignore_cols, "path"), col_name]
  
  dt_colnames <- colnames(dt)
  
  colname_messages <- character()
  
  # Check number of columns (only counting relevant columns)
  if (length(dt_colnames) != length(expected_cols_filtered)) {
    colname_messages <- c(colname_messages,
                          paste0("Number of columns differs: expected ", length(expected_cols_filtered),
                                 ", found ", length(dt_colnames)))
  }
  
  # Check column names order and equality
  n_check <- min(length(dt_colnames), length(expected_cols_filtered))
  for (i in seq_len(n_check)) {
    if (dt_colnames[i] != expected_cols_filtered[i]) {
      colname_messages <- c(colname_messages,
                            paste0('column "', dt_colnames[i], '" (', i, '): expected "', expected_cols_filtered[i], '" but found "', dt_colnames[i], '"'))
    }
  }
  
  # Map R types to simplified type names
  map_type <- function(x) {
    if (inherits(x, "integer")) return("int")
    if (inherits(x, "numeric") | inherits(x, "double")) return("num")
    if (inherits(x, "character")) return("char")
    if (inherits(x, "logical")) return("logical")
    if (inherits(x, "factor")) return("char")
    tolower(class(x)[1])
  }
  
  vartype_messages <- character()
  for (i in seq_len(nrow(codebook_dt))) {
    col <- codebook_dt$col_name[i]
    if (!(col %in% dt_colnames)) next
    if (col %in% c(ignore_cols, "path")) next
    
    actual_type <- map_type(dt[[col]])
    expected_type <- codebook_dt$variable_type[i]
    
    if (actual_type != expected_type) {
      vartype_messages <- c(vartype_messages,
                            paste0('column "', col, '" (', i, '): expected type "', expected_type, '" but found "', actual_type, '"'))
    }
  }
  
  validvals_messages <- character()
  for (i in seq_len(nrow(codebook_dt))) {
    col <- codebook_dt$col_name[i]
    if (!(col %in% dt_colnames)) next
    if (col %in% c(ignore_cols, "path")) next
    
    valid_values_raw <- codebook_dt$valid_values[i]
    if (is.na(valid_values_raw) || valid_values_raw == "") next
    
    valid_values_list <- str_trim(unlist(strsplit(valid_values_raw, ";")))
    actual_values <- unique(as.character(dt[[col]]))
    invalid_values <- setdiff(actual_values, valid_values_list)
    
    if (length(invalid_values) > 0) {
      invalid_values_msg <- paste(invalid_values, collapse = "\n")
      validvals_messages <- c(validvals_messages,
                              paste0('column "', col, '" (', i, '): unexpected values found:\n', invalid_values_msg))
    }
  }
  
  # Print results
  cat("Testing data structure for:", dt_name, "\n")
  cat("File path:", file_path, "\n")
  cat("Data dimensions:", nrow(dt), "rows x", ncol(dt), "columns\n\n")
  
  cat("col_names:\n")
  if (length(colname_messages) == 0) {
    cat("all column names correct\n\n")
  } else {
    cat(paste0(colname_messages, collapse = "\n"), "\n\n")
  }
  
  cat("variable_type:\n")
  if (length(vartype_messages) == 0) {
    cat("all variable types correct\n\n")
  } else {
    cat(paste0(vartype_messages, collapse = "\n"), "\n\n")
  }
  
  cat("valid_values:\n")
  if (length(validvals_messages) == 0) {
    cat("all values correct\n")
  } else {
    cat(paste0(validvals_messages, collapse = "\n"), "\n")
  }
  
  # Clean up: remove the loaded data table from memory
  rm(dt)
  
  # Return invisibly (for potential further use)
  return(invisible(TRUE))
}


######



# 1. initialize input if not existing. 
init_input_dataset <- function(paths) {
  # Define path to original input dataset
  original_input_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/all_links_filtered_by_date.rds"
  # Define path to output input.rds
  output_input_path <- file.path(paths$input, "input.rds")
  
  # Check if input.rds already exists
  if (file.exists(output_input_path)) {
    message("Input dataset already exists at ", output_input_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Load the original input dataset
  input_dt <- readRDS(original_input_path)
  
  # Ensure data.table format
  setDT(input_dt)
  
  # Helper function to clean domains from URLs, remove 'www.' and TLDs (.de, .com, etc.)
  .extract_domain <- function(url) {
    # Remove protocol and www.
    domain <- sub("^https?://(?:www\\.)?", "", url)
    # Remove everything after first slash
    domain <- sub("/.*$", "", domain)
    # Remove TLD by cutting from last dot to end
    domain <- sub("\\.[^.]+$", "", domain)
    domain
  }
  
  # Create the new input dataset with cleaned domain and initialized flags
  input <- input_dt[, .(
    id = .I,                       # unique incremental identifier
    domain = .extract_domain(domain_url),  # cleaned domain
    url = result_link,
    processed = FALSE,             # initialize as FALSE
    retry = FALSE                 # initialize as FALSE
  )]
  
  # Save the new input dataset as input.rds
  saveRDS(input, output_input_path)
  
  message("Input dataset successfully created and saved to ", output_input_path)
  
  return(invisible(TRUE))
}

# Create input 
init_input_dataset(paths)

# Test input
test_data_structure("input_ds")

# Clean up environment 
rm(init_input_dataset)

# Load if wanted 
# input_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/input.rds")



##### 



# 2. Initialize final output if not existing 
init_final_output_dataset <- function(paths) {
  # Define the full output file path
  output_path <- file.path(paths$output, "final_data.rds")
  
  # Check if final_data.rds already exists to avoid overwriting
  if (file.exists(output_path)) {
    message("Output dataset already exists at ", output_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  final_data <- data.table(
    id = integer(),            # integer (to match input id)
    domain = character(),
    url = character(),        
    timestamp_scraped = as.POSIXct(character()),  # POSIXct (date-time of scraping)
    date_time = character(),
    author = character(),
    headline = character(),
    text = character(),
    paywall = logical()
  )
  
  # Save the empty dataset as RDS file to the output path
  saveRDS(final_data, output_path)
  
  message("Final output dataset successfully created and saved to ", output_path)
  
  return(invisible(TRUE))
}

# Create output 
init_final_output_dataset(paths)

# Test input
test_data_structure("final_output_ds")

# Clean up environment
rm(init_final_output_dataset)

# Load if wanted 
# final_output_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/output/final_data.rds")



###### 



# 3: Initializing the VPN log file as rds. 
init_vpn_log <- function() {
  # Define the filename for the vpn log RDS file
  vpn_log_file <- file.path(paths$logs, "vpn_log.rds")

  # Check if already existing 
  if (file.exists(vpn_log_file)) {
    message("'vpn_log.rds' already exists at ", vpn_log_file, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
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
    message("VPN log successfully created and saved to", vpn_log_file)
  # Return the vpn log data.table for further use
  return(vpn_log)
}

# Create vpn log 
init_vpn_log()

# Clean up environment
rm(init_vpn_log)

# Load if wanted 
# vpn_log <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/vpn_log.rds")



###### 



# 4: Function to create realistic user agents:
# Function to generate a single user agent string
generate_user_agent <- function(os, browser, device = NULL) {
  # Base Mozilla string
  ua_parts <- "Mozilla/5.0"
  
  # Add operating system information
  if (!is.null(device)) {
    # Mobile device user agents
    if (device %in% c("iPhone", "iPad")) {
      os_info <- sprintf("(%s; CPU %s OS 17_0 like Mac OS X)", device, device)
    } else {
      # Android devices
      os_info <- sprintf("(Linux; Android 14; %s)", device)
    }
  } else {
    # Desktop user agents
    os_info <- sprintf("(%s)", os$string)
  }
  
  ua_parts <- paste(ua_parts, os_info)
  
  # Add browser engine and compatibility
  if (browser$name == "Firefox") {
    # Firefox uses Gecko engine
    ua_parts <- paste(ua_parts, browser$engine)
    ua_parts <- paste(ua_parts, paste0(browser$name, "/", browser$version))
  } else {
    # WebKit-based browsers
    ua_parts <- paste(ua_parts, browser$engine)
    if (!is.null(browser$compatibility)) {
      ua_parts <- paste(ua_parts, sprintf("(%s)", browser$compatibility))
    }
    
    # Add browser-specific information
    if (browser$name == "Safari") {
      # Safari includes Version information
      if (!is.null(device) && device %in% c("iPhone", "iPad")) {
        ua_parts <- paste(ua_parts, "Mobile/15E148")
      }
      ua_parts <- paste(ua_parts, paste0(browser$full_version, " Safari/", browser$version))
    } else if (browser$name == "Chrome") {
      ua_parts <- paste(ua_parts, paste0("Chrome/", browser$version))
      ua_parts <- paste(ua_parts, paste0("Safari/", "537.36"))
    } else if (browser$name == "Edge") {
      ua_parts <- paste(ua_parts, paste0("Chrome/", "123.0.6312.122"))
      ua_parts <- paste(ua_parts, paste0("Safari/", "537.36"))
      ua_parts <- paste(ua_parts, paste0("Edg/", browser$version))
    }
  }
  
  return(ua_parts)
}

# Function to create the complete user agents data table
create_user_agents_table <- function(n_agents = 100) {
  # Define user agent components inside the function
  # Operating system definitions with current versions
  os_windows <- list(
    name = "Windows",
    string = "Windows NT 10.0; Win64; x64",
    compatible_browsers = c("Chrome", "Firefox", "Edge")
  )
  
  os_macos <- list(
    name = "macOS",
    string = "Macintosh; Intel Mac OS X 10_15_7",
    compatible_browsers = c("Chrome", "Safari", "Firefox")
  )
  
  os_ios <- list(
    name = "iOS",
    string = "iPhone; CPU iPhone OS 17_0 like Mac OS X",
    compatible_browsers = c("Safari")
  )
  
  os_android <- list(
    name = "Android",
    string = "Linux; Android 14",
    compatible_browsers = c("Chrome", "Firefox")
  )
  
  # Browser definitions with current versions
  browser_chrome <- list(
    name = "Chrome",
    version = "123.0.6312.122",
    engine = "AppleWebKit/537.36",
    compatibility = "KHTML, like Gecko"
  )
  
  browser_safari <- list(
    name = "Safari",
    version = "537.36",
    engine = "AppleWebKit/537.36",
    compatibility = "KHTML, like Gecko",
    full_version = "Version/17.4.1"
  )
  
  browser_firefox <- list(
    name = "Firefox",
    version = "125.0",
    engine = "Gecko/20100101",
    compatibility = NULL
  )
  
  browser_edge <- list(
    name = "Edge",
    version = "123.0.2420.97",
    engine = "AppleWebKit/537.36",
    compatibility = "KHTML, like Gecko"
  )
  
  # Mobile device definitions
  devices_mobile <- list(
    iphone = "iPhone",
    ipad = "iPad",
    pixel = "Pixel 8",
    samsung = "SM-S928B"
  )
  
  # Validate input
  if (n_agents < 20) {
    stop("Minimum number of user agents is 20 to maintain reasonable distribution")
  }
  
  # Calculate proportions for each category
  # Desktop: 75% (Windows: 50%, macOS: 25%)
  n_windows <- round(n_agents * 0.50)
  n_macos <- round(n_agents * 0.25)
  
  # Mobile: 25% (iPhone: 10%, Android: 10%, iPad: 5%)
  n_iphone <- round(n_agents * 0.10)
  n_android <- round(n_agents * 0.10)
  n_ipad <- round(n_agents * 0.05)
  
  # Adjust for rounding errors
  total_calculated <- n_windows + n_macos + n_iphone + n_android + n_ipad
  if (total_calculated < n_agents) {
    n_windows <- n_windows + (n_agents - total_calculated)
  } else if (total_calculated > n_agents) {
    n_windows <- n_windows - (total_calculated - n_agents)
  }
  
  # Initialize list to store unique user agents
  user_agents_list <- list()
  unique_strings <- character()
  id_counter <- 1
  
  # Helper function to generate unique user agent
  generate_unique_ua <- function(os, browser, device = NULL, max_attempts = 100) {
    attempts_made <- 0
    
    for (attempt in 1:max_attempts) {
      attempts_made <- attempt
      
      # Create copies to avoid modifying original objects
      os_copy <- os
      browser_copy <- browser
      device_copy <- device
      
      # For browsers with version variations, add some randomness
      if (browser_copy$name == "Chrome" || browser_copy$name == "Edge") {
        # Vary the minor version numbers
        original_version <- browser_copy$version
        version_parts <- strsplit(original_version, "\\.")[[1]]
        version_parts[3] <- as.character(sample(6300:6320, 1))
        version_parts[4] <- as.character(sample(100:130, 1))
        browser_copy$version <- paste(version_parts, collapse = ".")
      } else if (browser_copy$name == "Firefox") {
        # Vary Firefox version with more options
        firefox_major <- sample(120:127, 1)
        firefox_minor <- sample(0:3, 1)
        browser_copy$version <- sprintf("%d.%d", firefox_major, firefox_minor)
      } else if (browser_copy$name == "Safari") {
        # Vary Safari version
        safari_versions <- c("17.3", "17.3.1", "17.4", "17.4.1", "17.5")
        browser_copy$full_version <- paste0("Version/", sample(safari_versions, 1))
        # Also vary the Safari build number
        browser_copy$version <- paste0("537.", sample(35:36, 1))
      }
      
      # For macOS, vary the version
      if (!is.null(os_copy$name) && os_copy$name == "macOS" && is.null(device_copy)) {
        # More macOS versions for variety
        macos_versions <- c("10_15_5", "10_15_6", "10_15_7", "11_0", "11_1", "12_0", "13_0", "14_0")
        os_copy$string <- sprintf("Macintosh; Intel Mac OS X %s", sample(macos_versions, 1))
      }
      
      # For Windows, add variation in Windows version
      if (!is.null(os_copy$name) && os_copy$name == "Windows") {
        # Vary between Windows 10 and 11
        windows_version <- sample(c("10.0", "11.0"), 1)
        os_copy$string <- sprintf("Windows NT %s; Win64; x64", windows_version)
      }
      
      # For Android devices, vary the Android version and device model
      if (!is.null(device_copy) && device_copy %in% c("Pixel 8", "SM-S928B")) {
        android_version <- sample(12:14, 1)
        if (device_copy == "Pixel 8") {
          device_copy <- paste0("Pixel ", sample(6:8, 1))
        } else {
          device_copy <- paste0("SM-S", sample(900:930, 1), sample(c("B", "U", "N"), 1))
        }
        os_copy$string <- sprintf("Linux; Android %d", android_version)
      }
      
      # For iOS devices, vary the iOS version
      if (!is.null(os_copy$name) && os_copy$name == "iOS") {
        ios_version <- sample(c("16_0", "17_0", "17_1", "17_2"), 1)
        if (!is.null(device_copy)) {
          os_copy$string <- sprintf("%s; CPU %s OS %s like Mac OS X", 
                                    device_copy, device_copy, ios_version)
        }
      }
      
      ua_string <- generate_user_agent(os_copy, browser_copy, device_copy)
      
      # Check if this user agent is unique
      if (!ua_string %in% unique_strings) {
        return(ua_string)
      }
    }
    
    # Debug output when failing
    stop("Could not generate unique user agent after maximum attempts")
  }
  
  # Generate Windows desktop user agents
  for (i in 1:n_windows) {
    # Distribute browsers: Chrome (60%), Firefox (25%), Edge (15%)
    browser_choice <- sample(1:100, 1)
    if (browser_choice <= 60) {
      ua_string <- generate_unique_ua(os_windows, browser_chrome)
    } else if (browser_choice <= 85) {
      ua_string <- generate_unique_ua(os_windows, browser_firefox)
    } else {
      ua_string <- generate_unique_ua(os_windows, browser_edge)
    }
    
    unique_strings <- c(unique_strings, ua_string)
    user_agents_list[[id_counter]] <- list(
      user_agent_id = id_counter,
      user_agent_string = ua_string
    )
    id_counter <- id_counter + 1
  }
  
  # Generate macOS desktop user agents
  for (i in 1:n_macos) {
    # Distribute browsers: Safari (40%), Chrome (40%), Firefox (20%)
    browser_choice <- sample(1:100, 1)
    if (browser_choice <= 40) {
      ua_string <- generate_unique_ua(os_macos, browser_safari)
    } else if (browser_choice <= 80) {
      ua_string <- generate_unique_ua(os_macos, browser_chrome)
    } else {
      ua_string <- generate_unique_ua(os_macos, browser_firefox)
    }
    
    unique_strings <- c(unique_strings, ua_string)
    user_agents_list[[id_counter]] <- list(
      user_agent_id = id_counter,
      user_agent_string = ua_string
    )
    id_counter <- id_counter + 1
  }
  
  # Generate iPhone user agents
  for (i in 1:n_iphone) {
    ua_string <- generate_unique_ua(os_ios, browser_safari, devices_mobile$iphone)
    
    unique_strings <- c(unique_strings, ua_string)
    user_agents_list[[id_counter]] <- list(
      user_agent_id = id_counter,
      user_agent_string = ua_string
    )
    id_counter <- id_counter + 1
  }
  
  # Generate Android device user agents
  for (i in 1:n_android) {
    # Alternate between Pixel and Samsung devices
    device <- if (sample(c(TRUE, FALSE), 1)) devices_mobile$pixel else devices_mobile$samsung
    
    # Mostly Chrome (80%), some Firefox (20%)
    browser_choice <- sample(1:100, 1)
    if (browser_choice <= 80) {
      ua_string <- generate_unique_ua(os_android, browser_chrome, device)
    } else {
      ua_string <- generate_unique_ua(os_android, browser_firefox, device)
    }
    
    unique_strings <- c(unique_strings, ua_string)
    user_agents_list[[id_counter]] <- list(
      user_agent_id = id_counter,
      user_agent_string = ua_string
    )
    id_counter <- id_counter + 1
  }
  
  # Generate iPad user agents
  for (i in 1:n_ipad) {
    ua_string <- generate_unique_ua(os_ios, browser_safari, devices_mobile$ipad)
    
    unique_strings <- c(unique_strings, ua_string)
    user_agents_list[[id_counter]] <- list(
      user_agent_id = id_counter,
      user_agent_string = ua_string
    )
    id_counter <- id_counter + 1
  }
  
  # Convert list to data.table
  user_agents_dt <- rbindlist(user_agents_list)
  
  # Final verification that all user agents are unique
  if (length(unique(user_agents_dt$user_agent_string)) != nrow(user_agents_dt)) {
    stop("Duplicate user agents detected in final table")
  }
  
  return(user_agents_dt)
}

# Function to save user agents to RDS file
save_user_agents <- function(n_agents = 100) {
  # Get paths configuration
  paths <- get_module_paths()
  
  # Define output file path
  output_file <- file.path(paths$input, "user_agents.rds")
  
  # Check if file already exists
  if (file.exists(output_file)) {
    message(sprintf("'user_agents.rds' already exists at %s. Skipping creation.", output_file))
  } else {
    # Generate user agents table with specified number
    user_agents_dt <- create_user_agents_table(n_agents)
    
    # Save to RDS file
    saveRDS(user_agents_dt, output_file)
    
    # Print confirmation message
    message(sprintf("User agents list successfully created and saved to %s", output_file))
  }
}

# Create list 
save_user_agents(100)

test_data_structure("user_agents")

# Load if wanted 
# user_agents <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/user_agents.rds")

# Test code to check for duplicates in the generated user agents
test_user_agents <- function() {
  # Load the generated user agents file
  test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/user_agents.rds")
  
  # Basic information about the data
  message("\n=== USER AGENT TEST RESULTS ===")
  message(sprintf("Total user agents loaded: %d", nrow(test)))
  message(sprintf("Unique user agents: %d", length(unique(test$user_agent_string))))
  
  # Check for duplicates
  duplicates <- test[duplicated(user_agent_string) | duplicated(user_agent_string, fromLast = TRUE)]
  
  if (nrow(duplicates) > 0) {
    message(sprintf("\nWARNING: Found %d duplicate entries!", nrow(duplicates)))
    message("\nDuplicate user agents:")
    setorder(duplicates, user_agent_string)
    print(duplicates)
  } else {
    message("\nSUCCESS: No duplicates found! All user agents are unique.")
  }
  
  # Distribution analysis
  message("\n=== DISTRIBUTION ANALYSIS ===")
  
  # Count different types
  windows_count <- sum(grepl("Windows NT", test$user_agent_string))
  macos_count <- sum(grepl("Macintosh", test$user_agent_string))
  iphone_count <- sum(grepl("iPhone", test$user_agent_string))
  android_count <- sum(grepl("Android", test$user_agent_string))
  ipad_count <- sum(grepl("iPad", test$user_agent_string))
  
  # Browser counts
  chrome_count <- sum(grepl("Chrome/", test$user_agent_string) & !grepl("Edg/", test$user_agent_string))
  safari_count <- sum(grepl("Safari/", test$user_agent_string) & grepl("Version/", test$user_agent_string))
  firefox_count <- sum(grepl("Firefox/", test$user_agent_string))
  edge_count <- sum(grepl("Edg/", test$user_agent_string))
  
  # Display distribution
  message("\nOperating System Distribution:")
  message(sprintf("  Windows: %d (%.1f%%)", windows_count, windows_count/nrow(test)*100))
  message(sprintf("  macOS: %d (%.1f%%)", macos_count, macos_count/nrow(test)*100))
  message(sprintf("  iPhone: %d (%.1f%%)", iphone_count, iphone_count/nrow(test)*100))
  message(sprintf("  Android: %d (%.1f%%)", android_count, android_count/nrow(test)*100))
  message(sprintf("  iPad: %d (%.1f%%)", ipad_count, ipad_count/nrow(test)*100))
  
  message("\nBrowser Distribution:")
  message(sprintf("  Chrome: %d (%.1f%%)", chrome_count, chrome_count/nrow(test)*100))
  message(sprintf("  Safari: %d (%.1f%%)", safari_count, safari_count/nrow(test)*100))
  message(sprintf("  Firefox: %d (%.1f%%)", firefox_count, firefox_count/nrow(test)*100))
  message(sprintf("  Edge: %d (%.1f%%)", edge_count, edge_count/nrow(test)*100))
  
  # Show sample user agents
  message("\n=== SAMPLE USER AGENTS ===")
  message("First 5 user agents:")
  for (i in 1:min(5, nrow(test))) {
    message(sprintf("  %d: %s", i, test$user_agent_string[i]))
  }
  
  # Return the test data invisibly
  invisible(test)
}

# Run the test
test_user_agents()

# Clean up environment - remove all created variables and functions
rm(list = c("get_module_paths", "generate_user_agent", "create_user_agents_table",
            "save_user_agents", "test_user_agents"))


