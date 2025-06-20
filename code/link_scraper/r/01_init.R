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
library(httr)
library(stringr)


# Path Configuration Function
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
  loaded_data <- readRDS(file_path)
  
  # Check if it's a list (like header_params) or data.table
  is_list_structure <- is.list(loaded_data) && !is.data.table(loaded_data)
  
  if (!is.data.table(loaded_data) && !is_list_structure) {
    stop("Loaded file is neither a data.table nor a list")
  }
  
  # Get column/element names based on structure type
  if (is_list_structure) {
    dt_colnames <- names(loaded_data)
  } else {
    dt_colnames <- colnames(loaded_data)
  }
  
  # Columns to ignore in the check
  ignore_cols <- c("col_name")
  
  # Filter expected columns (exclude ignored and path)
  expected_cols_filtered <- codebook_dt[!col_name %in% c(ignore_cols, "path"), col_name]
  
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
    if (is.list(x) && !is.data.frame(x)) return("list")
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
    
    if (is_list_structure) {
      actual_type <- map_type(loaded_data[[col]])
    } else {
      actual_type <- map_type(loaded_data[[col]])
    }
    expected_type <- codebook_dt$variable_type[i]
    
    if (actual_type != expected_type) {
      vartype_messages <- c(vartype_messages,
                            paste0('column "', col, '" (', i, '): expected type "', expected_type, '" but found "', actual_type, '"'))
    }
  }
  
  # Skip valid values check for list structures (they have nested values)
  validvals_messages <- character()
  if (!is_list_structure) {
    for (i in seq_len(nrow(codebook_dt))) {
      col <- codebook_dt$col_name[i]
      if (!(col %in% dt_colnames)) next
      if (col %in% c(ignore_cols, "path")) next
      
      valid_values_raw <- codebook_dt$valid_values[i]
      if (is.na(valid_values_raw) || valid_values_raw == "") next
      
      valid_values_list <- str_trim(unlist(strsplit(valid_values_raw, ";")))
      actual_values <- unique(as.character(loaded_data[[col]]))
      invalid_values <- setdiff(actual_values, valid_values_list)
      
      if (length(invalid_values) > 0) {
        invalid_values_msg <- paste(invalid_values, collapse = "\n")
        validvals_messages <- c(validvals_messages,
                                paste0('column "', col, '" (', i, '): unexpected values found:\n', invalid_values_msg))
      }
    }
  } else {
    validvals_messages <- "Valid values check skipped for list structure"
  }
  
  # Print results
  cat("Testing data structure for:", dt_name, "\n")
  cat("File path:", file_path, "\n")
  cat("Data type:", ifelse(is_list_structure, "list", "data.table"), "\n")
  
  if (is_list_structure) {
    cat("List elements:", length(dt_colnames), "\n\n")
  } else {
    cat("Data dimensions:", nrow(loaded_data), "rows x", ncol(loaded_data), "columns\n\n")
  }
  
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
  if (is_list_structure) {
    cat(validvals_messages, "\n")
  } else {
    if (length(validvals_messages) == 0) {
      cat("all values correct\n")
    } else {
      cat(paste0(validvals_messages, collapse = "\n"), "\n")
    }
  }
  
  # Clean up: remove the loaded data from memory
  rm(loaded_data)
  
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
  
  # remove possible duplicates
  input_dt <- unique(input_dt, by = "result_link")
  
  # Helper function to clean domains from URLs, remove 'www.' and TLDs (.de, .com, etc.)
  .extract_domain <- function(url) {
    url
  }
  
  # Create the new input dataset with cleaned domain and initialized flags
  input <- input_dt[, .(
    id = .I,                       # unique incremental identifier
    domain = .extract_domain(domain_url),  # cleaned domain
    url = result_link,
    processed = FALSE,             # initialize as FALSE
    total_requests = 0L,           # initialize as 0
    retry = FALSE,                 # initialize as FALSE
    parse_error = FALSE,          # initialize as FALSE
    error = FALSE                  # initialize as FALSE
    
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
rm("generate_user_agent", "create_user_agents_table",
            "save_user_agents", "test_user_agents")



#####



# 5. Initialize parse error dataset if not existing 
init_parse_error_dataset <- function(paths) {
  # Define the full parse error file path
  parse_error_path <- file.path(paths$input, "parse_error.rds")
  
  # Check if parse_error.rds already exists to avoid overwriting
  if (file.exists(parse_error_path)) {
    message("Parse error dataset already exists at ", parse_error_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  parse_error <- data.table(
    id = integer(),                # integer (to match input id)
    domain = character(),          # character (cleaned domain from input)
    url = character(),             # character (URL from input)
    timestamp_scraped = as.POSIXct(character()),  # POSIXct (date-time of scraping attempt)
    date_time = character(),       # character (extracted date/time from content)
    author = character(),          # character (extracted author information)
    headline = character(),        # character (extracted headline/title)
    text = character(),            # character (extracted text content)
    paywall = logical(),           # logical (paywall detection flag)
    html_content = character()     # character (raw HTML content for debugging)
  )
  
  # Save the empty dataset as RDS file to the input path
  saveRDS(parse_error, parse_error_path)
  
  message("Parse error dataset successfully created and saved to ", parse_error_path)
  
  return(invisible(TRUE))
}

# Create parse error dataset
init_parse_error_dataset(paths)

# Test parse error dataset structure
test_data_structure("parse_error_ds")

# Clean up environment
rm(init_parse_error_dataset)

# Load if wanted 
# parse_error_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error.rds")



#####


# 6. Initialize retry dataset if not existing 
init_retry_dataset <- function(paths) {
  # Define the full retry file path
  retry_path <- file.path(paths$input, "retry.rds")
  
  # Check if retry.rds already exists to avoid overwriting
  if (file.exists(retry_path)) {
    message("Retry dataset already exists at ", retry_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  retry <- data.table(
    request_id = integer(),        # integer (ID from request log)
    response_id = integer(),       # integer (ID from response log)
    id = integer(),                # integer (to match input id)
    domain = character(),          # character (cleaned domain from input)
    url = character(),             # character (URL from input)
    timestamp_scraped = as.POSIXct(character()),  # POSIXct (date-time of scraping attempt)
    from_chunk = integer(),        # integer (chunk number for batch tracking)
    status_code = integer(),       # integer (HTTP status code)
    response_headers = list(),     # list (all response headers for analysis)
    response_body = character(),   # character (raw response body content)
    response_time = numeric(),     # numeric (response time in seconds)
    retry_reason = character(),    # character (reason for retry requirement)
    server_date = as.POSIXct(character()),  # POSIXct (server date from Date header)
    content_type = character(),    # character (content type from headers)
    content_length = integer(),    # integer (content length from headers or calculated)
    server = character(),          # character (server information from headers)
    user_agent_id = integer(),     # integer (user agent used for request)
    ip_address = character(),      # character (IP/VPN address used)
    dns_time = numeric(),          # numeric (DNS lookup time in seconds)
    connect_time = numeric(),      # numeric (connection establishment time in seconds)
    total_time = numeric(),        # numeric (total request time in seconds)
    curl_error_code = integer(),   # integer (curl error code for low-level errors)
    ssl_verify_result = integer(), # integer (SSL verification result code)
    redirect_count = integer(),    # integer (number of redirects followed)
    rate_limit_remaining = integer(),  # integer (remaining rate limit from headers)
    rate_limit_reset = as.POSIXct(character()),  # POSIXct (rate limit reset time)
    retry_after = integer()        # integer (retry after seconds from headers)
  )
  
  # Save the empty dataset as RDS file to the input path
  saveRDS(retry, retry_path)
  
  message("Retry dataset successfully created and saved to ", retry_path)
  
  return(invisible(TRUE))
}

# Create retry dataset
init_retry_dataset(paths)

# Test retry dataset structure
test_data_structure("retry_ds")

# Clean up environment
rm(init_retry_dataset)

# Load if wanted 
# retry_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/retry.rds")



#####



# 7. Initialize request log dataset if not existing 
init_request_log_dataset <- function(paths) {
  # Define the full request log file path
  request_log_path <- file.path(paths$logs, "request_log.rds")
  
  # Check if request_log.rds already exists to avoid overwriting
  if (file.exists(request_log_path)) {
    message("Request log dataset already exists at ", request_log_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Create empty request log with all columns matching module 10 logging function
  request_log <- data.table(
    # Core identification
    request_id = integer(),                          # integer (unique incremental request ID)
    id = integer(),                                  # integer (to match input id)
    domain = character(),                            # character (cleaned domain from input)
    url = character(),                               # character (URL from input)
    timestamp_scraped = as.POSIXct(character()),     # POSIXct (date-time of scraping attempt)
    from_chunk = integer(),                          # integer (chunk number for batch tracking)
    
    # Session and worker info
    session_id = character(),                        # character (session identifier)
    worker_id = integer(),                           # integer (worker process ID)
    user_agent_id = integer(),                       # integer (user agent used for request)
    ip_address = character(),                        # character (IP/VPN address used)
    
    # All header information as separate columns
    user_agent = character(),                        # character (full user agent string)
    accept = character(),                            # character (accept header value)
    accept_language = character(),                   # character (accept-language header)
    accept_encoding = character(),                   # character (accept-encoding header)
    connection = character(),                        # character (connection header: keep-alive/close)
    referer = character(),                           # character (referer URL)
    host = character(),                              # character (host header)
    upgrade_insecure_requests = character(),         # character (upgrade-insecure-requests value)
    sec_fetch_dest = character(),                    # character (sec-fetch-dest header)
    sec_fetch_mode = character(),                    # character (sec-fetch-mode header)
    sec_fetch_site = character(),                    # character (sec-fetch-site: cross-site/same-site)
    
    # Additional request metadata
    aggressiveness_level = integer(),                # integer (aggressiveness parameter 1-5)
    browser_type = character(),                      # character (chrome/firefox/safari/edge)
    is_mobile = logical(),                           # logical (TRUE if mobile user agent)
    is_first_request = logical(),                    # logical (TRUE if first request in session)
    session_request_count = integer(),               # integer (number of requests in this session)
    cookie_jar_path = character()                    # character (path to cookie jar file)
  )
  
  # Save the empty dataset as RDS file to the logs path
  saveRDS(request_log, request_log_path)
  
  message("Request log dataset successfully created and saved to ", request_log_path)
  
  return(invisible(TRUE))
}

# Create request log dataset
init_request_log_dataset(paths)

# Test request log dataset structure
test_data_structure("request_log_ds")

# Clean up environment
rm(init_request_log_dataset)

# Load if wanted 
# request_log_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/request_log.rds")



##### 



# 8. Initialize response log dataset if not existing 
init_response_log_dataset <- function(paths) {
  # Define the full response log file path
  response_log_path <- file.path(paths$logs, "response_log.rds")
  
  # Check if response_log.rds already exists to avoid overwriting
  if (file.exists(response_log_path)) {
    message("Response log dataset already exists at ", response_log_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Create empty response log with all columns matching module 10 logging function
  response_log <- data.table(
    # IDs and core info
    request_id = integer(),                          # integer (corresponding request ID)
    response_id = integer(),                         # integer (unique incremental response ID)
    id = integer(),                                  # integer (to match input id)
    domain = character(),                            # character (cleaned domain from input)
    url = character(),                               # character (URL from input)
    timestamp_scraped = as.POSIXct(character()),     # POSIXct (date-time of scraping attempt)
    from_chunk = integer(),                          # integer (chunk number for batch tracking)
    
    # Response details
    status_code = integer(),                         # integer (HTTP status code)
    response_headers = list(),                       # list (all response headers for analysis)
    response_time = numeric(),                       # numeric (response time in seconds)
    server_date = as.POSIXct(character()),          # POSIXct (server date from Date header)
    content_type = character(),                      # character (content type from headers)
    content_length = integer(),                      # integer (content length from headers or calculated)
    server = character(),                            # character (server information from headers)
    
    # Request context
    user_agent_id = integer(),                       # integer (user agent used for request)
    ip_address = character(),                        # character (IP/VPN address used)
    
    # Timing details
    dns_time = numeric(),                            # numeric (DNS lookup time in seconds)
    connect_time = numeric(),                        # numeric (connection establishment time in seconds)
    total_time = numeric(),                          # numeric (total request time in seconds)
    
    # Error and SSL info
    curl_error_code = integer(),                     # integer (curl error code for low-level errors)
    ssl_verify_result = integer(),                   # integer (SSL verification result code)
    redirect_count = integer(),                      # integer (number of redirects followed)
    
    # Rate limiting
    rate_limit_remaining = integer(),                # integer (remaining rate limit from headers)
    rate_limit_reset = as.POSIXct(character()),      # POSIXct (rate limit reset time)
    retry_after = integer(),                         # integer (retry after seconds from headers)
    
    # Response analysis result
    response_analysis = character()                  # character (analysis result from module 07)
  )
  
  # Save the empty dataset as RDS file to the logs path
  saveRDS(response_log, response_log_path)
  
  message("Response log dataset successfully created and saved to ", response_log_path)
  
  return(invisible(TRUE))
}

# Create response log dataset
init_response_log_dataset(paths)

# Test response log dataset structure
test_data_structure("response_log_ds")

# Clean up environment
rm(init_response_log_dataset)

# Load if wanted 
# response_log_ds <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/response_log.rds")



######



# 9. Initialize header parameters if not existing
init_header_params <- function(paths) {
  # Define the full header params file path
  header_params_path <- file.path(paths$input, "header_params.rds")
  
  # Check if header_params.rds already exists to avoid overwriting
  if (file.exists(header_params_path)) {
    message("Header params dataset already exists at ", header_params_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  message("Creating header parameters dataset...")
  
  # Initialize empty header_params structure as nested list
  header_params <- list(
    accept = list(),
    accept_language = list(),
    accept_encoding = list(),
    cross_site_referer = list(),
    same_site_referer = list(),
    host = list(),
    upgrade_insecure_requests = list(),
    sec_fetch_dest = list(),
    sec_fetch_mode = list(),
    sec_fetch_site = list()
  )
  
  # Load original input to get unique domains
  original_input_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/all_links_filtered_by_date.rds"
  
  if (!file.exists(original_input_path)) {
    stop("Original input file not found at: ", original_input_path)
  }
  
  # Read the original input
  original_input <- readRDS(original_input_path)
  setDT(original_input)
  
  # Get unique domain URLs
  unique_domains <- unique(original_input$domain_url)
  unique_domains <- unique_domains[!is.na(unique_domains) & unique_domains != ""]
  
  message(sprintf("Found %d unique domains", length(unique_domains)))
  
  # Fetch same-site referers for each domain
  message("Fetching same-site referers...")
  
  for (i in seq_along(unique_domains)) {
    domain_url <- unique_domains[i]
    
    # Clean domain for storage key (do this first for consistency)
    clean_domain <- sub("^https?://(?:www\\.)?", "", domain_url)
    clean_domain <- sub("/.*$", "", clean_domain)
    
    # Progress indicator
    if (i %% 10 == 0) {
      message(sprintf("Processing domain %d/%d", i, length(unique_domains)))
    }
    
    tryCatch({
      # Ensure URL has protocol
      if (!grepl("^https?://", domain_url)) {
        domain_url <- paste0("https://", domain_url)
      }
      
      # Send GET request to the domain
      response <- httr::GET(
        url = domain_url,
        httr::timeout(10),
        httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/123.0.0.0 Safari/537.36")
      )
      
      # Get the final URL after redirects
      final_url <- response$url
      
      # Store in the list with domain as key
      header_params$same_site_referer[[clean_domain]] <- final_url
      
      # Add small delay to avoid rate limiting
      Sys.sleep(runif(1, 0.5, 1.5))
      
    }, error = function(e) {
      # If request fails, use basic construction
      constructed_url <- paste0("https://", clean_domain, "/")
      header_params$same_site_referer[[clean_domain]] <- constructed_url
      message(sprintf("Failed to fetch %s, using constructed URL: %s", clean_domain, constructed_url))
    })
  }
  
  message("Completed fetching same-site referers")
  
  # Manually add missing domains for bild and bnn 
  if (!"bild.de" %in% names(header_params$same_site_referer)) {
    header_params$same_site_referer[["bild.de"]] <- "https://bild.de/"
    message("Manually added bild.de")
  }
  
  if (!"br.de" %in% names(header_params$same_site_referer)) {
    header_params$same_site_referer[["br.de"]] <- "https://br.de/"
    message("Manually added br.de")
  }
  
  # Set Accept headers with browser-specific options
  header_params$accept <- list(
    chrome_option_1 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7",
    chrome_option_2 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.9",
    firefox_option_1 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8",
    firefox_option_2 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    safari_option_1 = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
    safari_option_2 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    edge_option_1 = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7"
  )
  
  # Set Accept-Language headers with German preference
  header_params$accept_language <- list(
    german_primary_1 = "de-DE,de;q=0.9,en;q=0.8",
    german_primary_2 = "de,en-US;q=0.7,en;q=0.3",
    german_primary_3 = "de-DE,de;q=0.8,en-US;q=0.5,en;q=0.3",
    german_english_1 = "de-DE,de;q=0.9,en-US;q=0.8,en;q=0.7",
    german_english_2 = "de-DE,en;q=0.9",
    chrome_german = "de-DE,de;q=0.9,en;q=0.8",
    firefox_german = "de,en-US;q=0.7,en;q=0.3",
    safari_german = "de-DE,de;q=0.9"
  )
  
  # Set Accept-Encoding headers
  header_params$accept_encoding <- list(
    modern_full = "gzip, deflate, br, zstd",
    modern_standard = "gzip, deflate, br",
    legacy_standard = "gzip, deflate",
    chrome_standard = "gzip, deflate, br",
    firefox_standard = "gzip, deflate, br",
    safari_standard = "gzip, deflate, br"
  )
  
  # Set Cross-site referer URLs
  header_params$cross_site_referer <- list(
    google = "https://www.google.com/",
    facebook = "https://www.facebook.com/",
    linkedin = "https://www.linkedin.com/",
    instagram = "https://www.instagram.com/"
  )
  
  # Set Host headers (same as unique domains, cleaned)
  for (domain in unique_domains) {
    clean_domain <- sub("^https?://(?:www\\.)?", "", domain)
    clean_domain <- sub("/.*$", "", clean_domain)
    header_params$host[[clean_domain]] <- clean_domain
  }
  
  # Set static headers
  header_params$upgrade_insecure_requests <- list(
    default = "1"
  )
  
  header_params$sec_fetch_dest <- list(
    default = "document"
  )
  
  header_params$sec_fetch_mode <- list(
    default = "navigate"
  )
  
  header_params$sec_fetch_site <- list(
    cross_site = "cross-site",
    same_site = "same-site"
  )
  
  # Save the header params dataset as RDS file
  saveRDS(header_params, header_params_path)
  
  message("Header params dataset successfully created and saved to ", header_params_path)
  
  return(invisible(TRUE))
}

# Create header params
init_header_params(paths)

# Test header params dataset structure
test_data_structure("header_params_ds")

# Clean up environment
rm(init_header_params)

# Load if wanted 
# header_params <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/header_params.rds")



#####



# 10: Initialize error dataset if not existing
init_error_dataset <- function(paths) {
  # Define the full error file path
  error_path <- file.path(paths$output, "error.rds")
  
  # Check if error.rds already exists to avoid overwriting
  if (file.exists(error_path)) {
    message("'error.rds' already exists at ", error_path, ". Skipping creation.")
    return(invisible(TRUE))
  }
  
  # Create empty error dataset with defined structure
  error <- data.table(
    id = integer(),           # integer (to match input id)
    domain = character(),     # character (cleaned domain from input)
    url = character(),        # character (URL from input)
    error_reason = character() # character (specific error description)
  )
  
  # Save the empty dataset as RDS file to the input path
  saveRDS(error, error_path)
  
  message("Error dataset successfully created and saved to ", error_path)
  
  return(invisible(TRUE))
}

# Create error dataset
init_error_dataset(paths)

# Test error ds
test_data_structure("error_ds")

# Clean up environment
rm("init_error_dataset", "paths")

