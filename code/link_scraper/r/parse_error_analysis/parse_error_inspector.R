
setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")

# Helper function to define all necessary paths for the project
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input          = file.path(base_path, "data", "input"),
    output         = file.path(base_path, "data", "output"),
    config         = file.path(base_path, "data", "config"),
    parsing_config = file.path(base_path, "data", "config", "06_parsing_config"),
    state          = file.path(base_path, "data", "state"),
    logs           = file.path(base_path, "data", "logs"),
    chunk_outputs  = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs   = file.path(base_path, "data", "input",  "chunk_inputs"),
    parse_error    = file.path(base_path, "data", "input", "parse_error")
  )
}

# ==============================================================================
# MODULE: PARSE ERROR INSPECTOR
# ==============================================================================
#
# This module provides a suite of tools to inspect, analyze, and correct
# parsing errors from failed scrape attempts. It allows the user to load
# domain-specific parse error files, analyze which data fields are missing,
# sample raw HTML content, and locally test and edit parsing rules.
# Once the rules are corrected, this module can apply the new rules to the
# failed responses and integrate the successfully parsed data back into the
# main 'final_data.rds' dataset.
#
# This workflow avoids re-requesting URLs and allows for iterative refinement
# of parsing logic in a controlled, local environment.
#
# ==============================================================================

# Load required packages
library(data.table)
library(rvest)
library(stringr)
library(jsonlite)
library(lubridate)

# Source necessary functions from other modules
# Make sure the paths are correct based on your working directory
source("09_storage_manager.R")
source("11_overview_manager.R")
source("06_html_parser.R") # For the original parse function structure

# --- 1. INITIAL SETUP AND OVERVIEW ---

#' @title Setup Local Parsing Rules
#' @description Copies the original parsing, paywall, and bot detection rule
#' files to create local versions for safe editing and testing. This should be
#' run once at the start of an inspection session.
setup_local_rules <- function() {
  paths <- get_module_paths()
  config_dir <- paths$parsing_config
  
  files_to_copy <- c(
    "06_bot_detection_rules.rds",
    "06_paywall_rules_generated.rds",
    "06_parser_rules_fetched.rds"
  )
  
  message("Creating local copies of parsing rule files...")
  
  for (file in files_to_copy) {
    original_path <- file.path(config_dir, file)
    local_path <- file.path(config_dir, gsub("\\.rds$", "_local.rds", file))
    
    if (file.exists(original_path)) {
      file.copy(original_path, local_path, overwrite = TRUE)
      message(sprintf("  -> Copied '%s' to '%s'", basename(original_path), basename(local_path)))
    } else {
      warning(sprintf("Original rule file not found: %s", original_path))
    }
  }
  
  message("Local rule setup complete.")
  invisible(TRUE)
}


#' @title Create Parse Error Overview
#' @description Generates a summary table of domains that have parse errors,
#' showing the total number of links, scraped links, and parse errors.
#' @return A data.table with the parse error overview.
create_parse_error_overview <- function() {
  message("Generating parse error overview...")
  
  # Generate the full progress report from module 11
  progress_report <- func_11_generate_progress_report()
  
  # The column name from the report is "Parse Error Links".
  # We must use backticks to reference it because of the spaces.
  required_col <- "Parse Error Links"
  if (!required_col %in% names(progress_report)) {
    stop(sprintf("The progress report from func_11 does not contain the expected '%s' column.", required_col))
  }
  
  # Filter for domains with parse errors
  # The `get()` function is used here to access the column by its string name
  parse_error_domains <- progress_report[get(required_col) > 0]
  
  if (nrow(parse_error_domains) == 0) {
    message("No domains with parse errors found.")
    return(data.table())
  }
  
  # The column names in the report are already user-friendly.
  # We just select the ones we want for this specific overview.
  overview_cols <- c("Domain", "Input Links", "Scraped Links", "Parse Error Links", "Parse Error Percentage")
  
  # Ensure all required columns exist before creating the final table
  missing_cols <- setdiff(overview_cols, names(parse_error_domains))
  if (length(missing_cols) > 0) {
    stop(sprintf("The progress report is missing the following required columns: %s", paste(missing_cols, collapse = ", ")))
  }
  
  overview <- parse_error_domains[, ..overview_cols]
  
  message("Parse error overview created successfully.")
  return(overview)
}


# --- 2. DATA LOADING AND INSPECTION FUNCTIONS ---

#' @title Load Parse Error Data
#' @description Loads the entire domain-specific parse error RDS file into the
#' global environment for detailed inspection.
#' @param domain_name The name of the domain (e.g., "bild", "schwaebische.de").
load_parse_error <- function(domain_name) {
  paths <- get_module_paths()
  # Use the provided domain name directly to build the file path
  error_file_path <- file.path(paths$parse_error, paste0(domain_name, "_parse_error.rds"))
  
  if (!file.exists(error_file_path)) {
    stop(sprintf("Parse error file for domain '%s' not found at: %s", domain_name, error_file_path))
  }
  
  # Clean the domain name for use as an R object name
  domain_clean_for_object <- sub("\\..*$", "", domain_name)
  
  error_dt <- readRDS(error_file_path)
  object_name <- paste0(domain_clean_for_object, "_parse_error")
  assign(object_name, error_dt, envir = .GlobalEnv)
  
  message(sprintf("Loaded %d parse error entries for '%s' into object '%s'.", nrow(error_dt), domain_name, object_name))
  invisible(TRUE)
}


#' @title Analyze Parse Error Fields
#' @description Analyzes field completion for either an original parse error RDS
#' file from disk or a local data.table object.
#' @param input_data If original is TRUE, the domain name (string). If FALSE,
#' the data.table object itself.
#' @param original Logical. If TRUE (default), loads from the parse_error RDS.
#' If FALSE, analyzes the provided local data.table.
analyze_parse_error <- function(input_data, original = TRUE) {
  
  if (original) {
    # --- Original behavior: Load from RDS file ---
    domain_name <- input_data
    if (!is.character(domain_name) || length(domain_name) != 1) {
      stop("When 'original = TRUE', the first argument must be the domain name as a string.")
    }
    
    paths <- get_module_paths()
    error_file_path <- file.path(paths$parse_error, paste0(domain_name, "_parse_error.rds"))
    
    if (!file.exists(error_file_path)) {
      stop(sprintf("Parse error file for domain '%s' not found at: %s", domain_name, error_file_path))
    }
    
    message(sprintf("Temporarily loading original parse error file for '%s'...", domain_name))
    error_dt <- readRDS(error_file_path)
    # Ensure temporary data is cleaned up even if an error occurs
    on.exit({
      rm(error_dt)
      gc()
      message("Temporary data has been cleared from memory.")
    }, add = TRUE)
    
    analysis_target_name <- domain_name
    
  } else {
    # --- New behavior: Analyze local data.table ---
    analysis_target_name <- deparse(substitute(input_data))
    if (!is.data.frame(input_data)) {
      stop("When 'original = FALSE', the first argument must be a data.frame or data.table object.")
    }
    error_dt <- as.data.table(input_data)
    message(sprintf("Analyzing local data.table '%s'...", analysis_target_name))
  }
  
  total_rows <- nrow(error_dt)
  
  if (total_rows == 0) {
    message(sprintf("Input data for '%s' is empty.", analysis_target_name))
    return(invisible())
  }
  
  fields_to_check <- c("date_time", "author", "headline", "text")
  
  # Convert empty strings to NA before analysis
  # This ensures that fields containing just "" are treated as missing.
  for (field in fields_to_check) {
    if (field %in% names(error_dt) && is.character(error_dt[[field]])) {
      # Using `:=` for efficient in-place modification
      error_dt[get(field) == "", (field) := NA_character_]
    }
  }
  
  message(sprintf("--- Analyzing Field Completion for '%s' (%d entries) ---", analysis_target_name, total_rows))
  
  for (field in fields_to_check) {
    if (field %in% names(error_dt)) {
      non_na_count <- sum(!is.na(error_dt[[field]]))
      completion_pct <- (non_na_count / total_rows) * 100
      message(sprintf("  - %-10s: %.2f%% complete (%d / %d)", field, completion_pct, non_na_count, total_rows))
    } else {
      message(sprintf("  - %-10s: Column not found.", field))
    }
  }
  
  message("---------------------------------------------------------")
  message("Analysis complete.")
  
  invisible(TRUE)
}


#' @title Sample HTML Content
#' @description Loads a domain's parse error file, creates a random sample,
#' saves the sample to the global environment, and removes the full dataset
#' from memory.
#' @param domain_name The name of the domain (e.g., "bild", "schwaebische.de").
#' @param sample_count The number of random samples to draw.
sample_htmls <- function(domain_name, sample_count = 5) {
  paths <- get_module_paths()
  # Use the provided domain name directly to build the file path
  error_file_path <- file.path(paths$parse_error, paste0(domain_name, "_parse_error.rds"))
  
  if (!file.exists(error_file_path)) {
    stop(sprintf("Parse error file for domain '%s' not found at: %s", domain_name, error_file_path))
  }
  
  # Load the data temporarily
  message(sprintf("Temporarily loading parse error file for '%s' to create sample...", domain_name))
  source_dt <- readRDS(error_file_path)
  
  if (nrow(source_dt) < sample_count) {
    warning(sprintf("Sample count (%d) is larger than the number of available entries (%d). Using all entries.", sample_count, nrow(source_dt)))
    sample_count <- nrow(source_dt)
  }
  
  # Create the sample
  sample_dt <- source_dt[sample(.N, sample_count)]
  
  # Clean the domain name for use as an R object name
  domain_clean_for_object <- sub("\\..*$", "", domain_name)
  sample_object_name <- paste0(domain_clean_for_object, "_sample")
  assign(sample_object_name, sample_dt, envir = .GlobalEnv)
  
  message(sprintf("Created sample object '%s' with %d random entries for domain '%s'.", sample_object_name, sample_count, domain_name))
  
  # Clean up the large source data.table
  rm(source_dt)
  gc()
  message("Full parse error data has been cleared from memory, only the sample remains.")
  
  invisible(TRUE)
}


# --- 3. LOCAL PARSING AND RULE MANAGEMENT ---

#' @title Parse HTML Locally
#' @description Parses a local data.table (e.g., a sample) using the LOCAL
#' parsing rules and saves the result to a new object.
#' @param data_table_input The data.table object to parse, or its name as a string.
parse_html_local <- local({
  
  .local_rules_cache <- list()
  
  .load_local_rules <- function(force_reload = FALSE) {
    # The cache is ignored if force_reload is TRUE or if it's empty
    if (force_reload || length(.local_rules_cache) == 0) {
      if (force_reload) message("Forcing reload of local parsing rules from disk...")
      else message("Loading local parsing rules for the first time...")
      
      paths <- get_module_paths()
      config_dir <- paths$parsing_config
      
      .local_rules_cache$parser <<- readRDS(file.path(config_dir, "06_parser_rules_fetched_local.rds"))
      .local_rules_cache$paywall <<- readRDS(file.path(config_dir, "06_paywall_rules_generated_local.rds"))
      .local_rules_cache$bot <<- readRDS(file.path(config_dir, "06_bot_detection_rules_local.rds"))
    }
  }
  
  function(data_table_input, force_reload = FALSE) {
    .load_local_rules(force_reload)
    
    data_table_name <- deparse(substitute(data_table_input))
    
    if (is.character(data_table_input)) {
      data_table_name <- data_table_input
      if (!exists(data_table_name, envir = .GlobalEnv)) {
        stop(sprintf("Data.table '%s' not found in the global environment.", data_table_name))
      }
      source_dt <- get(data_table_name, envir = .GlobalEnv)
    } else if (is.data.frame(data_table_input)) {
      source_dt <- as.data.table(data_table_input)
    } else {
      stop("Invalid input for 'data_table_input'. Please provide a data.table object or its name as a string.")
    }
    
    domain_clean <- sub("\\..*$", "", source_dt$domain[1])
    message(sprintf("--- Locally parsing %d entries from '%s' for domain '%s' ---", nrow(source_dt), data_table_name, domain_clean))
    
    parser_entry <- .local_rules_cache$parser[[domain_clean]]
    if (is.null(parser_entry) || !isTRUE(parser_entry$success)) {
      stop(sprintf("No valid local parsing function found for domain '%s'.", domain_clean))
    }
    parsing_func <- parser_entry$parse_function
    
    results_list <- list()
    pb <- txtProgressBar(min = 0, max = nrow(source_dt), style = 3)
    
    for (i in 1:nrow(source_dt)) {
      row_data <- source_dt[i]
      html_content <- row_data$html_content
      html_parsed <- tryCatch(rvest::read_html(html_content), error = function(e) NULL)
      
      parsed_data <- list(
        date_time = as.POSIXct(NA), author = NA_character_,
        headline = NA_character_, text = NA_character_
      )
      
      if (!is.null(html_parsed)) {
        extracted_data <- tryCatch(parsing_func(html_parsed), error = function(e) list(error = e$message))
        if (is.null(extracted_data$error)) {
          parsed_data$date_time <- extracted_data$datetime[1]
          parsed_data$author    <- as.character(extracted_data$author[1])
          parsed_data$headline  <- as.character(extracted_data$headline[1])
          parsed_data$text      <- as.character(extracted_data$text[1])
        }
      }
      
      results_list[[i]] <- data.table(
        id = row_data$id, domain = row_data$domain, url = row_data$url,
        timestamp_scraped = row_data$timestamp_scraped,
        date_time = parsed_data$date_time, author = parsed_data$author,
        headline = parsed_data$headline, text = parsed_data$text,
        paywall = row_data$paywall
      )
      setTxtProgressBar(pb, i)
    }
    close(pb)
    
    result_dt <- rbindlist(results_list, fill = TRUE)
    result_object_name <- paste0(data_table_name, "_parsed_local")
    assign(result_object_name, result_dt, envir = .GlobalEnv)
    
    message(sprintf("\n--- Parsing complete. Results saved to '%s'. ---", result_object_name))
    print(head(result_dt))
    invisible(TRUE)
  }
})



#' @title Show Parse Rules
#' @description Prints the current LOCAL parsing rules for a specific domain to the console.
#' @param domain_name The name of the domain.
show_parse_rules <- function(domain_name) {
  domain_clean <- sub("\\..*$", "", domain_name)
  paths <- get_module_paths()
  config_dir <- paths$parsing_config
  
  # Load local rules
  parser_rules <- readRDS(file.path(config_dir, "06_parser_rules_fetched_local.rds"))
  paywall_rules <- readRDS(file.path(config_dir, "06_paywall_rules_generated_local.rds"))
  bot_rules <- readRDS(file.path(config_dir, "06_bot_detection_rules_local.rds"))
  
  message(sprintf("--- Current LOCAL Parsing Rules for '%s' ---", domain_clean))
  
  # Show content parsing rules
  message("\n--- Content Parsing (from Paperboy) ---")
  parser_entry <- parser_rules[[domain_clean]]
  if (!is.null(parser_entry) && isTRUE(parser_entry$success)) {
    cat(parser_entry$raw_code)
  } else {
    message("No content parsing rules found.")
  }
  
  # Show paywall rules
  message("\n\n--- Paywall Detection Rules ---")
  paywall_entry <- paywall_rules[[domain_clean]]
  if (!is.null(paywall_entry)) {
    print(paywall_entry)
  } else {
    message("No paywall rules found.")
  }
  
  # Bot detection rules are global, but we show them for completeness
  message("\n\n--- Bot Detection Rules (Global) ---")
  print(bot_rules)
  
  message("\n--- End of Rules ---")
  invisible(TRUE)
}


#' @title Edit Parse Rules
#' @description Modifies a rule for a given domain in the LOCAL rule files.
#' @param domain_name The name of the domain.
#' @param rule_type The type of rule to edit. Must be one of: "parser_rules",
#' "paywall_rules", "bot_detection_rules".
#' @param new_rule For "parser_rules", a string containing the entire new
#' function. For others, a list with the new rule structure.
edit_parse_rules <- function(domain_name, rule_type, new_rule) {
  domain_clean <- sub("\\..*$", "", domain_name)
  paths <- get_module_paths()
  config_dir <- paths$parsing_config
  
  # Validate rule_type
  valid_types <- c("parser_rules", "paywall_rules", "bot_detection_rules")
  if (!rule_type %in% valid_types) {
    stop(sprintf("'rule_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }
  
  target_file <- switch(rule_type,
                        "parser_rules" = "06_parser_rules_fetched_local.rds",
                        "paywall_rules" = "06_paywall_rules_generated_local.rds",
                        "bot_detection_rules" = "06_bot_detection_rules_local.rds"
  )
  
  file_path <- file.path(config_dir, target_file)
  rules <- readRDS(file_path)
  
  if (rule_type == "parser_rules") {
    if (!is.character(new_rule) || length(new_rule) != 1) {
      stop("For 'parser_rules', 'new_rule' must be a single string containing the entire new function code.")
    }
    
    # Try to evaluate the new function string to ensure it's valid R code
    new_func <- tryCatch({
      eval(parse(text = new_rule))
    }, error = function(e) {
      stop(sprintf("The provided string for 'new_rule' is not a valid function. R error: %s", e$message))
    })
    
    if (!is.function(new_func)) {
      stop("The provided string for 'new_rule' did not evaluate to a function.")
    }
    
    rules[[domain_clean]]$raw_code <- new_rule
    rules[[domain_clean]]$parse_function <- new_func
    
  } else if (rule_type == "paywall_rules") {
    if (!is.list(new_rule)) stop("For 'paywall_rules', 'new_rule' must be a list.")
    rules[[domain_clean]] <- new_rule
    
  } else if (rule_type == "bot_detection_rules") {
    if (!is.list(new_rule)) stop("For 'bot_detection_rules', 'new_rule' must be a list.")
    # Bot detection rules are global, so we replace the whole object
    rules <- new_rule
    message(sprintf("Note: Bot detection rules are global. The entire rule set in '%s' has been replaced.", target_file))
  }
  
  saveRDS(rules, file_path)
  message(sprintf("Successfully updated '%s' for domain '%s' in file '%s'.", rule_type, domain_name, target_file))
  invisible(TRUE)
}


# --- 4. APPLYING AND RESETTING RULES ---

#' @title Apply New Rule
#' @description Copies a corrected rule from the LOCAL file to the ORIGINAL
#' production rule file. Use with caution.
#' @param domain_name The name of the domain.
#' @param rule_type The type of rule to apply. Must be one of: "parser_rules",
#' "paywall_rules", "bot_detection_rules".
apply_new_rule <- function(domain_name, rule_type) {
  domain_clean <- sub("\\..*$", "", domain_name)
  paths <- get_module_paths()
  config_dir <- paths$parsing_config
  
  # Validate rule_type
  valid_types <- c("parser_rules", "paywall_rules", "bot_detection_rules")
  if (!rule_type %in% valid_types) {
    stop(sprintf("'rule_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }
  
  # Determine local and original file paths
  local_file <- switch(rule_type,
                       "parser_rules" = "06_parser_rules_fetched_local.rds",
                       "paywall_rules" = "06_paywall_rules_generated_local.rds",
                       "bot_detection_rules" = "06_bot_detection_rules_local.rds"
  )
  original_file <- gsub("_local", "", local_file)
  
  local_path <- file.path(config_dir, local_file)
  original_path <- file.path(config_dir, original_file)
  
  # Load both files
  local_rules <- readRDS(local_path)
  original_rules <- readRDS(original_path)
  
  # Copy the rule from local to original
  if (rule_type == "bot_detection_rules") {
    original_rules <- local_rules
    message(sprintf("Applying global '%s' from local to production file.", rule_type))
  } else {
    original_rules[[domain_clean]] <- local_rules[[domain_clean]]
    message(sprintf("Applying '%s' for domain '%s' from local to production file.", rule_type, domain_name))
  }
  
  # Save the updated original file
  saveRDS(original_rules, original_path)
  
  message(sprintf("Successfully applied new rule to production file '%s'.", original_file))
  invisible(TRUE)
}


#' @title Reset Rule
#' @description Resets a rule in the LOCAL file back to its state in the
#' ORIGINAL production file.
#' @param domain_name The name of the domain.
#' @param rule_type The type of rule to reset. Must be one of: "parser_rules",
#' "paywall_rules", "bot_detection_rules".
reset_rule <- function(domain_name, rule_type) {
  domain_clean <- sub("\\..*$", "", domain_name)
  paths <- get_module_paths()
  config_dir <- paths$parsing_config
  
  # Validate rule_type
  valid_types <- c("parser_rules", "paywall_rules", "bot_detection_rules")
  if (!rule_type %in% valid_types) {
    stop(sprintf("'rule_type' must be one of: %s", paste(valid_types, collapse = ", ")))
  }
  
  # Determine local and original file paths
  local_file <- switch(rule_type,
                       "parser_rules" = "06_parser_rules_fetched_local.rds",
                       "paywall_rules" = "06_paywall_rules_generated_local.rds",
                       "bot_detection_rules" = "06_bot_detection_rules_local.rds"
  )
  original_file <- gsub("_local", "", local_file)
  
  local_path <- file.path(config_dir, local_file)
  original_path <- file.path(config_dir, original_file)
  
  # Load both files
  local_rules <- readRDS(local_path)
  original_rules <- readRDS(original_path)
  
  # Copy the rule from original to local
  if (rule_type == "bot_detection_rules") {
    local_rules <- original_rules
    message(sprintf("Resetting global '%s' in local file from production version.", rule_type))
  } else {
    local_rules[[domain_clean]] <- original_rules[[domain_clean]]
    message(sprintf("Resetting '%s' for domain '%s' in local file from production version.", rule_type, domain_name))
  }
  
  # Save the updated local file
  saveRDS(local_rules, local_path)
  
  message(sprintf("Successfully reset local rule in '%s'.", local_file))
  invisible(TRUE)
}


# --- 5. FINAL DATA INTEGRATION ---

#' @title Compare Results with Paperboy
#' @description Re-scrapes URLs from a parsed sample using the paperboy package
#' and compares the results to the local parser's output.
#' @param parsed_local_dt A data.table object, typically the result of
#' `parse_html_local`.
#' @return A data.table with added columns for paperboy results and matches.
compare_results <- function(parsed_local_dt) {
  # Check for paperboy package and install if not present
  if (!requireNamespace("paperboy", quietly = TRUE)) {
    message("The 'paperboy' package is not installed. Installing now...")
    install.packages("paperboy")
  }
  library(paperboy)
  
  # Ensure input is a data.table
  if (!is.data.table(parsed_local_dt)) {
    parsed_local_dt <- as.data.table(parsed_local_dt)
  }
  
  dt <- copy(parsed_local_dt)
  
  # Add new columns for paperboy results
  dt[, `:=`(
    datetime_pb = as.POSIXct(NA),
    author_pb = NA_character_,
    headline_pb = NA_character_,
    text_pb = NA_character_
  )]
  
  message(sprintf("Re-scraping %d URLs with paperboy for comparison...", nrow(dt)))
  pb <- txtProgressBar(min = 0, max = nrow(dt), style = 3)
  
  for (i in 1:nrow(dt)) {
    url_to_scrape <- dt$url[i]
    # Use tryCatch to handle potential errors from paperboy
    paperboy_result <- tryCatch({
      # Correctly call the main exported function from the paperboy package
      paperboy::pb_deliver(url_to_scrape, verbose = FALSE)
    }, error = function(e) {
      message(sprintf("\nPaperboy failed for URL %s: %s", url_to_scrape, e$message))
      NULL # Return NULL on error
    })
    
    if (!is.null(paperboy_result) && nrow(paperboy_result) > 0) {
      # Use set() for efficient updating
      set(dt, i, "datetime_pb", paperboy_result$datetime[1])
      set(dt, i, "author_pb", toString(paperboy_result$author[1]))
      set(dt, i, "headline_pb", paperboy_result$headline[1])
      set(dt, i, "text_pb", paperboy_result$text[1])
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  message("\nScraping complete. Comparing results...")
  
  # --- Comparison Logic ---
  # Helper function for clean comparison (treats two NAs as a match)
  compare_values <- function(a, b) {
    (is.na(a) & is.na(b)) | (!is.na(a) & !is.na(b) & a == b)
  }
  
  dt[, `:=`(
    datetime_match = compare_values(date_time, datetime_pb),
    author_match = compare_values(author, author_pb),
    headline_match = compare_values(headline, headline_pb),
    text_match = compare_values(text, text_pb)
  )]
  
  # --- Summary Report ---
  total_rows <- nrow(dt)
  
  # Calculate per-column match percentage
  datetime_match_pct <- sum(dt$datetime_match, na.rm = TRUE) / total_rows * 100
  author_match_pct <- sum(dt$author_match, na.rm = TRUE) / total_rows * 100
  headline_match_pct <- sum(dt$headline_match, na.rm = TRUE) / total_rows * 100
  text_match_pct <- sum(dt$text_match, na.rm = TRUE) / total_rows * 100
  
  # Calculate overall match percentage (rows where everything matched)
  dt[, all_match := datetime_match & author_match & headline_match & text_match]
  overall_match_pct <- sum(dt$all_match, na.rm = TRUE) / total_rows * 100
  
  message("\n--- Comparison Summary ---")
  message(sprintf("Overall Perfect Match: %.2f%% (%d / %d rows)",
                  overall_match_pct, sum(dt$all_match, na.rm = TRUE), total_rows))
  message("\n--- Match Rate by Column ---")
  message(sprintf("  - Datetime: %.2f%%", datetime_match_pct))
  message(sprintf("  - Author:   %.2f%%", author_match_pct))
  message(sprintf("  - Headline: %.2f%%", headline_match_pct))
  message(sprintf("  - Text:     %.2f%%", text_match_pct))
  message("--------------------------")
  
  return(dt)
}


#' @title Fill Results
#' @description Appends the data from a locally parsed data.table to the main
#' 'final_data.rds' file, applying validation rules, and runs deduplication.
#' @param parsed_local_dt_name The string name of the `_parsed_local` data.table.
fill_results <- function(parsed_local_dt_name) {
  if (!exists(parsed_local_dt_name, envir = .GlobalEnv)) {
    stop(sprintf("Object '%s' not found in the global environment.", parsed_local_dt_name))
  }
  
  local_dt <- get(parsed_local_dt_name, envir = .GlobalEnv)
  
  if (nrow(local_dt) == 0) {
    message("Parsed data table is empty. Nothing to fill.")
    return(invisible(TRUE))
  }
  
  # --- Data Sanitization and Validation ---
  message("Sanitizing and validating local data before appending...")
  
  # Create a copy to avoid modifying the original local object
  dt_to_append <- copy(local_dt)
  
  # 1. Convert empty strings "" to NA for character columns
  for (col in names(dt_to_append)) {
    if (is.character(dt_to_append[[col]])) {
      dt_to_append[get(col) == "", (col) := NA_character_]
    }
  }
  
  # 2. Apply the core validation rule: 'date_time' and 'text' must not be NA
  rows_before_filter <- nrow(dt_to_append)
  dt_to_append <- dt_to_append[!is.na(date_time) & !is.na(text)]
  rows_after_filter <- nrow(dt_to_append)
  
  rows_discarded <- rows_before_filter - rows_after_filter
  if (rows_discarded > 0) {
    message(sprintf("  -> Discarded %d rows due to missing 'date_time' or 'text'.", rows_discarded))
  }
  
  if (rows_after_filter == 0) {
    message("No valid rows remaining after validation. Nothing to append.")
    return(invisible(TRUE))
  }
  
  # --- Appending to final_data.rds ---
  paths <- get_module_paths()
  final_data_path <- file.path(paths$output, "final_data.rds")
  
  if (!file.exists(final_data_path)) {
    stop("'final_data.rds' not found. Cannot append results.")
  }
  
  message(sprintf("Appending %d valid rows from '%s' to 'final_data.rds'.", nrow(dt_to_append), parsed_local_dt_name))
  
  final_data_dt <- readRDS(final_data_path)
  
  # Combine and save
  combined_dt <- rbindlist(list(final_data_dt, dt_to_append), use.names = TRUE, fill = TRUE)
  saveRDS(combined_dt, final_data_path)
  
  message("Append complete. Now running deduplication...")
  
  # Clean duplicates
  func_09_clean_dupes()
  
  message("Results filled and duplicates cleaned successfully.")
  invisible(TRUE)
}

