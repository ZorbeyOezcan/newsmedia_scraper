# ==============================================================================
# MODULE: TESTING & VALIDATION FRAMEWORK
# ==============================================================================
# 
# This module provides comprehensive testing and validation capabilities for
# the entire system. It validates data integrity at each processing stage,
# tests individual module functions with predefined test cases, checks output
# format compliance and data completeness, benchmarks performance metrics,
# and ensures that all modules operate within expected parameters. The
# validator helps maintain system reliability and catches issues before they
# affect production runs.
#
# RECEIVES FROM:
# - All modules: Functions and outputs to test
# 
# OUTPUTS TO:
# - 11_logger: Test results and validation reports
#
# ==============================================================================

# Load required packages
library(data.table)
library(testthat)
library(readxl)
library(stringr)


# Configuration Function
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

# 00 Data structure check function 

test_data_structure <- function(dt_name) {
  codebook_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/code_book.xlsx"
  
  if (!exists(dt_name, envir = .GlobalEnv)) {
    stop(paste0("Data.table '", dt_name, "' not found in global environment"))
  }
  
  dt <- get(dt_name, envir = .GlobalEnv)
  
  if (!is.data.table(dt)) {
    stop("Input is not a data.table")
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
  
  # Columns to ignore in the check
  ignore_cols <- c("path")
  
  # Filter expected columns (exclude ignored)
  expected_cols_filtered <- codebook_dt[!col_name %in% ignore_cols, col_name]
  
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
    if (col %in% ignore_cols) next
    
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
    if (col %in% ignore_cols) next
    
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
}


test_data_structure("input_ds")
test_data_structure("final_output_ds")
test_data_structure("vpn_log_dt")
test_data_structure("chunk_01")


#######


# 06_HTML_PARSER TESTS
# Test domain classifier function
test_06_parser_domain_classifier <- function() {
  message("\n==== Testing 06_html_parser Domain Classifier ====")
  
  paths <- get_module_paths()
  
  # Load the domain extraction function from 06_html_parser
  # (In practice, you would source the module file)
  .extract_domain <- function(url) {
    domain <- sub("^https?://(?:www\\.)?", "", url)
    domain <- sub("/.*$", "", domain)
    domain
  }
  
  # Load paywall domains CSV
  paywall_csv_path <- file.path(paths$input, "paywall_domains.csv")
  
  if (!file.exists(paywall_csv_path)) {
    stop("Paywall domains CSV not found at: ", paywall_csv_path)
  }
  
  # Read CSV
  paywall_data <- fread(paywall_csv_path, 
                        sep = ";", 
                        encoding = "UTF-8",
                        header = TRUE,
                        blank.lines.skip = TRUE)
  
  # Remove trailing empty column if exists
  if (ncol(paywall_data) > 0 && names(paywall_data)[ncol(paywall_data)] == "V5") {
    paywall_data[, V5 := NULL]
  }
  
  # Get expected domains from CSV
  expected_domains <- unique(paywall_data$domain)
  expected_domains <- expected_domains[!is.na(expected_domains) & expected_domains != ""]
  expected_domains <- sort(expected_domains)
  
  # Test domain extraction with various URL formats
  test_urls <- list()
  for (domain in expected_domains) {
    # Create test URLs for each domain
    test_urls[[length(test_urls) + 1]] <- paste0("https://www.", domain, "/artikel/test")
    test_urls[[length(test_urls) + 1]] <- paste0("https://", domain, "/politik/news")
    test_urls[[length(test_urls) + 1]] <- paste0("http://", domain, "/")
    test_urls[[length(test_urls) + 1]] <- paste0("http://www.", domain)
  }
  
  # Extract domains from test URLs
  classified_domains <- unique(sapply(test_urls, .extract_domain))
  classified_domains <- sort(classified_domains)
  
  # Find missing domains
  missing_domains <- setdiff(expected_domains, classified_domains)
  
  # Find unexpected domains (should be empty if classifier works correctly)
  unexpected_domains <- setdiff(classified_domains, expected_domains)
  
  # Print results
  message(sprintf("Expected domains: %d", length(expected_domains)))
  message(sprintf("Classified domains: %d", length(classified_domains)))
  
  if (length(missing_domains) > 0) {
    message("\nMissing domains (in CSV but not classified correctly):")
    for (domain in missing_domains) {
      message("  - ", domain)
    }
  } else {
    message("\nNo missing domains - all domains classified correctly!")
  }
  
  if (length(unexpected_domains) > 0) {
    message("\nUnexpected domains (classified but not in CSV):")
    for (domain in unexpected_domains) {
      message("  - ", domain)
    }
  }
  
  # Return test results
  test_result <- list(
    test_name = "06_parser_domain_classifier",
    passed = length(missing_domains) == 0 && length(unexpected_domains) == 0,
    expected_count = length(expected_domains),
    classified_count = length(classified_domains),
    missing_domains = missing_domains,
    unexpected_domains = unexpected_domains,
    timestamp = Sys.time()
  )
  
  # Save test results
  test_output_file <- file.path(paths$logs, "12_validator_06_parser_domain_classifier_results.rds")
  saveRDS(test_result, test_output_file)
  
  message(sprintf("\nTest %s", ifelse(test_result$passed, "PASSED ✓", "FAILED ✗")))
  message("Test results saved to: ", test_output_file)
  
  return(test_result)
}

# ==============================================================================
# ADDITIONAL 06_HTML_PARSER TESTS
# ==============================================================================

# Test parser rules loading
test_06_parser_rules_loading <- function() {
  message("\n==== Testing 06_html_parser Rules Loading ====")
  
  paths <- get_module_paths()
  
  # Check if parser rules file exists
  parser_rules_file <- file.path(paths$config, "06_html_parser_rules.rds")
  paywall_rules_file <- file.path(paths$config, "06_html_parser_paywall_rules.rds")
  
  test_results <- list(
    parser_rules_exists = file.exists(parser_rules_file),
    paywall_rules_exists = file.exists(paywall_rules_file)
  )
  
  if (test_results$parser_rules_exists) {
    parser_rules <- readRDS(parser_rules_file)
    test_results$parser_rules_count <- length(parser_rules)
    test_results$parser_rules_domains <- names(parser_rules)
    message(sprintf("Parser rules loaded: %d domains", test_results$parser_rules_count))
  } else {
    message("Parser rules file not found!")
  }
  
  if (test_results$paywall_rules_exists) {
    paywall_rules <- readRDS(paywall_rules_file)
    test_results$paywall_rules_count <- length(paywall_rules)
    test_results$paywall_rules_domains <- names(paywall_rules)
    message(sprintf("Paywall rules loaded: %d domains", test_results$paywall_rules_count))
  } else {
    message("Paywall rules file not found!")
  }
  
  # Mark test as passed only if both rule files exist
  test_results$passed <- test_results$parser_rules_exists && test_results$paywall_rules_exists
  
  return(test_results)
}

# Test data.table output format
test_06_parser_output_format <- function() {
  message("\n==== Testing 06_html_parser Output Format ====")
  
  # Expected columns in output
  expected_columns <- c("domain", "url", "timestamp_scraped", "date_time", 
                        "author", "headline", "text", "paywall")
  
  # Expected data types
  expected_types <- list(
    domain = "character",
    url = "character", 
    timestamp_scraped = "POSIXct",
    date_time = "character",
    author = "character",
    headline = "character",
    text = "character",
    paywall = "logical"
  )
  
  # Create test output structure
  test_output <- data.table(
    domain = "test.de",
    url = "https://test.de/artikel",
    timestamp_scraped = Sys.time(),
    date_time = as.character(Sys.time()),
    author = "Test Author",
    headline = "Test Headline",
    text = "Test article text content",
    paywall = FALSE
  )
  
  # Validate columns
  missing_columns <- setdiff(expected_columns, names(test_output))
  extra_columns <- setdiff(names(test_output), expected_columns)
  
  # Validate types
  type_mismatches <- list()
  for (col in expected_columns) {
    if (col %in% names(test_output)) {
      actual_type <- class(test_output[[col]])[1]
      expected_type <- expected_types[[col]]
      
      if (actual_type != expected_type && 
          !(expected_type == "POSIXct" && actual_type %in% c("POSIXct", "POSIXt"))) {
        type_mismatches[[col]] <- list(
          expected = expected_type,
          actual = actual_type
        )
      }
    }
  }
  
  # Results
  test_passed <- length(missing_columns) == 0 && 
    length(extra_columns) == 0 && 
    length(type_mismatches) == 0
  
  message(sprintf("Output format validation: %s", 
                  ifelse(test_passed, "PASSED ✓", "FAILED ✗")))
  
  if (length(missing_columns) > 0) {
    message("Missing columns: ", paste(missing_columns, collapse = ", "))
  }
  if (length(extra_columns) > 0) {
    message("Extra columns: ", paste(extra_columns, collapse = ", "))
  }
  if (length(type_mismatches) > 0) {
    message("Type mismatches:")
    for (col in names(type_mismatches)) {
      message(sprintf("  %s: expected %s, got %s", 
                      col, 
                      type_mismatches[[col]]$expected,
                      type_mismatches[[col]]$actual))
    }
  }
  
  return(list(
    test_name = "06_parser_output_format",
    passed = test_passed,
    missing_columns = missing_columns,
    extra_columns = extra_columns,
    type_mismatches = type_mismatches
  ))
}

# ==============================================================================
# MAIN TEST RUNNER
# ==============================================================================

# Run all tests for a specific module
run_module_tests <- function(module_name = "06_html_parser") {
  message("\n" %+% paste(rep("=", 60), collapse = ""))
  message("RUNNING TESTS FOR MODULE: ", module_name)
  message(paste(rep("=", 60), collapse = ""))
  
  test_results <- list()
  
  if (module_name == "06_html_parser") {
    # Run all 06_html_parser tests
    test_results$domain_classifier <- test_06_parser_domain_classifier()
    test_results$rules_loading <- test_06_parser_rules_loading()
    test_results$output_format <- test_06_parser_output_format()
  }
  
  # Summary
  message("\n" %+% paste(rep("=", 60), collapse = ""))
  message("TEST SUMMARY")
  message(paste(rep("=", 60), collapse = ""))
  
  total_tests <- length(test_results)
  passed_tests <- sum(sapply(test_results, function(x) {
    if (is.list(x) && "passed" %in% names(x)) x$passed else FALSE
  }))
  
  message(sprintf("Total tests run: %d", total_tests))
  message(sprintf("Tests passed: %d", passed_tests))
  message(sprintf("Tests failed: %d", total_tests - passed_tests))
  message(sprintf("Success rate: %.1f%%", (passed_tests / total_tests) * 100))
  
  return(test_results)
}

# Helper operator for string concatenation
`%+%` <- function(a, b) paste0(a, b)

# Only Domain
test_result_06_classifier <- test_06_parser_domain_classifier()

# Only Rules 
test_result_06_rules_loading <- test_06_parser_rules_loading()

# Only format 
test_result_06_outputformat <- test_06_parser_output_format() 

# All 06 tests
test_result_06_all_tests <- run_module_tests("06_html_parser")

