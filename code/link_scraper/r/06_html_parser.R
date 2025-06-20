# MODULE: HTML PARSING & DATA EXTRACTION
# 
# This module extracts structured article data from raw HTML responses using
# domain-specific parsing rules. It identifies and extracts key article 
# elements (title, text, author, date, etc.), applies domain-specific 
# extraction logic for different news sites, detects paywalls and other 
# content barriers, and validates the completeness of extracted data. The 
# parser ensures that only fully parsed articles with all required fields 
# are marked as successful, while incomplete parses are sent to retry.
#
# RECEIVES FROM:
# - 05_request_executor: HTML content and URL
# 
# OUTPUTS TO:
# - 10_storage_manager: Successfully parsed articles (data.table)
# - 08_retry_manager: Failed parses with HTML for reprocessing
#
# USAGE:
# 1. First run: Execute traffic-heavy setup functions once
# 2. Subsequent runs: Comment out setup calls, use initialize_html_parser()

# Load required packages
library(data.table)
library(httr)
library(rvest)
library(jsonlite)
library(lubridate)
library(stringr)

# 1: Configuration Function
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

# 2: Setup paywall domains
setup_paywall_domains <- function() {
  paths <- get_module_paths()
  
  # Load CSV and convert to RDS
  paywall_csv_path <- file.path(paths$input, "paywall_domains.csv")
  paywall_rds_path <- file.path(paths$config, "06_paywall_domains_processed.rds")
  
  if (file.exists(paywall_csv_path)) {
    # Read CSV with semicolon separator and header
    paywall_data <- fread(paywall_csv_path, 
                          sep = ";", 
                          encoding = "UTF-8",
                          header = TRUE,
                          blank.lines.skip = TRUE)
    
    # Remove any trailing empty column (from trailing semicolon)
    if (ncol(paywall_data) > 0 && names(paywall_data)[ncol(paywall_data)] == "V5") {
      paywall_data[, V5 := NULL]
    }
    
    # Ensure correct column names if they were not read properly
    expected_cols <- c("domain", "paywall_url", "free_url", "possible_markers")
    if (!all(expected_cols %in% names(paywall_data))) {
      # If headers weren't read correctly, set them manually
      if (ncol(paywall_data) >= 4) {
        setnames(paywall_data, 
                 old = names(paywall_data)[1:4], 
                 new = expected_cols)
      }
    }
    
    # Save processed data to config directory
    saveRDS(paywall_data, paywall_rds_path)
    message("Paywall domains processed and saved to: ", paywall_rds_path)
    
    return(paywall_data)
  } else {
    stop("Paywall domains CSV not found at: ", paywall_csv_path)
  }
}

# 3: Transform domain to GitHub filename
domain_to_paperboy_filename <- function(domain) {
  # Special cases
  if (domain == "augsburger-allgemeine.de") {
    return("deliver_augsburger_allgemeine.R")
  }
  if (domain == "n-tv.de") {
    return("deliver_n-tv_de.R")
  }
  
  # Default transformation
  filename <- gsub("\\.", "_", domain)
  filename <- gsub("-", "_", filename)
  return(paste0("deliver_", filename, ".R"))
}

# 4: Extract parsing rules from paperboy R script
extract_paperboy_rules <- function(script_content, domain) {
  # Check if script uses JSON parsing
  uses_json <- grepl("jsonlite::fromJSON", script_content, fixed = TRUE)
  
  # Extract datetime rule
  datetime_pattern <- "datetime <- ([^\\n]+(?:\\n\\s+[^\\n]+)*?)(?=\\n\\s*[a-zA-Z_]+ <-|\\n\\s*s_n_list)"
  datetime_match <- regmatches(script_content, regexec(datetime_pattern, script_content, perl = TRUE))
  datetime_rule <- ifelse(length(datetime_match[[1]]) > 1, 
                          trimws(datetime_match[[1]][2]), 
                          NA)
  
  # Extract headline rule
  headline_pattern <- "headline <- ([^\\n]+(?:\\n\\s+[^\\n]+)*?)(?=\\n\\s*[a-zA-Z_]+ <-|\\n\\s*s_n_list)"
  headline_match <- regmatches(script_content, regexec(headline_pattern, script_content, perl = TRUE))
  headline_rule <- ifelse(length(headline_match[[1]]) > 1, 
                          trimws(headline_match[[1]][2]), 
                          NA)
  
  # Extract author rule
  author_pattern <- "author <- ([^\\n]+(?:\\n\\s+[^\\n]+)*?)(?=\\n\\s*[a-zA-Z_]+ <-|\\n\\s*s_n_list)"
  author_match <- regmatches(script_content, regexec(author_pattern, script_content, perl = TRUE))
  author_rule <- ifelse(length(author_match[[1]]) > 1, 
                        trimws(author_match[[1]][2]), 
                        NA)
  
  # Extract text rule
  text_pattern <- "text <- ([^\\n]+(?:\\n\\s+[^\\n]+)*?)(?=\\n\\s*s_n_list)"
  text_match <- regmatches(script_content, regexec(text_pattern, script_content, perl = TRUE))
  text_rule <- ifelse(length(text_match[[1]]) > 1, 
                      trimws(text_match[[1]][2]), 
                      NA)
  
  # Return as list for new format
  return(list(
    domain = domain,
    uses_json = uses_json,
    rules = list(
      datetime = list(
        selector = datetime_rule,
        type = ifelse(uses_json, "json", "html")
      ),
      headline = list(
        selector = headline_rule,
        type = ifelse(uses_json, "json", "html")
      ),
      author = list(
        selector = author_rule,
        type = ifelse(uses_json, "json", "html")
      ),
      text = list(
        selector = text_rule,
        type = ifelse(uses_json, "json", "html")
      )
    )
  ))
}

# 5: Fetch and process all parser rules (TRAFFIC-HEAVY)
fetch_parser_rules <- function(domains) {
  paths <- get_module_paths()
  parser_rules <- list()
  
  processed_count <- 0
  failed_count <- 0
  
  for (domain in domains) {
    # Generate filename
    filename <- domain_to_paperboy_filename(domain)
    
    # Construct GitHub URL
    github_url <- paste0("https://raw.githubusercontent.com/JBGruber/paperboy/main/R/", filename)
    
    # Download script
    response <- httr::GET(github_url)
    
    if (httr::status_code(response) == 200) {
      # Extract content
      script_content <- httr::content(response, as = "text", encoding = "UTF-8")
      
      # Extract rules
      domain_rules <- extract_paperboy_rules(script_content, domain)
      
      # Store in list
      parser_rules[[domain]] <- domain_rules$rules
      parser_rules[[domain]]$uses_json <- domain_rules$uses_json
      
      processed_count <- processed_count + 1
      
    } else {
      message(sprintf("Failed to download parser for domain: %s (HTTP %d)", 
                      domain, httr::status_code(response)))
      failed_count <- failed_count + 1
    }
  }
  
  # Save parser rules to config directory
  parser_rules_path <- file.path(paths$config, "06_parser_rules_fetched.rds")
  saveRDS(parser_rules, parser_rules_path)
  
  message(sprintf("Parser rules extracted for %d domains, %d failed", 
                  processed_count, failed_count))
  message("Parser rules saved to: ", parser_rules_path)
  
  return(parser_rules)
}

# 6: Generate paywall detection rules
generate_paywall_rules <- function(paywall_data) {
  paths <- get_module_paths()
  
  # TO-DO: Implement full paywall detection logic
  # For now, create basic structure
  
  unique_domains <- unique(paywall_data$domain)
  paywall_rules <- list()
  
  # Basic paywall keywords
  paywall_keywords <- c("paywall", "premium", "plus", "abo", "subscriber", 
                        "subscription", "metered", "bezahlschranke", "bezahlartikel")
  
  for (domain in unique_domains) {
    domain_data <- paywall_data[domain == domain]
    
    # Check if domain has no paywall
    has_no_paywall <- any(grepl("no_paywall", domain_data$paywall_url, ignore.case = TRUE))
    
    if (has_no_paywall) {
      paywall_rules[[domain]] <- list(
        has_paywall = FALSE,
        paywall_markers = character(0)
      )
    } else {
      # TO-DO: Implement actual marker detection
      # For now, use basic keywords
      paywall_rules[[domain]] <- list(
        has_paywall = TRUE,
        paywall_markers = paste0("[class*='", paywall_keywords, "']")
      )
    }
  }
  
  # Save paywall rules to config directory
  paywall_rules_path <- file.path(paths$config, "06_paywall_rules_generated.rds")
  saveRDS(paywall_rules, paywall_rules_path)
  
  message("Paywall rules generated (basic implementation)")
  message("Paywall rules saved to: ", paywall_rules_path)
  
  return(paywall_rules)
}

# 7: Execute traffic-heavy setup functions once
run_initial_setup <- function() {
  message("Running initial setup - this will generate network traffic...")
  
  # Setup paywall domains
  paywall_data <- setup_paywall_domains()
  
  # Get unique domains
  unique_domains <- unique(paywall_data$domain)
  unique_domains <- unique_domains[!is.na(unique_domains) & unique_domains != ""]
  
  message(sprintf("Found %d unique domains", length(unique_domains)))
  
  # Fetch parser rules from paperboy (TRAFFIC-HEAVY)
  parser_rules <- fetch_parser_rules(unique_domains)
  
  # Generate paywall rules
  paywall_rules <- generate_paywall_rules(paywall_data)
  
  message("Initial setup completed successfully")
  
  return(list(
    parser_rules = parser_rules,
    paywall_rules = paywall_rules,
    paywall_data = paywall_data
  ))
}

# Comment out after first run 
# run_initial_setup()
rm(run_initial_setup, setup_paywall_domains, fetch_parser_rules, generate_paywall_rules, domain_to_paperboy_filename, extract_paperboy_rules)

# 8: Load rules from saved RDS files (no network traffic)
.load_parser_rules <- function() {
  paths <- get_module_paths()
  
  # Load from config directory where setup functions saved the data
  parser_rules_file <- file.path(paths$config, "06_parser_rules_fetched.rds")
  paywall_rules_file <- file.path(paths$config, "06_paywall_rules_generated.rds")
  
  if (!file.exists(parser_rules_file) || !file.exists(paywall_rules_file)) {
    stop("Parser rules not found. Run run_initial_setup() first to generate the rules.")
  }
  
  list(
    parser = readRDS(parser_rules_file),
    paywall = readRDS(paywall_rules_file)
  )
}

# 9: Extract domain from URL
.extract_domain <- function(url) {
  domain <- sub("^https?://(?:www\\.)?", "", url)
  domain <- sub("/.*$", "", domain)
  domain
}

# 10: Apply parser rule
.apply_parser_rule <- function(html, rule, json_df = NULL) {
  # Check if rule or selector is invalid
  if (is.null(rule) || is.na(rule$selector) || is.null(rule$selector)) {
    return(NA_character_)
  }
  
  # Try to evaluate the rule
  result <- tryCatch({
    if (rule$type == "json" && !is.null(json_df)) {
      # Evaluate JSON-based rule
      eval(parse(text = rule$selector))
    } else {
      # Evaluate HTML-based rule
      eval(parse(text = rule$selector))
    }
  }, error = function(e) {
    return(NA_character_)
  }, warning = function(w) {
    # Catch warnings and still try to return a result
    suppressWarnings({
      if (rule$type == "json" && !is.null(json_df)) {
        eval(parse(text = rule$selector))
      } else {
        eval(parse(text = rule$selector))
      }
    })
  })
  
  # Handle NULL results
  if (is.null(result)) {
    return(NA_character_)
  }
  
  # Handle zero-length results
  if (length(result) == 0) {
    return(NA_character_)
  }
  
  # Handle multiple results - take first one
  if (length(result) > 1) {
    result <- result[1]
  }
  
  # Convert to character
  result <- as.character(result)
  
  # Handle NA after conversion
  if (is.na(result)) {
    return(NA_character_)
  }
  
  # Check for empty strings using is.na-safe method
  if (!is.na(result) && nchar(result) == 0) {
    return(NA_character_)
  }
  
  return(result)
}

# Alternative simplified version that's even more robust
.apply_parser_rule_safe <- function(html, rule, json_df = NULL) {
  # Early return for invalid rules
  if (is.null(rule) || is.null(rule$selector) || is.na(rule$selector)) {
    return(NA_character_)
  }
  
  # Wrap entire evaluation in tryCatch
  result <- tryCatch({
    # Suppress warnings during evaluation
    suppressWarnings({
      if (rule$type == "json" && !is.null(json_df)) {
        eval(parse(text = rule$selector))
      } else {
        eval(parse(text = rule$selector))
      }
    })
  }, error = function(e) {
    NA_character_
  })
  
  # Convert result to character vector, handling all edge cases
  if (is.null(result) || length(result) == 0) {
    return(NA_character_)
  }
  
  # Take first element if multiple
  if (length(result) > 1) {
    result <- result[1]
  }
  
  # Safe conversion to character
  result <- tryCatch(
    as.character(result),
    error = function(e) NA_character_
  )
  
  # Final check for empty or NA
  if (is.na(result) || identical(result, "") || identical(result, character(0))) {
    return(NA_character_)
  }
  
  return(result)
}

# Use the safer version
.apply_parser_rule <- .apply_parser_rule_safe

# Also update the datetime parsing section to be more robust
func_06_parse_html <- function(html_content, url, rules = NULL) {
  # Load rules if not provided
  if (is.null(rules)) {
    rules <- .load_parser_rules()
  }
  
  # Extract domain
  domain <- .extract_domain(url)
  
  # Initialize output
  output <- data.table(
    domain = domain,
    url = url,
    timestamp_scraped = Sys.time(),
    date_time = NA_character_,
    author = NA_character_,
    headline = NA_character_,
    text = NA_character_,
    paywall = NA
  )
  
  # Parse HTML
  html <- tryCatch({
    read_html(html_content)
  }, error = function(e) {
    return(list(
      success = FALSE,
      data = output,
      html = html_content,
      error = paste("HTML parsing failed:", e$message)
    ))
  })
  
  # Check if html parsing returned an error
  if (is.list(html) && "success" %in% names(html)) {
    return(html)
  }
  
  # Get domain-specific rules
  parser_rules <- rules$parser[[domain]]
  paywall_rules <- rules$paywall[[domain]]
  
  if (is.null(parser_rules)) {
    return(list(
      success = FALSE,
      data = output,
      html = html_content,
      error = paste("No parser rules for domain:", domain)
    ))
  }
  
  # Handle JSON parsing if needed
  json_df <- NULL
  if (!is.null(parser_rules$uses_json) && parser_rules$uses_json) {
    json_txt <- tryCatch({
      html %>%
        html_elements("script[type = \"application/ld+json\"]") %>%
        html_text()
    }, error = function(e) character(0))
    
    if (length(json_txt) > 0 && nchar(json_txt[1]) > 0) {
      json_df <- tryCatch({
        fromJSON(json_txt[1])
      }, error = function(e) NULL)
    }
  }
  
  # Extract fields using the safe parser rule function
  output$date_time <- .apply_parser_rule(html, parser_rules$datetime, json_df)
  output$headline <- .apply_parser_rule(html, parser_rules$headline, json_df)
  output$author <- .apply_parser_rule(html, parser_rules$author, json_df)
  output$text <- .apply_parser_rule(html, parser_rules$text, json_df)
  
  # Safe datetime formatting
  if (!is.na(output$date_time)) {
    # Suppress timezone warnings
    formatted_date <- suppressWarnings(tryCatch({
      dt <- as_datetime(output$date_time)
      if (!is.na(dt)) {
        as.character(dt)
      } else {
        output$date_time
      }
    }, error = function(e) {
      output$date_time
    }))
    output$date_time <- formatted_date
  }
  
  # Check paywall
  if (!is.null(paywall_rules)) {
    if (isFALSE(paywall_rules$has_paywall)) {
      output$paywall <- FALSE
    } else {
      output$paywall <- tryCatch(
        .check_paywall(html, paywall_rules$paywall_markers),
        error = function(e) NA
      )
    }
  }
  
  # Validate output
  is_valid <- !is.na(output$headline) && 
    !is.na(output$text) && 
    !is.na(nchar(output$text)) &&
    nchar(output$text) > 50
  
  return(list(
    success = is_valid,
    data = output,
    html = ifelse(is_valid, NA_character_, html_content),
    error = ifelse(is_valid, NA_character_, "Validation failed - missing required fields")
  ))
}

# 11: Check for paywall markers
.check_paywall <- function(html, markers) {
  if (length(markers) == 0) {
    return(FALSE)
  }
  
  for (marker in markers) {
    nodes <- html_nodes(html, marker)
    if (length(nodes) > 0) {
      return(TRUE)
    }
  }
  
  return(FALSE)
}

# 12: Main HTML parsing function for pipeline integration
func_06_parse_html <- function(response_result, chunk_name = current_chunk) {
  # Validate input
  if (!is.list(response_result) || !response_result$success) {
    warning("Invalid response result provided to parser")
    return(invisible(FALSE))
  }
  
  # Extract HTML content and URL from response
  html_content <- tryCatch({
    httr2::resp_body_string(response_result$httr2_response)
  }, error = function(e) {
    warning("Failed to extract HTML content from response")
    return(invisible(FALSE))
  })
  
  # Get URL and other info from response
  url <- response_result$request_info$url
  input_info <- list(
    id = response_result$request_info$id,
    domain = response_result$request_info$domain,
    url = url
  )
  
  # Load parsing rules
  rules <- .load_parser_rules()
  
  # Extract domain from URL
  domain <- .extract_domain(url)
  
  # Parse HTML
  html <- tryCatch({
    read_html(html_content)
  }, error = function(e) {
    # If HTML parsing fails completely, log as error
    func_10_append_error(
      error_reason = paste("HTML parsing failed:", e$message),
      input_info = input_info,
      chunk_name = chunk_name
    )
    return(invisible(FALSE))
  })
  
  # Get domain-specific rules
  parser_rules <- rules$parser[[domain]]
  paywall_rules <- rules$paywall[[domain]]
  
  if (is.null(parser_rules)) {
    # No parser rules for this domain - log as error
    func_10_append_error(
      error_reason = paste("No parser rules for domain:", domain),
      input_info = input_info,
      chunk_name = chunk_name
    )
    return(invisible(FALSE))
  }
  
  # Handle JSON parsing if needed
  json_df <- NULL
  if (!is.null(parser_rules$uses_json) && parser_rules$uses_json) {
    json_txt <- tryCatch({
      html %>%
        html_elements("script[type = \"application/ld+json\"]") %>%
        html_text()
    }, error = function(e) character(0))
    
    if (length(json_txt) > 0 && nchar(json_txt[1]) > 0) {
      json_df <- tryCatch({
        fromJSON(json_txt[1])
      }, error = function(e) NULL)
    }
  }
  
  # Extract fields using parser rules
  date_time <- .apply_parser_rule(html, parser_rules$datetime, json_df)
  headline <- .apply_parser_rule(html, parser_rules$headline, json_df)
  author <- .apply_parser_rule(html, parser_rules$author, json_df)
  text <- .apply_parser_rule(html, parser_rules$text, json_df)
  
  # Format datetime if successfully extracted
  if (!is.na(date_time)) {
    formatted_date <- suppressWarnings(tryCatch({
      dt <- as_datetime(date_time)
      if (!is.na(dt)) {
        as.character(dt)
      } else {
        date_time
      }
    }, error = function(e) {
      date_time
    }))
    date_time <- formatted_date
  }
  
  # Check paywall status
  paywall <- NA
  if (!is.null(paywall_rules)) {
    if (isFALSE(paywall_rules$has_paywall)) {
      paywall <- FALSE
    } else {
      paywall <- tryCatch(
        .check_paywall(html, paywall_rules$paywall_markers),
        error = function(e) NA
      )
    }
  }
  
  # Create parse result object
  parse_result <- list(
    date_time = date_time,
    author = author,
    headline = headline,
    text = text,
    paywall = paywall
  )
  
  # Count how many fields are NA
  na_count <- sum(is.na(c(date_time, author, headline, text, paywall)))
  
  # Get response info for logging
  response_info <- list(
    server_date = tryCatch({
      date_header <- httr2::resp_header(response_result$httr2_response, "date")
      if (!is.null(date_header)) {
        httr2::parse_http_date(date_header)
      } else {
        Sys.time()
      }
    }, error = function(e) Sys.time())
  )
  
  # Decision logic based on NA values
  if (na_count == 5) {
    # All fields are NA - log as error
    func_10_append_error(
      error_reason = "All parsing fields returned NA",
      input_info = input_info,
      chunk_name = chunk_name
    )
  } else if (na_count == 0) {
    # No fields are NA - successful parse, append to output
    func_10_append_output(
      parse_result = parse_result,
      input_info = input_info,
      response_info = response_info,
      chunk_name = chunk_name
    )
  } else {
    # Some fields are NA, some are not - parse error
    func_10_append_parse_error(
      parse_result = parse_result,
      input_info = input_info,
      html_content = html_content,
      chunk_name = chunk_name
    )
  }
  
  # Function completed, ready for next response
  return(list(
    success = (na_count == 0),
    data    = parse_result,
    error   = if (na_count == 0) NULL else "missing_fields"
  ))
}


# 13: Batch processing function
func_06_parse_html_batch <- function(html_list, url_list, rules = NULL) {
  if (length(html_list) != length(url_list)) {
    stop("HTML and URL lists must have same length")
  }
  
  # Load rules once for batch
  if (is.null(rules)) {
    rules <- .load_parser_rules()
  }
  
  # Process all items
  results <- mapply(function(html, url) {
    func_06_parse_html(html, url, rules)
  }, html_list, url_list, SIMPLIFY = FALSE)
  
  # Separate successful and failed
  success_idx <- sapply(results, function(x) x$success)
  
  # Combine successful parses
  successful_data <- data.table()
  if (any(success_idx)) {
    successful_data <- rbindlist(lapply(results[success_idx], function(x) x$data))
  }
  
  # Collect failed parses
  failed_results <- results[!success_idx]
  
  return(list(
    successful = successful_data,
    failed = failed_results
  ))
}

# 14: Module initialization function
initialize_html_parser <- function() {
  message("Initializing HTML Parser Module (loading from saved data)...")
  
  # Check if required data files exist
  paths <- get_module_paths()
  required_files <- c(
    file.path(paths$config, "06_paywall_domains_processed.rds"),
    file.path(paths$config, "06_parser_rules_fetched.rds"),
    file.path(paths$config, "06_paywall_rules_generated.rds")
  )
  
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    stop("Missing required data files. Run run_initial_setup() first.\nMissing files:\n", 
         paste(missing_files, collapse = "\n"))
  }
  
  # Load rules (this is fast, no network traffic)
  rules <- .load_parser_rules()
  
  # Get some stats from loaded data
  parser_count <- length(rules$parser)
  paywall_count <- length(rules$paywall)
  
  message(sprintf("HTML Parser Module initialized successfully"))
  message(sprintf("Loaded %d parser rules and %d paywall rules", parser_count, paywall_count))
  
  return(list(
    parser_rules = rules$parser,
    paywall_rules = rules$paywall,
    stats = list(
      parser_count = parser_count,
      paywall_count = paywall_count
    )
  ))
}

# Execute initialization 
initialize_html_parser()
rm(initialize_html_parser)

# Check Parsing Rules: 
# parser_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parser_rules_fetched.rds") 