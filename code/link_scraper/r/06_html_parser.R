# ==============================================================================
# MODULE: HTML PARSING & DATA EXTRACTION
# ==============================================================================
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
# ==============================================================================

# Load required packages
library(data.table)
library(httr)
library(rvest)
library(jsonlite)
library(lubridate)
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


# 1. SETUP: Load and prepare paywall domain data

setup_paywall_domains <- function() {
  paths <- get_module_paths()
  
  # Load CSV and convert to RDS
  paywall_csv_path <- file.path(paths$input, "paywall_domains.csv")
  paywall_rds_path <- file.path(paths$config, "06_html_parser_paywall_domains.rds")
  
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
    
    # Save as RDS
    saveRDS(paywall_data, paywall_rds_path)
    message("Paywall domains saved to RDS: ", paywall_rds_path)
    
    return(paywall_data)
  } else {
    stop("Paywall domains CSV not found at: ", paywall_csv_path)
  }
}


# 2: Extract parser rules from paperboy repository

# Function to transform domain to GitHub filename
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

# Function to extract parsing rules from paperboy R script
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

# Main function to fetch and process all parser rules
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
  
  # Save parser rules
  saveRDS(parser_rules, file.path(paths$config, "06_html_parser_rules.rds"))
  
  message(sprintf("Parser rules extracted for %d domains, %d failed", 
                  processed_count, failed_count))
  
  return(parser_rules)
}


# 3: Paywall detection setup (TO-DO: Full implementation)

# Placeholder for paywall detection rule generation
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
  
  # Save paywall rules
  saveRDS(paywall_rules, file.path(paths$config, "06_html_parser_paywall_rules.rds"))
  
  message("Paywall rules generated (basic implementation)")
  
  return(paywall_rules)
}


# 4: Main parser functions

# Load rules (called once at module initialization)
.load_parser_rules <- function() {
  paths <- get_module_paths()
  
  parser_rules_file <- file.path(paths$config, "06_html_parser_rules.rds")
  paywall_rules_file <- file.path(paths$config, "06_html_parser_paywall_rules.rds")
  
  if (!file.exists(parser_rules_file) || !file.exists(paywall_rules_file)) {
    stop("Parser rules not found. Run setup functions first.")
  }
  
  list(
    parser = readRDS(parser_rules_file),
    paywall = readRDS(paywall_rules_file)
  )
}

# Extract domain from URL
.extract_domain <- function(url) {
  domain <- sub("^https?://(?:www\\.)?", "", url)
  domain <- sub("/.*$", "", domain)
  domain
}

# Apply parser rule
.apply_parser_rule <- function(html, rule, json_df = NULL) {
  if (is.na(rule$selector) || is.null(rule$selector)) {
    return(NA_character_)
  }
  
  tryCatch({
    if (rule$type == "json" && !is.null(json_df)) {
      # Evaluate JSON-based rule
      eval(parse(text = rule$selector))
    } else {
      # Evaluate HTML-based rule
      eval(parse(text = rule$selector))
    }
  }, error = function(e) {
    NA_character_
  })
}

# Check for paywall markers
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

# Main parser function
parse_html <- function(html_content, url, rules = NULL) {
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
    json_txt <- html %>%
      html_elements("script[type = \"application/ld+json\"]") %>%
      html_text()
    
    if (length(json_txt) > 0) {
      json_df <- tryCatch({
        fromJSON(json_txt[1])
      }, error = function(e) NULL)
    }
  }
  
  # Extract fields
  output$date_time <- .apply_parser_rule(html, parser_rules$datetime, json_df)
  output$headline <- .apply_parser_rule(html, parser_rules$headline, json_df)
  output$author <- .apply_parser_rule(html, parser_rules$author, json_df)
  output$text <- .apply_parser_rule(html, parser_rules$text, json_df)
  
  # Format datetime if extracted
  if (!is.na(output$date_time) && is.character(output$date_time)) {
    output$date_time <- tryCatch({
      as.character(as_datetime(output$date_time))
    }, error = function(e) output$date_time)
  }
  
  # Check paywall
  if (!is.null(paywall_rules)) {
    if (!paywall_rules$has_paywall) {
      output$paywall <- FALSE
    } else {
      output$paywall <- .check_paywall(html, paywall_rules$paywall_markers)
    }
  }
  
  # Validate output
  is_valid <- !is.na(output$headline) && 
    !is.na(output$text) && 
    nchar(output$text) > 50
  
  return(list(
    success = is_valid,
    data = output,
    html = ifelse(is_valid, NA_character_, html_content),
    error = ifelse(is_valid, NA_character_, "Validation failed - missing required fields")
  ))
}

# Batch processing function
parse_html_batch <- function(html_list, url_list, rules = NULL) {
  if (length(html_list) != length(url_list)) {
    stop("HTML and URL lists must have same length")
  }
  
  # Load rules once for batch
  if (is.null(rules)) {
    rules <- .load_parser_rules()
  }
  
  # Process all items
  results <- mapply(function(html, url) {
    parse_html(html, url, rules)
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


# 5. Module initialization 

# This function should be called once to initialize the module
initialize_html_parser <- function() {
  message("Initializing HTML Parser Module...")
  
  # Setup paywall domains
  paywall_data <- setup_paywall_domains()
  
  # Get unique domains
  unique_domains <- unique(paywall_data$domain)
  unique_domains <- unique_domains[!is.na(unique_domains) & unique_domains != ""]
  
  message(sprintf("Found %d unique domains", length(unique_domains)))
  
  # Fetch parser rules from paperboy
  parser_rules <- fetch_parser_rules(unique_domains)
  
  # Generate paywall rules
  paywall_rules <- generate_paywall_rules(paywall_data)
  
  message("HTML Parser Module initialized successfully")
  
  return(list(
    parser_rules = parser_rules,
    paywall_rules = paywall_rules
  ))
}

# Execute

initialize_html_parser()  
