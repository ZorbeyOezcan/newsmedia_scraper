# ==============================================================================
# MODULE: 06 - HTML PARSER & RULE MANAGEMENT
# ==============================================================================
#
# This module is responsible for setting up all parsing and paywall detection
# rules, and for applying these rules to raw HTML content.
#
# Rule Setup (run once):
# - setup_paywall_rules: Fetches sample pages and analyzes HTML/CSS/Script
#   differences to generate paywall markers.
# - setup_parser_rules: Clones the 'paperboy' repository and extracts
#   domain-specific parsing logic into executable R functions.
#
# Main Function (run per request):
# - func_06_parse_html: Receives a response object, applies the correct
#   paywall and parsing rules based on the domain, and routes the
#   structured result to the appropriate output (final_data, parse_error, error).
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr)
library(httr2)
library(rvest)
library(stringr)
library(jsonlite)
library(lubridate)

# 1. Main function to setup parser rules with complete code extraction
setup_parser_rules <- function() {
  paths <- get_module_paths()
  dir.create(paths$parsing_config, showWarnings = FALSE, recursive = TRUE)
  
  # Define paths
  paperboy_git_path <- file.path(paths$parsing_config, "paperboy_git")
  output_rds <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
  overview_rds <- file.path(paths$parsing_config, "06_parser_overview.rds")
  
  # Load domain list from CSV
  csv_path <- file.path(paths$input, "paywall_domains.csv")
  if (!file.exists(csv_path)) {
    stop("paywall_domains.csv not found at: ", csv_path)
  }
  
  message("Loading paywall domains CSV...")
  pw <- data.table::fread(csv_path, sep = ";", encoding = "UTF-8",
                          header = TRUE, blank.lines.skip = TRUE)
  data.table::setnames(pw, old = names(pw),
                       new = c("domain", "paywall_url", "free_url"))
  
  # Ensure every domain ends with ".de" for fetching
  pw[, domain_full := ifelse(grepl("\\.de$", domain, ignore.case = TRUE),
                             domain,
                             paste0(domain, ".de"))]
  
  # Create cleaned name (without TLD) for storage
  pw[, domain := sub("\\..*$", "", domain_full)]
  
  domains_full <- unique(pw$domain_full)
  domains_full <- domains_full[!is.na(domains_full) & nzchar(domains_full)]
  message(sprintf("Found %d unique domains to process", length(domains_full)))
  
  # Helper function to map domain to filename
  domain_to_file <- function(d) {
    switch(d,
           "augsburger-allgemeine.de" = "deliver_augsburger_allgemeine.R",
           "n-tv.de" = "deliver_n-tv_de.R",
           "faz.de" = "deliver_faz_net.R",
           sprintf("deliver_%s.R", gsub("[-\\.]", "_", d)))
  }
  
  # Step 1: Confirm Paperboy repository already exists
  message("\n=== Step 1: Using manually cloned Paperboy repository ===")
  if (!dir.exists(paperboy_git_path)) {
    stop("Expected paperboy_git directory does not exist: ", paperboy_git_path)
  }
  
  # Verify R directory exists
  r_dir <- file.path(paperboy_git_path, "R")
  if (!dir.exists(r_dir)) {
    stop("R directory not found in cloned repository. Please check the repository structure.")
  }
  
  # List available scripts for debugging
  available_scripts <- list.files(r_dir, pattern = "^deliver_.*\\.R$")
  message(sprintf("Found %d deliver scripts in repository", length(available_scripts)))
  
  # Step 2: Extract parsing rules from each script
  message("\n=== Step 2: Extracting parsing rules ===")
  
  # Initialize storage
  parser_rules <- list()
  overview_dt <- data.table(
    domain = character(),
    extraction_success = logical(),
    has_complex_logic = logical(),
    uses_json = logical(),
    error_message = character()
  )
  
  # The dynamically generated function will now return a proper POSIXct object
  # for datetime, instead of converting it to a character.
  extract_complete_parsing_logic <- function(script_content, domain_name) {
    tryCatch({
      # Find the function definition line
      func_start_idx <- grep("pb_deliver_paper\\.[^ ]+ <- function\\(", script_content)[1]
      if (is.na(func_start_idx)) {
        return(list(success = FALSE, error = "Could not find function definition (pb_deliver_paper...)"))
      }
      
      # Find the opening brace of the function body
      search_area <- script_content[func_start_idx:length(script_content)]
      open_brace_rel_idx <- grep("\\{", search_area)[1]
      if (is.na(open_brace_rel_idx)) {
        return(list(success = FALSE, error = "Could not find opening brace of function body"))
      }
      open_brace_idx <- func_start_idx + open_brace_rel_idx - 1
      
      # Find matching closing brace by counting
      close_brace_idx <- 0
      balance <- 0
      for (i in open_brace_idx:length(script_content)) {
        line <- script_content[i]
        balance <- balance + stringr::str_count(line, "\\{") - stringr::str_count(line, "\\}")
        if (balance == 0) {
          close_brace_idx <- i
          break
        }
      }
      
      if (close_brace_idx == 0) {
        return(list(success = FALSE, error = "Could not find closing brace of function body"))
      }
      
      # Extract function body
      body_lines <- script_content[(open_brace_idx + 1):(close_brace_idx - 1)]
      
      # Clean extracted code
      body_lines <- body_lines[!grepl("pb_tick\\(x, verbose, pb\\)", body_lines)]
      body_lines <- body_lines[!grepl("read_html\\s*\\(", body_lines)]
      
      # Analyze code for specific features
      uses_json <- any(grepl("jsonlite::fromJSON|fromJSON", body_lines))
      has_complex <- any(grepl("if\\s*\\(|else\\s*\\{|switch\\s*\\(", body_lines))
      
      # Build the wrapper function
      custom_s_n_list_def <- "
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
      datetime <<- datetime_val
      author <<- author_val
      headline <<- headline_val
      text <<- text_val
      invisible(NULL)
    }
    "
      
      wrapper_function_text <- sprintf(
        "function(html) {
        # Required libraries for execution
        library(rvest)
        library(lubridate)
        library(jsonlite)
        library(stringr)

        # Initialize variables, note datetime is now NA, not NA_character_
        datetime <- as.POSIXct(NA)
        author <- NA_character_
        headline <- NA_character_
        text <- NA_character_

        # Define helper function
        %s

        # Execute parsing logic within a tryCatch block
        tryCatch({
          %s
          list(
            datetime = datetime,
            author = as.character(author),
            headline = as.character(headline),
            text = as.character(text)
          )
        }, error = function(e) {
          list(
            datetime = as.POSIXct(NA),
            author = NA_character_,
            headline = NA_character_,
            text = NA_character_,
            error = paste('Parser execution error:', as.character(e$message))
          )
        })
      }",
        custom_s_n_list_def,
        paste(body_lines, collapse = "\n")
      )
      
      # Evaluate the wrapper text to a real function
      parsing_func <- eval(parse(text = wrapper_function_text))
      
      # Return the successful result
      return(list(
        success = TRUE,
        func = parsing_func,
        uses_json = uses_json,
        has_complex_logic = has_complex,
        raw_code = wrapper_function_text
      ))
      
    }, error = function(e) {
      # Catch unexpected errors during extraction
      return(list(
        success = FALSE,
        error = paste("Unexpected error in extract_complete_parsing_logic:", as.character(e$message))
      ))
    })
  }
  
  
  # Process each domain
  successful_count <- 0
  failed_count <- 0
  
  for (d_full in domains_full) {
    d_clean <- sub("\\..*$", "", d_full)
    message(sprintf("\nProcessing: %s", d_full))
    
    # Construct file path
    script_filename <- domain_to_file(d_full)
    script_file <- file.path(paperboy_git_path, "R", script_filename)
    
    if (!file.exists(script_file)) {
      message(sprintf("  ? Script file not found: %s", script_filename))
      
      # Try to find similar files
      r_dir <- file.path(paperboy_git_path, "R")
      similar_files <- list.files(r_dir, pattern = gsub("_de\\.R$", "", script_filename))
      if (length(similar_files) > 0) {
        message(sprintf("    Similar files found: %s", paste(similar_files, collapse = ", ")))
      }
      
      failed_count <- failed_count + 1
      
      # Add to overview
      overview_dt <- rbind(overview_dt, data.table(
        domain = d_clean,
        extraction_success = FALSE,
        has_complex_logic = NA,
        uses_json = NA,
        error_message = "Script file not found"
      ))
      
      # Store empty rule
      parser_rules[[d_clean]] <- list(
        success = FALSE,
        error = "Script file not found"
      )
      
      next
    }
    
    # Read script content
    script_content <- readLines(script_file, warn = FALSE)
    
    # Extract parsing logic
    extraction_result <- extract_complete_parsing_logic(script_content, d_clean)
    
    if (extraction_result$success) {
      successful_count <- successful_count + 1
      message(sprintf("  ? Successfully extracted (JSON: %s, Complex: %s)",
                      extraction_result$uses_json,
                      extraction_result$has_complex_logic))
      
      # Store the parsing function
      parser_rules[[d_clean]] <- list(
        success = TRUE,
        parse_function = extraction_result$func,
        uses_json = extraction_result$uses_json,
        has_complex_logic = extraction_result$has_complex_logic,
        raw_code = extraction_result$raw_code
      )
      
      # Add to overview
      overview_dt <- rbind(overview_dt, data.table(
        domain = d_clean,
        extraction_success = TRUE,
        has_complex_logic = extraction_result$has_complex_logic,
        uses_json = extraction_result$uses_json,
        error_message = NA_character_
      ))
      
    } else {
      failed_count <- failed_count + 1
      message(sprintf("  ? Extraction failed: %s", extraction_result$error))
      
      # Store error information
      parser_rules[[d_clean]] <- list(
        success = FALSE,
        error = extraction_result$error
      )
      
      # Add to overview
      overview_dt <- rbind(overview_dt, data.table(
        domain = d_clean,
        extraction_success = FALSE,
        has_complex_logic = NA,
        uses_json = NA,
        error_message = extraction_result$error
      ))
    }
  }
  
  # Step 3: Save results
  message("\n=== Step 3: Saving results ===")
  
  # Save parser rules
  saveRDS(parser_rules, output_rds)
  message(sprintf("Parser rules saved to: %s", output_rds))
  
  # Save overview
  saveRDS(overview_dt, overview_rds)
  message(sprintf("Overview saved to: %s", overview_rds))
  
  # Summary statistics
  message("\n=== Extraction Summary ===")
  message(sprintf("Total domains processed: %d", length(domains_full)))
  message(sprintf("Successfully extracted: %d (%.1f%%)",
                  successful_count,
                  successful_count / length(domains_full) * 100))
  message(sprintf("Failed extractions: %d", failed_count))
  
  if (nrow(overview_dt) > 0 && successful_count > 0) {
    json_count <- sum(overview_dt$uses_json, na.rm = TRUE)
    complex_count <- sum(overview_dt$has_complex_logic, na.rm = TRUE)
    
    message(sprintf("\nParsing characteristics:"))
    message(sprintf("  - Using JSON: %d domains (%.1f%%)",
                    json_count,
                    json_count / successful_count * 100))
    message(sprintf("  - Complex logic (if/else): %d domains (%.1f%%)",
                    complex_count,
                    complex_count / successful_count * 100))
  }
  
  # List failed domains for manual inspection
  failed_domains <- overview_dt[extraction_success == FALSE]$domain
  if (length(failed_domains) > 0) {
    message("\nFailed domains requiring manual attention:")
    for (dom in failed_domains) {
      error_msg <- overview_dt[domain == dom]$error_message
      message(sprintf("  - %s: %s", dom, error_msg))
    }
  }
  
  invisible(list(
    parser_rules = parser_rules,
    overview = overview_dt
  ))
}

# Run once 
# setup_parser_rules()
rm(setup_parser_rules)

# Load saved rules and overview
# parser_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_rules_fetched.rds")
# parser_overview <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_overview.rds")



#####



# 2. Function to setup paywall rules with integrated HTML fetching
setup_paywall_rules <- function() {
  paths <- get_module_paths()
  
  # Create parsing config directory if it doesn't exist
  dir.create(paths$parsing_config, showWarnings = FALSE, recursive = TRUE)
  
  # Define file paths
  paywall_csv_path <- file.path(paths$input, "paywall_domains.csv")
  html_storage_path <- file.path(paths$parsing_config, "06_paywall_html_storage.rds")
  paywall_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated.rds")
  paywall_overview_path <- file.path(paths$parsing_config, "06_paywall_overview.rds")
  
  if (!file.exists(paywall_csv_path)) {
    stop("Paywall domains CSV not found at: ", paywall_csv_path)
  }
  
  message("Loading paywall domains CSV...")
  
  # Read CSV with proper handling
  paywall_data <- fread(paywall_csv_path, 
                        sep = ";", 
                        encoding = "UTF-8",
                        header = TRUE,
                        blank.lines.skip = TRUE)
  
  # Clean column names
  setnames(paywall_data, 
           old = names(paywall_data), 
           new = c("domain", "paywall_url", "free_url"))
  
  # Clean domain names
  paywall_data[, domain := sub("\\..*$", "", domain)]
  
  # Get unique domains
  unique_domains <- unique(paywall_data$domain)
  message(sprintf("Found %d unique domains to process", length(unique_domains)))
  
  # Check if HTML storage already exists
  if (file.exists(html_storage_path)) {
    # Load existing HTML storage
    message("\nHTML storage already exists, loading from file")
    html_storage <- readRDS(html_storage_path)
    message(sprintf("Loaded HTML storage with %d entries from: %s", nrow(html_storage), html_storage_path))
    
  } else {
    # Fetch HTML content if storage doesn't exist
    message("\nFetching HTML content for paywall analysis")
    
    # Initialize session pool for fetching
    if (exists("func_04_initialize_session_pool")) {
      func_04_initialize_session_pool()
    } else {
      warning("func_04_initialize_session_pool not found. Using fallback HTTP method.")
    }
    
    # Prepare URLs for fetching
    paywalled_links <- paywall_data[!grepl("no_paywall", paywall_url, ignore.case = TRUE), .(domain, url = paywall_url)]
    paywalled_links[, label := "paywalled"]
    
    free_links <- paywall_data[!grepl("no_paywall", paywall_url, ignore.case = TRUE), .(domain, url = free_url)]
    free_links[, label := "free"]
    
    # Combine and clean URLs
    links_to_fetch <- rbindlist(list(paywalled_links, free_links), use.names = TRUE)
    links_to_fetch <- links_to_fetch[!is.na(url) & url != ""]
    links_to_fetch <- unique(links_to_fetch, by = "url")
    links_to_fetch[, id := .I]
    
    message(sprintf("Fetching %d unique URLs", nrow(links_to_fetch)))
    
    # Storage for fetched HTML
    html_results <- list()
    
    # Fetch each URL
    for (i in 1:nrow(links_to_fetch)) {
      link_info <- links_to_fetch[i]
      
      message(sprintf("[%d/%d] Fetching %s URL for domain: %s", 
                      i, nrow(links_to_fetch), link_info$label, link_info$domain))
      
      html_content <- NA_character_
      
      # Try pipeline method first
      if (exists("func_04_prepare_request") && exists("func_05_execute_request")) {
        tryCatch({
          request_package <- func_04_prepare_request(
            url = link_info$url,
            domain = paste0(link_info$domain, ".de"),
            aggressiveness_level = 1
          )
          
          if (request_package$success) {
            request_package$request_params$id <- link_info$id
            
            response_result <- func_05_execute_request(
              request_package = request_package,
              chunk_name = "chunk_999",
              worker_id = 1
            )
            
            if (response_result$success && !is.null(response_result$httr2_response)) {
              html_content <- resp_body_string(response_result$httr2_response)
            }
          }
        }, error = function(e) {
          # Silent fail, will try fallback
        })
      }
      
      # Fallback to direct HTTP if pipeline fails
      if (is.na(html_content)) {
        tryCatch({
          response <- GET(link_info$url, 
                          timeout(30),
                          user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
          
          if (status_code(response) == 200) {
            html_content <- content(response, "text", encoding = "UTF-8")
          }
        }, error = function(e) {
          message(sprintf("  Failed to fetch: %s", e$message))
        })
      }
      
      # Store successful fetches
      if (!is.na(html_content) && nchar(html_content) > 0) {
        html_results[[length(html_results) + 1]] <- data.table(
          domain = link_info$domain,
          url = link_info$url,
          label = link_info$label,
          html = html_content
        )
        message("  Success")
      } else {
        message("  Failed")
      }
      
      Sys.sleep(runif(1, 0.5, 1.5))
    }
    
    # Combine results
    if (length(html_results) > 0) {
      html_storage <- rbindlist(html_results, use.names = TRUE)
      saveRDS(html_storage, html_storage_path)
      message(sprintf("\nHTML storage saved with %d entries to: %s", 
                      nrow(html_storage), html_storage_path))
    } else {
      stop("No HTML content could be fetched. Aborting.")
    }
  }
  
  # Extract paywall rules from HTML storage
  message("\nAnalyzing HTML for paywall markers")
  
  # Define comprehensive paywall keywords
  paywall_keywords <- c(
    "paywall", "bezahlschranke", "bezahlartikel", "premium", "plus",
    "abo", "abonnement", "abonnent", "bezahlinhalt", "kostenpflichtig",
    "registrierung", "anmeldung", "zugang", "vollzugang", "bezahlpflicht",
    "artikellimit", "leserlimit", "gratis-artikel", "probe-abo", "testabo",
    "subscriber", "subscription", "metered", "paid-content", "member",
    "membership", "access", "full-access", "article-limit", "reader-limit",
    "trial", "register", "signin", "login", "unlock", "purchase",
    "newsletter", "konto", "kostenloses konto", "jetzt anmelden",
    "mit der anmeldung", "weiter zum artikel", "willkommen.*lesen",
    "passwort.*festlegen", "email.*eingeben", "exklusiver newsletter",
    "registrieren", "registrierung erforderlich", "digitales konto",
    "Jahresabo", "Probeabo", "monatlich kündbar", "az\\+?", "meine az\\+",
    "jetzt weiterlesen", "jetzt lesen", "probeabo", "jahresabo",
    "meinaz", "mein az", "azplus", "badge-meine_azplus",
    "artdetail_paywall", "funnelentry", "abo-start", "mein abo",
    "freischalten", "weiterlesen", "abo-wall", "angebot", "paywall-layer",
    "offerlink", "fp-paywall", "aboWall", "Beliebtestes Angebot", 
    "6 Wochen kostenlos testen", "Inklusive Zugriff auf die digitale Zeitung",
    "\\d+[,.]\\d+\\s*€.*(/\\s*(Monat|Woche))"
  )
  
  # Hardcoded rules for specific domains
  hardcoded_rules <- list(
    "noz" = list(
      has_paywall = TRUE,
      paywall_markers = list(
        css_selectors = c("span.paid-content"),
        meta_tags = c('meta[name="cXenseParse:qgw-access"][content~="Paid"]')
      )
    ),
    "bild" = list(
      has_paywall = TRUE,
      paywall_markers = list(
        css_selectors = c(
          "div.offer-module",
          "div.offer-module__ps"
        )
      )
    ),
    "shz" = list(
      has_paywall = FALSE,
      paywall_markers = list()
    ),
    "epochtimes" = list(
      has_paywall = as.logical(NA),
      paywall_markers = list()
    ),
    "wiwo" = list(
      has_paywall = as.logical(NA),
      paywall_markers = list()
    ),
    "freiepresse" = list(
      has_paywall = as.logical(NA),
      paywall_markers = list()
    ),
    "volksstimme" = list(
      has_paywall = TRUE,
      paywall_markers = list(),
      special_detection = "text_length"
    )
  )
  
  # Helper function to analyze HTML for markers
  analyze_html_for_markers <- function(raw_html) {
    markers <- list()
    
    # Parse HTML
    html_parsed <- tryCatch({
      read_html(raw_html)
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(html_parsed)) {
      return(markers)
    }
    
    # Extract CSS selectors with paywall keywords
    css_markers <- character()
    all_elements <- html_nodes(html_parsed, "*")
    
    for (element in all_elements) {
      element_tag <- html_name(element)
      all_attrs <- html_attrs(element)
      
      for (keyword in paywall_keywords) {
        matched <- FALSE
        element_selector <- NULL
        
        # Check attributes
        for (attr_name in names(all_attrs)) {
          attr_value <- all_attrs[[attr_name]]
          
          if (grepl(keyword, attr_name, ignore.case = TRUE) || 
              (!is.na(attr_value) && grepl(keyword, attr_value, ignore.case = TRUE))) {
            
            if (attr_name == "class") {
              classes <- unlist(strsplit(attr_value, " "))
              matching_class <- classes[grepl(keyword, classes, ignore.case = TRUE)][1]
              if (!is.na(matching_class)) {
                # Clean class name from special characters that could break CSS selectors
                clean_class <- gsub("[^a-zA-Z0-9_-]", "", matching_class)
                if (nchar(clean_class) > 0) {
                  element_selector <- paste0(element_tag, ".", clean_class)
                }
              }
            } else if (attr_name == "id") {
              # Clean ID from special characters
              clean_id <- gsub("[^a-zA-Z0-9_-]", "", attr_value)
              if (nchar(clean_id) > 0) {
                element_selector <- paste0(element_tag, "#", clean_id)
              }
            } else {
              # Skip attribute selectors with special characters that might cause issues
              if (!grepl("[:;\"']", attr_name) && !grepl("[:;\"']", keyword)) {
                element_selector <- sprintf('%s[%s*="%s"]', element_tag, attr_name, keyword)
              }
            }
            matched <- TRUE
            break
          }
        }
        
        if (matched && !is.null(element_selector)) {
          css_markers <- c(css_markers, element_selector)
          break
        }
      }
    }
    
    if (length(css_markers) > 0) {
      markers[["css_selectors"]] <- unique(css_markers)
    }
    
    # Check script blocks
    script_markers <- character()
    script_nodes <- html_nodes(html_parsed, "script")
    
    for (script in script_nodes) {
      script_content <- html_text(script)
      
      paywall_js_patterns <- c(
        "window\\.paywall\\s*=",
        "paywall\\s*:\\s*true",
        "isPaywalled\\s*:\\s*true"
      )
      
      for (pattern in paywall_js_patterns) {
        if (grepl(pattern, script_content, ignore.case = TRUE)) {
          script_markers <- c(script_markers, pattern)
        }
      }
    }
    
    if (length(script_markers) > 0) {
      markers[["script_blocks"]] <- unique(script_markers)
    }
    
    # Check meta tags
    meta_markers <- character()
    meta_nodes <- html_nodes(html_parsed, "meta")
    
    for (meta in meta_nodes) {
      meta_name <- html_attr(meta, "name")
      meta_property <- html_attr(meta, "property")
      meta_content <- html_attr(meta, "content")
      
      if (!is.na(meta_content) && 
          grepl("subscription|premium|paid|metered", meta_content, ignore.case = TRUE)) {
        if (!is.na(meta_name)) {
          meta_markers <- c(meta_markers, sprintf('meta[name="%s"]', meta_name))
        } else if (!is.na(meta_property)) {
          meta_markers <- c(meta_markers, sprintf('meta[property="%s"]', meta_property))
        }
      }
    }
    
    if (length(meta_markers) > 0) {
      markers[["meta_tags"]] <- unique(meta_markers)
    }
    
    return(markers)
  }
  
  # Initialize paywall rules storage
  paywall_rules <- list()
  
  # Initialize overview data.table
  overview_dt <- data.table(
    domain = character(),
    has_paywall = logical(),
    n_paywall_markers = integer(),
    paywall_markers = list(),
    successful_free_fetches = integer(),
    successful_paywall_fetches = integer()
  )
  
  # Identify no_paywall domains
  no_paywall_domains <- paywall_data[grepl("no_paywall", paywall_url, ignore.case = TRUE), unique(domain)]
  
  # Identify all_paywall domains
  all_paywall_domains <- paywall_data[paywall_url == "all_paywall", unique(domain)]
  
  # Process each domain
  for (domain_name in unique_domains) {
    message(sprintf("\nAnalyzing domain: %s", domain_name))
    
    # Check for no_paywall flag
    if (domain_name %in% no_paywall_domains) {
      message("  Domain has no paywalls")
      
      paywall_rules[[domain_name]] <- list(
        has_paywall = FALSE,
        paywall_markers = list()
      )
      
      # Count successful fetches
      domain_html <- html_storage[domain == domain_name]
      
      overview_dt <- rbind(overview_dt, data.table(
        domain = domain_name,
        has_paywall = FALSE,
        n_paywall_markers = 0L,
        paywall_markers = list(list()),
        successful_free_fetches = nrow(domain_html[label == "free"]),
        successful_paywall_fetches = nrow(domain_html[label == "paywalled"])
      ))
      
      next
    }
    
    # Check for all_paywall flag
    if (domain_name %in% all_paywall_domains) {
      message("  Domain is marked as 'all_paywall'")
      
      paywall_rules[[domain_name]] <- list(
        has_paywall = TRUE,
        is_all_paywall = TRUE,
        paywall_markers = list()
      )
      
      # Count successful fetches
      domain_html <- html_storage[domain == domain_name]
      
      overview_dt <- rbind(overview_dt, data.table(
        domain = domain_name,
        has_paywall = TRUE,
        n_paywall_markers = 0L,
        paywall_markers = list(list(note = "all_paywall_flag")),
        successful_free_fetches = nrow(domain_html[label == "free"]),
        successful_paywall_fetches = nrow(domain_html[label == "paywalled"])
      ))
      
      next
    }
    
    # Check for hardcoded rules first
    if (domain_name %in% names(hardcoded_rules)) {
      message("  Using predefined rules")
      
      paywall_rules[[domain_name]] <- hardcoded_rules[[domain_name]]
      
      # Count markers
      n_markers <- length(unlist(hardcoded_rules[[domain_name]]$paywall_markers))
      
      # Count successful fetches
      domain_html <- html_storage[domain == domain_name]
      
      overview_dt <- rbind(overview_dt, data.table(
        domain = domain_name,
        has_paywall = hardcoded_rules[[domain_name]]$has_paywall,
        n_paywall_markers = n_markers,
        paywall_markers = list(hardcoded_rules[[domain_name]]$paywall_markers),
        successful_free_fetches = nrow(domain_html[label == "free"]),
        successful_paywall_fetches = nrow(domain_html[label == "paywalled"])
      ))
      
      next
    }
    
    # Get HTML data for this domain
    domain_html_data <- html_storage[domain == domain_name]
    
    if (nrow(domain_html_data) == 0) {
      message("  No HTML data available for this domain")
      
      paywall_rules[[domain_name]] <- list(
        has_paywall = TRUE,
        paywall_markers = list()
      )
      
      overview_dt <- rbind(overview_dt, data.table(
        domain = domain_name,
        has_paywall = TRUE,
        n_paywall_markers = 0L,
        paywall_markers = list(list()),
        successful_free_fetches = 0L,
        successful_paywall_fetches = 0L
      ))
      
      next
    }
    
    # Separate paywall and free HTML data
    paywall_html_data <- domain_html_data[label == "paywalled"]
    free_html_data <- domain_html_data[label == "free"]
    
    # Analyze paywall HTMLs
    paywall_markers_list <- list()
    for (i in 1:nrow(paywall_html_data)) {
      markers <- analyze_html_for_markers(paywall_html_data$html[i])
      paywall_markers_list[[i]] <- markers
    }
    
    # Analyze free HTMLs
    free_markers_list <- list()
    for (i in 1:nrow(free_html_data)) {
      markers <- analyze_html_for_markers(free_html_data$html[i])
      free_markers_list[[i]] <- markers
    }
    
    # Find exclusive paywall markers
    exclusive_markers <- list()
    marker_types <- unique(unlist(lapply(paywall_markers_list, names)))
    
    for (type in marker_types) {
      paywall_type_markers <- unique(unlist(lapply(paywall_markers_list, function(m) m[[type]])))
      free_type_markers <- unique(unlist(lapply(free_markers_list, function(m) m[[type]])))
      exclusive_type_markers <- setdiff(paywall_type_markers, free_type_markers)
      
      if (length(exclusive_type_markers) > 0) {
        exclusive_markers[[type]] <- exclusive_type_markers
      }
    }
    
    # Store rules
    paywall_rules[[domain_name]] <- list(
      has_paywall = TRUE,
      paywall_markers = exclusive_markers
    )
    
    # Calculate total markers
    n_markers <- if (length(exclusive_markers) > 0) {
      sum(sapply(exclusive_markers, length))
    } else {
      0L
    }
    
    # Add to overview
    overview_dt <- rbind(overview_dt, data.table(
      domain = domain_name,
      has_paywall = TRUE,
      n_paywall_markers = n_markers,
      paywall_markers = list(exclusive_markers),
      successful_free_fetches = nrow(free_html_data),
      successful_paywall_fetches = nrow(paywall_html_data)
    ))
    
    # Print summary
    message(sprintf("  Has paywall: %s", TRUE))
    message(sprintf("  Total markers found: %d", n_markers))
    message(sprintf("  Successful fetches - Free: %d, Paywall: %d", 
                    nrow(free_html_data), nrow(paywall_html_data)))
  }
  
  # Save paywall rules
  saveRDS(paywall_rules, paywall_rules_path)
  
  # Save overview
  saveRDS(overview_dt, paywall_overview_path)
  
  message(sprintf("\nPaywall rules setup completed"))
  message(sprintf("Paywall rules saved to: %s", paywall_rules_path))
  message(sprintf("Overview saved to: %s", paywall_overview_path))
  
  # Print summary statistics
  message("\nSummary Statistics")
  message(sprintf("Total domains analyzed: %d", nrow(overview_dt)))
  message(sprintf("Domains with paywalls: %d", sum(overview_dt$has_paywall, na.rm = TRUE)))
  message(sprintf("Domains without paywalls: %d", sum(!overview_dt$has_paywall, na.rm = TRUE)))
  message(sprintf("Domains with NA paywall status: %d", sum(is.na(overview_dt$has_paywall))))
  message(sprintf("Total paywall markers found: %d", sum(overview_dt$n_paywall_markers)))
  
  return(invisible(TRUE))
}


# Execute the paywall rules setup function - only run once
# setup_paywall_rules()
rm(setup_paywall_rules)
# Load saved rules
# paywall_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_paywall_rules_generated.rds")
# paywall_overview <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_paywall_overview.rds")



#####



# 3. Main function to setup bot detection rules
setup_bot_detection_rules <- function() {
  
  # Get path to the parsing configuration directory
  # This assumes a helper function get_module_paths() exists, similar to other scripts.
  paths <- get_module_paths()
  dir.create(paths$parsing_config, showWarnings = FALSE, recursive = TRUE)
  
  message("\n=== Setting up Bot Detection Rules ===")
  
  # This list is structured into categories for clarity and targeted checks.
  # It is adjsutable for later fixing
  bot_detection_rules <- list(
    
    # --------------------------------------------------------------------------
    # 1. CSS Selectors
    # Checks for the existence of specific HTML elements (by ID, class, or attribute).
    # This is very reliable as these selectors are often unique to bot-check pages.
    # --------------------------------------------------------------------------
    css_selectors = c(
      # --- Cloudflare ---
      "div#cf-wrapper",                 # Main wrapper for Cloudflare's challenge pages.
      "div.cf-browser-verification",    # Common class for the verification box.
      "div#cf-challenge-running",       # ID for the element showing the running check.
      "form#challenge-form",            # The form used to submit the challenge result.
      
      # --- Google reCAPTCHA ---
      "div.g-recaptcha",                # Standard class for the reCAPTCHA widget container.
      "iframe[src*='google.com/recaptcha']", # Iframe loading the reCAPTCHA challenge.
      "div#rc-anchor-container",        # The "I'm not a robot" checkbox container.
      
      # --- hCaptcha ---
      "div.h-captcha",                  # Standard class for the hCaptcha widget.
      "iframe[src*='hcaptcha.com']",    # Iframe loading the hCaptcha challenge.
      
      # --- Imperva / Incapsula ---
      "iframe[src*='incapsula']",       # Iframe related to Incapsula security checks.
      
      # --- Generic Captcha Patterns ---
      "[id*='captcha']",                # Elements with "captcha" in their ID.
      "[class*='captcha']"              # Elements with "captcha" in their class name.
    ),
    
    # --------------------------------------------------------------------------
    # 2. Text Keywords & Phrases
    # Searches for specific strings within the HTML's <title> tag and visible text.
    # The phrases are chosen to be unambiguous.
    # --------------------------------------------------------------------------
    text_keywords = c(
      # --- Phrases in <title> tag ---
      "Just a moment...",
      "Attention Required!",
      "Checking your browser",
      
      # --- Phrases in the body text ---
      "DDoS protection by Cloudflare",
      "Verifying you are human",
      "I'm not a robot",
      "protected by reCAPTCHA",
      "I am human",
      "protected by hCaptcha",
      "Sicherheitsüberprüfung",
      "human verification",
      "enable JavaScript and cookies to continue",
      "enable cookies and javascript",
      "Sorry, you have been blocked"
    ),
    
    # --------------------------------------------------------------------------
    # 3. JavaScript Patterns
    # Searches within <script> tags (both src attribute and inline content) for
    # patterns that indicate the presence of a bot-detection script.
    # --------------------------------------------------------------------------
    js_patterns = c(
      # --- Script URLs (from 'src' attribute) ---
      "/cdn-cgi/challenge-platform/",  # Core URL for Cloudflare's Turnstile/Challenge.
      "recaptcha/api.js",              # Google reCAPTCHA's main API script.
      "hcaptcha.com/1/api.js",         # hCaptcha's main API script.
      "/_Incapsula_Resource",          # Imperva/Incapsula's script resource path.
      
      # --- Inline script content and variable names ---
      "window.cloudflare",             # Global object created by Cloudflare scripts.
      "window.grecaptcha",             # Global object for the Google reCAPTCHA API.
      "window.hcaptcha",               # Global object for the hCaptcha API.
      "__cf_challenge_opt",            # A common Cloudflare challenge options object.
      "turnstile.execute"              # Function call for Cloudflare Turnstile.
    )
  )
  
  # Define the output path for the RDS file
  output_rds_path <- file.path(paths$parsing_config, "06_bot_detection_rules.rds")
  
  # Save the rules list to an RDS file
  saveRDS(bot_detection_rules, file = output_rds_path)
  
  # Provide feedback to the user
  message("Successfully defined and structured bot detection rules.")
  n_css <- length(bot_detection_rules$css_selectors)
  n_txt <- length(bot_detection_rules$text_keywords)
  n_js <- length(bot_detection_rules$js_patterns)
  message(sprintf(" -> Defined %d CSS selectors, %d text keywords, and %d JavaScript patterns.", n_css, n_txt, n_js))
  message(sprintf("Bot detection rules saved to: %s", output_rds_path))
  
  invisible(NULL) # Return nothing to the global environment
}

# Execute the bot detection rules setup function - only run once
# setup_bot_detection_rules()

# Clean up the function from the environment after running, if desired
rm(setup_bot_detection_rules)

# load rds. if needed
# bot_detection_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_bot_detection_rules.rds")


#####



# 4. Main callable parsing function 
func_06_parse_html <- local({
  
  # --- Private Caching Environment ---
  .parser_rules_cache <- NULL
  .paywall_rules_cache <- NULL
  .bot_rules_cache <- NULL 
  
  # --- Internal helper function to load rules if needed ---
  .load_cached_rules <- function() {
    paths <- get_module_paths() 
    
    if (is.null(.parser_rules_cache)) {
      parser_rules_path <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
      if (!file.exists(parser_rules_path)) stop("FATAL: Parser rule file not found: ", parser_rules_path)
      .parser_rules_cache <<- readRDS(parser_rules_path)
    }
    
    if (is.null(.paywall_rules_cache)) {
      paywall_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated.rds")
      if (!file.exists(paywall_rules_path)) stop("FATAL: Paywall rule file not found: ", paywall_rules_path)
      .paywall_rules_cache <<- readRDS(paywall_rules_path)
    }
    
    if (is.null(.bot_rules_cache)) {
      bot_rules_path <- file.path(paths$parsing_config, "06_bot_detection_rules.rds")
      if (!file.exists(bot_rules_path)) stop("FATAL: Bot detection rule file not found: ", bot_rules_path)
      .bot_rules_cache <<- readRDS(bot_rules_path)
    }
  }
  
  # --- Internal helper functions ---
  .check_paywall_markers <- function(html_parsed, paywall_markers) {
    if (length(paywall_markers) == 0) return(FALSE)
    for (type in names(paywall_markers)) {
      for (pattern in paywall_markers[[type]]) {
        tryCatch({
          if (type == "css_selectors") {
            escaped_pattern <- gsub(":", "\\\\:", pattern)
            if (length(html_nodes(html_parsed, escaped_pattern)) > 0) return(TRUE)
          } else if (type == "meta_tags") {
            if (length(html_nodes(html_parsed, pattern)) > 0) return(TRUE)
          }
        }, error = function(e) {
          message(sprintf("Warning: Could not parse selector '%s': %s", pattern, e$message))
          return(FALSE)
        })
      }
    }
    return(FALSE)
  }
  
  .check_bot_detection <- function(html_parsed, bot_rules) {
    for (selector in bot_rules$css_selectors) {
      if (length(rvest::html_nodes(html_parsed, selector)) > 0) return(TRUE)
    }
    title_text <- rvest::html_text(rvest::html_node(html_parsed, "title"))
    body_text <- rvest::html_text(rvest::html_node(html_parsed, "body"))
    combined_text <- paste(title_text, body_text)
    for (keyword in bot_rules$text_keywords) {
      if (stringr::str_detect(combined_text, stringr::fixed(keyword, ignore_case = TRUE))) return(TRUE)
    }
    all_scripts <- rvest::html_nodes(html_parsed, "script")
    all_script_content <- paste(rvest::html_attr(all_scripts, "src"), 
                                rvest::html_text(all_scripts), 
                                collapse = " ")
    for (pattern in bot_rules$js_patterns) {
      if (stringr::str_detect(all_script_content, stringr::fixed(pattern))) return(TRUE)
    }
    return(FALSE)
  }
  
  .sanitize_value <- function(value) {
    if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA_character_)
    # Always take the first element and collapse multiple values into one string
    value <- value[1] 
    return(as.character(value))
  }
  
  .check_volksstimme_paywall <- function(text_content) {
    if (is.na(text_content) || is.null(text_content)) return(FALSE)
    clean_text <- trimws(as.character(text_content))
    text_length <- nchar(clean_text)
    return(text_length >= 300 && text_length <= 500)
  }
  
  # --- The main function ---
  function(response_result, chunk_name) {
    
    .load_cached_rules()
    
    request_info <- response_result$request_info
    
    html_content <- tryCatch(httr2::resp_body_string(response_result$httr2_response), error = function(e) NULL)
    
    # Robust check for NULL, NA, or empty content
    if (is.null(html_content) || is.na(html_content) || nchar(html_content) == 0) {
      minimal_dt <- data.table(id = request_info$id, domain = request_info$domain, url = request_info$url)
      func_10_append_error("empty_html_content", minimal_dt, chunk_name)
      return(list(success = FALSE, data = NULL, reason = "empty_html_content"))
    }
    
    temp_dt <- data.table(
      id                = as.integer(request_info$id),
      domain            = as.character(request_info$domain),
      url               = as.character(request_info$url),
      timestamp_scraped = as.POSIXct(request_info$request_timestamp),
      date_time         = as.POSIXct(NA),
      author            = NA_character_,
      headline          = NA_character_,
      text              = NA_character_,
      paywall           = as.logical(NA),
      bot_detect        = as.logical(NA)
    )
    
    html_parsed <- tryCatch(rvest::read_html(html_content), error = function(e) NULL)
    
    if (is.null(html_parsed)) {
      minimal_dt <- data.table(id = request_info$id, domain = request_info$domain, url = request_info$url)
      func_10_append_error("html_parse_failed", minimal_dt, chunk_name)
      return(list(success = FALSE, data = NULL, reason = "html_parse_failed"))
    }
    
    temp_dt$bot_detect <- .check_bot_detection(html_parsed, .bot_rules_cache)
    if (isTRUE(temp_dt$bot_detect)) {
      func_10_append_retry("bot_detected", request_info, chunk_name) 
      return(list(success = FALSE, data = temp_dt, reason = "bot_detected"))
    }
    
    domain_clean <- sub("\\..*$", "", temp_dt$domain)
    parser_entry <- .parser_rules_cache[[domain_clean]]
    if (!is.null(parser_entry) && isTRUE(parser_entry$success)) {
      parsing_func <- parser_entry$parse_function
      extracted_data <- tryCatch(parsing_func(html_parsed), error = function(e) list(error = e$message))
      
      if (is.null(extracted_data$error)) {
        # Always take the first element [1] to prevent assignment errors
        if (!is.null(extracted_data$datetime) && length(extracted_data$datetime) > 0 && !all(is.na(extracted_data$datetime))) {
          temp_dt$date_time <- extracted_data$datetime[1]
        }
        temp_dt$author    <- .sanitize_value(extracted_data$author)
        temp_dt$headline  <- .sanitize_value(extracted_data$headline)
        temp_dt$text      <- .sanitize_value(extracted_data$text)
      }
    }
    
    paywall_entry <- .paywall_rules_cache[[domain_clean]]
    if (!is.null(paywall_entry)) {
      if (!is.null(paywall_entry$special_detection) && paywall_entry$special_detection == "text_length") {
        temp_dt$paywall <- .check_volksstimme_paywall(temp_dt$text)
      } else if (!is.na(paywall_entry$has_paywall)) {
        if (isTRUE(paywall_entry$has_paywall)) {
          temp_dt$paywall <- .check_paywall_markers(html_parsed, paywall_entry$paywall_markers)
        } else {
          temp_dt$paywall <- FALSE
        }
      }
    }
    
    # This ensures no empty strings ("") are passed, only proper NAs.
    temp_dt[is.na(date_time), date_time := as.POSIXct(NA)]
    temp_dt[is.na(author) | nchar(trimws(author)) == 0, author := NA_character_]
    temp_dt[is.na(headline) | nchar(trimws(headline)) == 0, headline := NA_character_]
    temp_dt[is.na(text) | nchar(trimws(text)) == 0, text := NA_character_]
    temp_dt[is.na(paywall), paywall := as.logical(NA)]
    temp_dt[is.na(bot_detect), bot_detect := as.logical(NA)]
    
    is_na_date_time <- all(is.na(temp_dt$date_time))
    is_na_text <- all(is.na(temp_dt$text))
    
    # Treat NA paywall as TRUE for routing decisions to be safe
    paywall_for_routing <- ifelse(is.na(temp_dt$paywall), TRUE, temp_dt$paywall)
    
    if (!is_na_date_time && !is_na_text) {
      func_10_append_output(temp_dt, chunk_name)
      return(list(success = TRUE, data = temp_dt))
    } else if (isTRUE(paywall_for_routing)) { 
      func_10_append_error("paywalled_content_or_missing_fields", temp_dt, chunk_name)
      return(list(success = FALSE, data = temp_dt, reason = "paywalled_content_or_missing_fields"))
    } else {
      func_10_append_parse_error(temp_dt, html_content, chunk_name)
      return(list(success = FALSE, data = temp_dt, reason = "parsing_failed_no_paywall"))
    }
  }
})
