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


# 1. Main function to setup paywall rules
setup_paywall_rules <- function() {
  paths <- get_module_paths()
  
  # Create parsing config directory if it doesn't exist
  dir.create(paths$parsing_config, showWarnings = FALSE, recursive = TRUE)
  
  # Load paywall domains CSV
  paywall_csv_path <- file.path(paths$input, "paywall_domains.csv")
  
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
  
  paywall_data[, domain := sub("\\..*$", "", domain)]
  
  # Get unique domains
  unique_domains <- unique(paywall_data$domain)
  message(sprintf("Found %d unique domains to process", length(unique_domains)))
  
  # Check if HTML storage already exists
  html_storage_path <- file.path(paths$parsing_config, "06_paywall_html_storage.rds")
  
  if (file.exists(html_storage_path)) {
    # Load existing HTML storage
    message("\n=== HTML storage already exists, loading from file ===")
    html_storage <- readRDS(html_storage_path)
    message(sprintf("Loaded HTML storage with %d entries from: %s", nrow(html_storage), html_storage_path))
    
  } else {
    # Define user agent strings for retry attempts
    user_agents <- c(
      "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
      "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15"
    )
    
    # STEP 1: FETCH ALL HTML CONTENT WITH RETRY LOGIC
    message("\n Fetching HTML content with retry logic")
    
    # Initialize data.table to store all HTML content
    html_storage <- data.table(
      domain = character(),
      url = character(),
      url_type = character(), # "paywall" or "free"
      raw_html = character(),
      status_code = integer(),
      fetch_success = logical(),
      retry_attempted = logical()
    )
    
    # Helper function to fetch URL with retry logic
    fetch_url_with_retry <- function(url, url_type, domain_name, user_agents) {
      # First attempt with primary user agent
      response <- tryCatch({
        GET(url,
            timeout(30),
            user_agent(user_agents[1]))
      }, error = function(e) {
        message(sprintf("ERROR: Network request failed - %s", e$message))
        return(NULL)
      })
      
      # Process first attempt
      if (!is.null(response)) {
        status <- status_code(response)
        
        if (status == 200) {
          raw_html <- tryCatch({
            as.character(content(response, "text", encoding = "UTF-8"))
          }, error = function(e) {
            NA_character_
          })
          
          if (!is.na(raw_html) && nchar(raw_html) > 0) {
            message("FETCH_SUCCESS")
            return(data.table(
              domain = domain_name,
              url = url,
              url_type = url_type,
              raw_html = raw_html,
              status_code = status,
              fetch_success = TRUE,
              retry_attempted = FALSE
            ))
          }
        }
      }
      
      # If first attempt failed, try with alternative user agent
      message("First attempt failed, trying with alternative user agent...")
      Sys.sleep(2) # Longer delay before retry
      
      response <- tryCatch({
        GET(url,
            timeout(30),
            user_agent(user_agents[2]))
      }, error = function(e) {
        message(sprintf("ERROR: Retry request failed - %s", e$message))
        return(NULL)
      })
      
      if (!is.null(response)) {
        status <- status_code(response)
        
        if (status == 200) {
          raw_html <- tryCatch({
            as.character(content(response, "text", encoding = "UTF-8"))
          }, error = function(e) {
            NA_character_
          })
          
          if (!is.na(raw_html) && nchar(raw_html) > 0) {
            message("FETCH_SUCCESS (on retry)")
            return(data.table(
              domain = domain_name,
              url = url,
              url_type = url_type,
              raw_html = raw_html,
              status_code = status,
              fetch_success = TRUE,
              retry_attempted = TRUE
            ))
          }
        }
      }
      
      # Both attempts failed
      message("ERROR: Both fetch attempts failed")
      return(data.table(
        domain = domain_name,
        url = url,
        url_type = url_type,
        raw_html = NA_character_,
        status_code = ifelse(!is.null(response), status_code(response), NA_integer_),
        fetch_success = FALSE,
        retry_attempted = TRUE
      ))
    }
    
    # Process each domain
    for (domain_name in unique_domains) {
      message(sprintf("\nProcessing domain: %s", domain_name))
      
      # Get all URLs for this domain
      domain_data <- paywall_data[domain == domain_name]
      
      # Check for no_paywall flag
      if (any(grepl("no_paywall", domain_data$paywall_url, ignore.case = TRUE))) {
        message(sprintf("Domain has no paywalls: %s", domain_name))
        next
      }
      
      # Separate paywall and free URLs
      paywall_urls <- domain_data$paywall_url[domain_data$paywall_url != "" & !is.na(domain_data$paywall_url)]
      free_urls <- domain_data$free_url[domain_data$free_url != "" & !is.na(domain_data$free_url)]
      
      # Fetch paywall URLs
      for (i in seq_along(paywall_urls)) {
        url <- paywall_urls[i]
        message(sprintf("Fetching paywall link %d of %d", i, length(paywall_urls)))
        
        result <- fetch_url_with_retry(url, "paywall", domain_name, user_agents)
        html_storage <- rbind(html_storage, result)
        
        Sys.sleep(1) # Delay between requests
      }
      
      # Fetch free URLs
      for (i in seq_along(free_urls)) {
        url <- free_urls[i]
        message(sprintf("Fetching free link %d of %d", i, length(free_urls)))
        
        result <- fetch_url_with_retry(url, "free", domain_name, user_agents)
        html_storage <- rbind(html_storage, result)
        
        Sys.sleep(1) # Delay between requests
      }
    }
    
    # Save HTML storage for future use
    saveRDS(html_storage, html_storage_path)
    message(sprintf("\nHTML storage saved to: %s", html_storage_path))
  }
  
  # STEP 2: ENHANCED HTML ANALYSIS INCLUDING SCRIPTS AND META TAGS
  message("\n HTML analysis for paywall markers")
  
  # Define comprehensive paywall keywords
  paywall_keywords <- c(
    # add to:
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
    "offerlink", "fp-paywall", "aboWall", "Beliebtestes Angebot", "6 Wochen kostenlos testen",
    "Inklusive Zugriff auf die digitale Zeitung",
    
    # Pricing
    "\\d+[,.]\\d+\\s*€.*(/\\s*(Monat|Woche))"
  )
  
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
  
  # Helper function to analyze HTML for paywall markers
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
    
    # 1. Enhanced CSS and attribute selectors check
    css_markers <- character()
    
    # Get all elements
    all_elements <- html_nodes(html_parsed, "*")
    
    for (element in all_elements) {
      element_tag <- html_name(element)
      
      # Get ALL attributes of the element
      all_attrs <- html_attrs(element)
      
      for (keyword in paywall_keywords) {
        matched <- FALSE
        element_selector <- NULL
        
        # Check all attribute names and values
        for (attr_name in names(all_attrs)) {
          attr_value <- all_attrs[[attr_name]]
          
          # Check if keyword appears in attribute NAME
          if (grepl(keyword, attr_name, ignore.case = TRUE)) {
            # Create selector with attribute name
            element_selector <- sprintf('%s[%s]', element_tag, attr_name)
            matched <- TRUE
            break
          }
          
          # Check if keyword appears in attribute VALUE
          if (!is.na(attr_value) && grepl(keyword, attr_value, ignore.case = TRUE)) {
            # Special handling for common attributes
            if (attr_name == "class") {
              classes <- unlist(strsplit(attr_value, " "))
              matching_class <- classes[grepl(keyword, classes, ignore.case = TRUE)][1]
              if (!is.na(matching_class)) {
                element_selector <- paste0(element_tag, ".", matching_class)
              }
            } else if (attr_name == "id") {
              element_selector <- paste0(element_tag, "#", attr_value)
            } else {
              # For other attributes, create attribute selector
              element_selector <- sprintf('%s[%s*="%s"]', element_tag, attr_name, keyword)
            }
            matched <- TRUE
            break
          }
        }
        
        # Also check text content with increased limit
        if (!matched) {
          element_text <- html_text(element, trim = TRUE)
          if (!is.na(element_text) && nchar(element_text) > 0 && nchar(element_text) < 500) {
            # Normalize text: remove extra whitespace and special characters
            normalized_text <- gsub("\\s+", " ", element_text)
            normalized_text <- gsub("[^[:alnum:][:space:].-]", "", normalized_text)
            
            if (grepl(keyword, normalized_text, ignore.case = TRUE)) {
              # Try to create a more specific selector
              if ("class" %in% names(all_attrs)) {
                first_class <- unlist(strsplit(all_attrs[["class"]], " "))[1]
                element_selector <- paste0(element_tag, ".", first_class)
              } else if ("id" %in% names(all_attrs)) {
                element_selector <- paste0(element_tag, "#", all_attrs[["id"]])
              } else {
                element_selector <- element_tag
              }
              matched <- TRUE
            }
          }
        }
        
        if (matched && !is.null(element_selector)) {
          css_markers <- c(css_markers, element_selector)
          break
        }
      }
      
      # Additional check for specific data attributes that might indicate paywalls
      paywall_attr_patterns <- c(
        "data-.*paywall", "data-.*premium", "data-.*subscription",
        "data-fp-flag", "data-access", "data-metered",
        "ng-if.*paywall", "ng-show.*subscription",
        "aria-label.*paywall", "aria-label.*premium",
        "external-event.*paywall", "external-event.*subscription"
      )
      
      for (pattern in paywall_attr_patterns) {
        for (attr_name in names(all_attrs)) {
          if (grepl(pattern, paste(attr_name, all_attrs[[attr_name]]), ignore.case = TRUE)) {
            element_selector <- sprintf('%s[%s]', element_tag, attr_name)
            css_markers <- c(css_markers, element_selector)
            break
          }
        }
      }
    }
    
    # Check all text nodes including nested ones
    text_nodes <- html_nodes(html_parsed, xpath = "//text()[normalize-space() != '']")
    text_markers <- character()
    
    for (text_node in text_nodes) {
      text_content <- html_text(text_node, trim = TRUE)
      if (!is.na(text_content) && nchar(text_content) < 500) {
        normalized_text <- gsub("\\s+", " ", text_content)
        
        for (keyword in paywall_keywords) {
          if (grepl(keyword, normalized_text, ignore.case = TRUE)) {
            # Get parent element for selector
            parent_xpath <- "parent::*"
            parent_node <- tryCatch({
              html_nodes(text_node, xpath = parent_xpath)[1]
            }, error = function(e) NULL)
            
            if (!is.null(parent_node)) {
              parent_tag <- html_name(parent_node)
              parent_attrs <- html_attrs(parent_node)
              
              if ("id" %in% names(parent_attrs)) {
                text_markers <- c(text_markers, paste0(parent_tag, "#", parent_attrs[["id"]]))
              } else if ("class" %in% names(parent_attrs)) {
                first_class <- unlist(strsplit(parent_attrs[["class"]], " "))[1]
                text_markers <- c(text_markers, paste0(parent_tag, ".", first_class))
              } else {
                text_markers <- c(text_markers, parent_tag)
              }
            }
            break
          }
        }
      }
    }
    
    # Combine CSS and text markers
    all_css_markers <- unique(c(css_markers, text_markers))
    if (length(all_css_markers) > 0) {
      markers[["css_selectors"]] <- all_css_markers
    }
    
    # 2. Check script blocks for paywall-related JavaScript
    script_markers <- character()
    script_nodes <- html_nodes(html_parsed, "script")
    
    for (script in script_nodes) {
      script_content <- html_text(script)
      
      # Check for common paywall JavaScript patterns
      paywall_js_patterns <- c(
        "window\\.paywall\\s*=",
        "window\\.__data__.*paywall",
        "window\\..*subscription",
        "paywall\\s*:\\s*true",
        "isPaywalled\\s*:\\s*true",
        "articleAccess.*subscription",
        "metered.*limit",
        "freeArticles.*remaining"
      )
      
      for (pattern in paywall_js_patterns) {
        if (grepl(pattern, script_content, ignore.case = TRUE)) {
          # Extract a snippet of the matching code
          match_pos <- regexpr(pattern, script_content, ignore.case = TRUE)
          if (match_pos > 0) {
            start <- max(1, match_pos - 20)
            end <- min(nchar(script_content), match_pos + attr(match_pos, "match.length") + 50)
            snippet <- substr(script_content, start, end)
            snippet <- gsub("\\s+", " ", snippet) # Normalize whitespace
            script_markers <- c(script_markers, snippet)
          }
        }
      }
    }
    
    # Check for JSON-LD scripts
    json_ld_nodes <- html_nodes(html_parsed, 'script[type="application/ld+json"]')
    for (json_node in json_ld_nodes) {
      json_content <- html_text(json_node)
      if (any(sapply(paywall_keywords, function(kw) grepl(kw, json_content, ignore.case = TRUE)))) {
        script_markers <- c(script_markers, "JSON-LD with paywall keywords")
      }
    }
    
    if (length(script_markers) > 0) {
      markers[["script_blocks"]] <- unique(script_markers)
    }
    
    # 3. Check meta tags
    meta_markers <- character()
    meta_nodes <- html_nodes(html_parsed, "meta")
    
    for (meta in meta_nodes) {
      meta_name <- html_attr(meta, "name")
      meta_property <- html_attr(meta, "property")
      meta_content <- html_attr(meta, "content")
      
      # Check for paywall-related meta tags
      if (!is.na(meta_name)) {
        if (grepl("access|subscription|paywall|premium", meta_name, ignore.case = TRUE)) {
          meta_markers <- c(meta_markers, sprintf('meta[name="%s"]', meta_name))
        }
      }
      
      if (!is.na(meta_property)) {
        if (grepl("access|subscription|paywall|premium", meta_property, ignore.case = TRUE)) {
          meta_markers <- c(meta_markers, sprintf('meta[property="%s"]', meta_property))
        }
      }
      
      # Check content for paywall indicators
      if (!is.na(meta_content)) {
        if (grepl("subscription|premium|paid|metered", meta_content, ignore.case = TRUE)) {
          if (!is.na(meta_name)) {
            meta_markers <- c(meta_markers, sprintf('meta[name="%s"][content~="%s"]', meta_name, meta_content))
          } else if (!is.na(meta_property)) {
            meta_markers <- c(meta_markers, sprintf('meta[property="%s"][content~="%s"]', meta_property, meta_content))
          }
        }
      }
    }
    
    if (length(meta_markers) > 0) {
      markers[["meta_tags"]] <- unique(meta_markers)
    }
    
    return(markers)
  }
  
  # Process each domain
  for (domain_name in unique_domains) {
    message(sprintf("\nAnalyzing domain: %s", domain_name))
    
    # Check for no_paywall flag
    domain_data <- paywall_data[domain == domain_name]
    is_no_paywall_domain <- any(grepl("no_paywall", domain_data$paywall_url, ignore.case = TRUE))
    
    if (is_no_paywall_domain) {
      # No paywall domain - set has_paywall to FALSE
      paywall_rules[[domain_name]] <- list(
        has_paywall = FALSE,
        paywall_markers = list()
      )
      
      # Count successful fetches for this domain
      domain_fetches <- html_storage[domain == domain_name & fetch_success == TRUE]
      successful_free <- nrow(domain_fetches[url_type == "free"])
      successful_paywall <- nrow(domain_fetches[url_type == "paywall"])
      
      # Add to overview
      overview_dt <- rbind(overview_dt, data.table(
        domain = domain_name,
        has_paywall = FALSE,
        n_paywall_markers = 0L,
        paywall_markers = list(list()),
        successful_free_fetches = successful_free,
        successful_paywall_fetches = successful_paywall
      ))
      
      next
    }
    
    # Get HTML data for this domain
    domain_html_data <- html_storage[domain == domain_name & fetch_success == TRUE]
    
    if (nrow(domain_html_data) == 0) {
      message("  No successful HTML fetches for this domain")
      # Domain has paywall (no no_paywall flag) but no successful fetches
      paywall_rules[[domain_name]] <- list(
        has_paywall = TRUE,
        paywall_markers = list()
      )
      
      # Add to overview with zero successful fetches
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
    paywall_html_data <- domain_html_data[url_type == "paywall"]
    free_html_data <- domain_html_data[url_type == "free"]
    
    # Analyze paywall HTMLs
    paywall_markers_list <- list()
    for (i in 1:nrow(paywall_html_data)) {
      markers <- analyze_html_for_markers(paywall_html_data$raw_html[i])
      paywall_markers_list[[i]] <- markers
    }
    
    # Analyze free HTMLs
    free_markers_list <- list()
    for (i in 1:nrow(free_html_data)) {
      markers <- analyze_html_for_markers(free_html_data$raw_html[i])
      free_markers_list[[i]] <- markers
    }
    
    # Find exclusive paywall markers by type
    exclusive_markers <- list()
    
    # Process each marker type
    marker_types <- unique(unlist(lapply(paywall_markers_list, names)))
    
    for (type in marker_types) {
      # Get all markers of this type from paywall pages
      paywall_type_markers <- unique(unlist(lapply(paywall_markers_list, function(m) m[[type]])))
      
      # Get all markers of this type from free pages
      free_type_markers <- unique(unlist(lapply(free_markers_list, function(m) m[[type]])))
      
      # Find exclusive markers
      exclusive_type_markers <- setdiff(paywall_type_markers, free_type_markers)
      
      if (length(exclusive_type_markers) > 0) {
        exclusive_markers[[type]] <- exclusive_type_markers
      }
    }
    
    # Determine if domain has paywall (based on no_paywall flag, not markers)
    has_paywall <- !is_no_paywall_domain # TRUE unless explicitly marked as no_paywall
    
    # Store rules
    paywall_rules[[domain_name]] <- list(
      has_paywall = has_paywall,
      paywall_markers = exclusive_markers
    )
    
    # Calculate total number of markers
    n_markers <- if (length(exclusive_markers) > 0) {
      sum(sapply(exclusive_markers, function(x) {
        if (is.list(x)) length(unlist(x)) else length(x)
      }))
    } else {
      0L
    }
    
    # Count successful fetches for this domain
    successful_free <- nrow(domain_html_data[url_type == "free"])
    successful_paywall <- nrow(domain_html_data[url_type == "paywall"])
    
    # Add to overview
    overview_dt <- rbind(overview_dt, data.table(
      domain = domain_name,
      has_paywall = has_paywall,
      n_paywall_markers = n_markers,
      paywall_markers = list(exclusive_markers),
      successful_free_fetches = successful_free,
      successful_paywall_fetches = successful_paywall
    ))
    
    # Print summary for this domain
    message(sprintf("  Has paywall: %s", has_paywall))
    message(sprintf("  Total markers found: %d", n_markers))
    message(sprintf("  Successful fetches - Free: %d, Paywall: %d", successful_free, successful_paywall))
    if (n_markers > 0) {
      for (type in names(exclusive_markers)) {
        message(sprintf("  - %s: %d markers", type, length(exclusive_markers[[type]])))
      }
    }
  }
  
  # Save paywall rules to RDS
  paywall_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated.rds")
  saveRDS(paywall_rules, paywall_rules_path)
  
  # Save overview data.table
  overview_path <- file.path(paths$parsing_config, "06_paywall_overview.rds")
  saveRDS(overview_dt, overview_path)
  
  message(sprintf("\nPaywall rules setup completed"))
  message(sprintf("Paywall rules saved to: %s", paywall_rules_path))
  message(sprintf("Overview saved to: %s", overview_path))
  
  # Print summary statistics
  message("\n=== Summary Statistics ===")
  message(sprintf("Total domains analyzed: %d", nrow(overview_dt)))
  message(sprintf("Domains with paywalls: %d", sum(overview_dt$has_paywall)))
  message(sprintf("Domains without paywalls: %d", sum(!overview_dt$has_paywall)))
  message(sprintf("Total paywall markers found: %d", sum(overview_dt$n_paywall_markers)))
  
  return(list(
    paywall_rules = paywall_rules,
    overview = overview_dt
  ))
}

# Execute the paywall rules setup function - only run once
# setup_paywall_rules()
rm(setup_paywall_rules)
# Load saved rules
# paywall_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_paywall_rules_generated.rds")
# paywall_overview <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_paywall_overview.rds")



#####



# 2. Main function to setup parser rules with complete code extraction
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
  
  # Function to extract complete parsing logic
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
    s_n_list <- function(datetime_val = NA_character_, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
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

        # Initialize variables
        datetime <- NA_character_
        author <- NA_character_
        headline <- NA_character_
        text <- NA_character_

        # Define helper function
        %s

        # Execute parsing logic within a tryCatch block
        tryCatch({
          %s
          # Return extracted data as a list
          list(
            datetime = as.character(datetime),
            author = as.character(author),
            headline = as.character(headline),
            text = as.character(text)
          )
        }, error = function(e) {
          # Return NAs and error message on failure
          list(
            datetime = NA_character_,
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
setup_parser_rules()

rm(setup_parser_rules)
# Load saved rules and overview
parser_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_rules_fetched.rds")
parser_overview <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_overview.rds")



#####



# 3. Define main parsing rule 
func_06_parse_html <- local({
  
  # --- Private Caching Environment ---
  .parser_rules_cache <- NULL
  .paywall_rules_cache <- NULL
  
  # --- Internal helper function to load rules if needed ---
  .load_cached_rules <- function() {
    paths <- get_module_paths()
    if (is.null(.parser_rules_cache)) {
      parser_rules_path <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
      if (!file.exists(parser_rules_path)) stop("FATAL: Parser rule file not found: ", parser_rules_path)
      message("Loading and caching content parser rules...")
      .parser_rules_cache <<- readRDS(parser_rules_path)
    }
    if (is.null(.paywall_rules_cache)) {
      paywall_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated.rds")
      if (!file.exists(paywall_rules_path)) stop("FATAL: Paywall rule file not found: ", paywall_rules_path)
      message("Loading and caching paywall rules...")
      .paywall_rules_cache <<- readRDS(paywall_rules_path)
    }
  }
  
  # --- Internal helper function to check for paywall markers ---
  .check_paywall_markers <- function(html_parsed, paywall_markers) {
    if (length(paywall_markers) == 0) return(FALSE)
    if (!is.null(paywall_markers$css_selectors)) {
      for (selector in paywall_markers$css_selectors) {
        if (length(html_nodes(html_parsed, selector)) > 0) return(TRUE)
      }
    }
    if (!is.null(paywall_markers$script_blocks)) {
      all_scripts <- paste(html_text(html_nodes(html_parsed, "script")), collapse = " ")
      for (pattern in paywall_markers$script_blocks) {
        if (grepl(pattern, all_scripts, ignore.case = TRUE, fixed = TRUE)) return(TRUE)
      }
    }
    if (!is.null(paywall_markers$meta_tags)) {
      for (meta_selector in paywall_markers$meta_tags) {
        if (length(html_nodes(html_parsed, meta_selector)) > 0) return(TRUE)
      }
    }
    return(FALSE)
  }
  
  # --- NEW: Helper function to sanitize values for data.table assignment ---
  .sanitize_value <- function(value) {
    # If value is NULL or a zero-length vector, return a single NA_character_
    if (is.null(value) || length(value) == 0) {
      return(NA_character_)
    }
    # Otherwise, return the value as a character
    return(as.character(value))
  }
  
  
  # --- The main function that is returned and exposed externally ---
  function(response_result, chunk_name) {
    
    .load_cached_rules()
    
    request_info <- response_result$request_info
    input_info <- list(
      id = request_info$id,
      domain = request_info$domain,
      url = request_info$url
    )
    
    html_content <- tryCatch({
      httr2::resp_body_string(response_result$httr2_response)
    }, error = function(e) {
      func_10_append_error("html_extraction_failed", input_info, chunk_name)
      return(NULL)
    })
    
    if (is.null(html_content) || nchar(html_content) == 0) {
      return(list(success = FALSE, data = NULL, reason = "empty_html_content"))
    }
    
    temp_dt <- data.table(
      id = as.integer(request_info$id),
      domain = as.character(request_info$domain),
      url = as.character(request_info$url),
      timestamp_scraped = as.POSIXct(request_info$request_timestamp),
      date_time = NA_character_,
      author = NA_character_,
      headline = NA_character_,
      text = NA_character_,
      paywall = NA
    )
    
    domain_clean <- sub("\\..*$", "", temp_dt$domain)
    parser_entry <- .parser_rules_cache[[domain_clean]]
    paywall_entry <- .paywall_rules_cache[[domain_clean]]
    
    html_parsed <- tryCatch(rvest::read_html(html_content), error = function(e) NULL)
    
    if (is.null(html_parsed)) {
      func_10_append_error("html_parse_failed", input_info, chunk_name)
      return(list(success = FALSE, data = NULL, reason = "html_parse_failed"))
    }
    
    if (!is.null(paywall_entry)) {
      if (isFALSE(paywall_entry$has_paywall)) {
        temp_dt$paywall <- FALSE
      } else {
        temp_dt$paywall <- .check_paywall_markers(html_parsed, paywall_entry$paywall_markers)
      }
    } else {
      temp_dt$paywall <- FALSE
    }
    
    if (!is.null(parser_entry) && isTRUE(parser_entry$success)) {
      parsing_func <- parser_entry$parse_function
      extracted_data <- tryCatch(parsing_func(html_parsed), error = function(e) list(error = e$message))
      
      if (is.null(extracted_data$error)) {
        # FIX: Sanitize each value before assigning it to the data.table
        temp_dt$date_time <- .sanitize_value(extracted_data$datetime)
        temp_dt$author    <- .sanitize_value(extracted_data$author)
        temp_dt$headline  <- .sanitize_value(extracted_data$headline)
        temp_dt$text      <- .sanitize_value(extracted_data$text)
      }
    }
    
    is_na_headline <- is.na(temp_dt$headline) || nchar(trimws(temp_dt$headline)) == 0
    is_na_text <- is.na(temp_dt$text) || nchar(trimws(temp_dt$text)) == 0
    
    if (!is_na_headline && !is_na_text) {
      # The 'parse_result' argument in the append function expects a data.table or list
      func_10_append_output(temp_dt, input_info, list(), chunk_name)
      return(list(success = TRUE, data = temp_dt))
    } else if (isTRUE(temp_dt$paywall)) {
      func_10_append_error("paywalled_content", input_info, chunk_name)
      return(list(success = FALSE, data = temp_dt, reason = "paywalled_content"))
    } else {
      # The 'parse_result' argument here expects the data that was attempted to be parsed
      func_10_append_parse_error(temp_dt, input_info, html_content, chunk_name)
      return(list(success = FALSE, data = temp_dt, reason = "missing_fields"))
    }
  }
})