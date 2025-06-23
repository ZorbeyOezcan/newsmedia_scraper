# ==============================================================================
# MODULE 06: HTML PARSER - ENHANCED PAYWALL RULES SETUP
# ==============================================================================
# 
# This function fetches HTML content from paywall and free URLs for each domain,
# analyzes the HTML structure including script blocks and meta tags to identify
# paywall-specific markers, and saves the results as RDS files for later use.
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr)
library(httr2)
library(rvest)
library(stringr)
library(jsonlite)


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
      url_type = character(),  # "paywall" or "free"
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
      Sys.sleep(2)  # Longer delay before retry
      
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
        
        Sys.sleep(1)  # Delay between requests
      }
      
      # Fetch free URLs
      for (i in seq_along(free_urls)) {
        url <- free_urls[i]
        message(sprintf("Fetching free link %d of %d", i, length(free_urls)))
        
        result <- fetch_url_with_retry(url, "free", domain_name, user_agents)
        html_storage <- rbind(html_storage, result)
        
        Sys.sleep(1)  # Delay between requests
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
    "\\\\d+[,.]\\\\d+\\s*€.*(/\\s*(Monat|Woche))"
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
            snippet <- gsub("\\s+", " ", snippet)  # Normalize whitespace
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
    has_paywall <- !is_no_paywall_domain  # TRUE unless explicitly marked as no_paywall
    
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



# 2. Main function to setup parser rules with enhanced tracking
setup_parser_rules <- function() {
  paths <- get_module_paths()
  dir.create(paths$parsing_config, showWarnings = FALSE, recursive = TRUE)
  
  csv_path     <- file.path(paths$input, "paywall_domains.csv")
  output_rds   <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
  overview_rds <- file.path(paths$parsing_config, "06_parser_overview.rds")
  
  if (!file.exists(csv_path)) {
    stop("paywall_domains.csv not found at: ", csv_path)
  }
  
  # Load CSV 
  message("Loading paywall domains CSV...")
  pw <- data.table::fread(csv_path, sep = ";", encoding = "UTF-8",
                          header = TRUE, blank.lines.skip = TRUE)
  data.table::setnames(pw, old = names(pw),
                       new = c("domain", "paywall_url", "free_url"))
  
  # Ensure every domain ends with ".de" for fetching 
  pw[, domain_full := ifelse(grepl("\\.de$", domain, ignore.case = TRUE),
                             domain,
                             paste0(domain, ".de"))]
  
  # create cleaned name (without TLD) for storage
  pw[, domain := sub("\\..*$", "", domain_full)]
  
  domains_full <- unique(pw$domain_full)
  domains_full <- domains_full[!is.na(domains_full) & nzchar(domains_full)]
  message(sprintf("Found %d unique domains to process", length(domains_full)))
  
  # Helpers 
  domain_to_file <- function(d) {
    switch(d,
           "augsburger-allgemeine.de" = "deliver_augsburger_allgemeine.R",
           "n-tv.de"                  = "deliver_n-tv_de.R",
           "faz.de"                   = "deliver_faz_net.R", 
           sprintf("deliver_%s.R", gsub("[-\\.]", "_", d)))
  }
  
  pull <- function(txt, pat) {
    m <- regmatches(txt, regexec(pat, txt, perl = TRUE))
    if (length(m[[1]]) > 1) trimws(m[[1]][2]) else NA_character_
  }
  
  
  extract_assignment <- function(txt, var) {
    # Look for '<-' that starts the assignment to the requested `var`
    # and capture everything until the next recognised assignment
    rx <- sprintf(
      "(?s)\\b%s\\s*<-\\s*(.*?)\\n\\s*(?:(?:datetime|headline|author|text)\\s*<-|s_n_list\\s*\\()",
      var
    )
    m <- regmatches(txt, regexec(rx, txt, perl = TRUE))
    if (length(m[[1]]) > 1) trimws(m[[1]][2]) else NA_character_
  }
  
  extract_rules <- function(txt) {
    uses_json <- grepl("jsonlite::fromJSON", txt, fixed = TRUE)
    
    datetime_rule <- extract_assignment(txt, "datetime")
    headline_rule <- extract_assignment(txt, "headline")
    author_rule   <- extract_assignment(txt, "author")
    text_rule     <- extract_assignment(txt, "text")
    
    n_rules <- sum(!is.na(c(datetime_rule, headline_rule, author_rule, text_rule)))
    
    list(
      rules = list(
        datetime = list(selector = datetime_rule, type = if (uses_json) "json" else "html"),
        headline = list(selector = headline_rule, type = if (uses_json) "json" else "html"),
        author   = list(selector = author_rule,   type = if (uses_json) "json" else "html"),
        text     = list(selector = text_rule,     type = if (uses_json) "json" else "html"),
        uses_json = uses_json
      ),
      stats = list(
        uses_json = uses_json,
        n_parse_rules_extracted = n_rules,
        success = (n_rules == 4)
      )
    )
  }
  
  # Storage objects 
  rules <- list()                    # named by cleaned domain later
  overview_dt <- data.table(
    domain = character(),
    uses_json = logical(),
    n_parse_rules_extracted = integer(),
    success = logical()
  )
  ok <- 0L; fail <- 0L
  
  # Fetch & process Paperboy scripts
  message("\nFetching parser rules from paperboy repository...")
  
  for (d_full in domains_full) {
    d_clean <- sub("\\..*$", "", d_full)          # cleaned name for storage
    
    raw_url <- sprintf(
      "https://raw.githubusercontent.com/JBGruber/paperboy/main/R/%s",
      domain_to_file(d_full)                      # uses full (possibly .de-appended) domain
    )
    
    txt <- tryCatch(
      httr::content(httr::GET(raw_url), as = "text", encoding = "UTF-8"),
      error = function(e) NULL
    )
    
    if (is.null(txt)) {
      message(sprintf("✗ %s – download failed", d_full))
      fail <- fail + 1L
      
      overview_dt <- rbind(overview_dt, data.table(
        domain = d_clean,
        uses_json = NA,
        n_parse_rules_extracted = 0L,
        success = FALSE
      ))
      
      rules[[d_clean]] <- list(
        datetime  = list(selector = NA_character_, type = "html"),
        headline  = list(selector = NA_character_, type = "html"),
        author    = list(selector = NA_character_, type = "html"),
        text      = list(selector = NA_character_, type = "html"),
        uses_json = FALSE
      )
      next
    }
    
    extraction_result <- extract_rules(txt)
    rules[[d_clean]] <- extraction_result$rules
    
    overview_dt <- rbind(overview_dt, data.table(
      domain = d_clean,
      uses_json = extraction_result$stats$uses_json,
      n_parse_rules_extracted = extraction_result$stats$n_parse_rules_extracted,
      success = extraction_result$stats$success
    ))
    
    ok <- ok + 1L
    if (extraction_result$stats$success) {
      message(sprintf("✓ %s – all rules extracted successfully", d_full))
    } else {
      message(sprintf("⚠ %s – partial extraction (%d/4 rules found)",
                      d_full, extraction_result$stats$n_parse_rules_extracted))
    }
  }
  
  # Save results 
  saveRDS(rules, output_rds)
  saveRDS(overview_dt, overview_rds)
  message(sprintf("\nParser rules saved to: %s", output_rds))
  message(sprintf("Overview saved to: %s", overview_rds))
  
  # Summary 
  message("\n=== Parser Rules Extraction Summary ===")
  message(sprintf("Total unique domains processed: %d", length(domains_full)))
  message(sprintf("Paperboy R scripts successfully loaded: %d", ok))
  message(sprintf("Failed downloads: %d", fail))
  if (nrow(overview_dt) > 0) {
    message(sprintf("\nExtraction success rate: %.1f%%",
                    sum(overview_dt$success, na.rm = TRUE) / nrow(overview_dt) * 100))
    message(sprintf("Domains using JSON parsing: %d",
                    sum(overview_dt$uses_json, na.rm = TRUE)))
    message(sprintf("Average rules extracted per domain: %.2f",
                    mean(overview_dt$n_parse_rules_extracted)))
    
    incomplete_domains <- overview_dt[success == FALSE & n_parse_rules_extracted > 0]$domain
    if (length(incomplete_domains) > 0) {
      message("\nDomains with incomplete rule extraction:")
      for (dom in incomplete_domains) {
        n_rules <- overview_dt[domain == dom]$n_parse_rules_extracted
        message(sprintf("  - %s (%d/4 rules)", dom, n_rules))
      }
    }
  }
  
  invisible(list(rules = rules, overview = overview_dt))
}

# Run once 
# setup_parser_rules()

rm(setup_parser_rules)
# Load saved rules and overview
# parser_rules <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_rules_fetched.rds")
# parser_overview <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/config/06_parsing_config/06_parser_overview.rds")



#####



# 3. Initialize parsing function. 
func_06_parse_html <- local({
  # Private cache variables for parsing rules
  .parser_rules_cache <- NULL
  .paywall_rules_cache <- NULL
  
  # Return the main function
  function(response_result, chunk_name = current_chunk) {
    
    # unified return object 
    make_ret <- function(success_flag, data = NULL, err = NULL, msg = "") {
      list(
        success           = success_flag,
        action            = "parse",                   # constant
        response_analysis = if (success_flag) "valid" else "invalid",
        parse_result      = list(
          success = success_flag,
          data    = data,
          error   = err
        ),
        message = msg
      )
    }
    
    # Function to load and cache parsing rules
    .load_cached_rules <- function() {
      if (is.null(.parser_rules_cache) || is.null(.paywall_rules_cache)) {
        paths <- get_module_paths()
        
        # read RDS files
        parser_rules_path  <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
        if (length(parser_rules_path) == 0 || !file.exists(parser_rules_path))
          stop(sprintf("Parser rules not found at: %s", parser_rules_path))
        .parser_rules_cache <<- readRDS(parser_rules_path)
        
        paywall_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated.rds")
        if (!file.exists(paywall_rules_path))
          stop("Paywall rules not found at: ", paywall_rules_path)
        .paywall_rules_cache <<- readRDS(paywall_rules_path)
        
        # harmonise list names 
        fix_names <- function(nms) {
          vapply(nms, function(n) {
            if (grepl("\\.", n))           return(n)              # already has TLD
            if (n == "faz")                  return("faz.net")   # special case
            paste0(n, ".de")
          }, character(1))
        }
        names(.parser_rules_cache)  <<- fix_names(names(.parser_rules_cache))
        names(.paywall_rules_cache) <<- fix_names(names(.paywall_rules_cache))
        
        message("Parsing rules loaded, cached, and domain names normalised")
      }
      list(parser = .parser_rules_cache, paywall = .paywall_rules_cache)
    }
    
    # Helper function to check for paywall markers
    .check_paywall_markers <- function(html_parsed, paywall_markers) {
      # Return FALSE immediately if no markers to check
      if (length(paywall_markers) == 0) {
        return(FALSE)
      }
      
      # Check CSS selectors
      if (!is.null(paywall_markers$css_selectors)) {
        for (selector in paywall_markers$css_selectors) {
          # Try to find nodes matching the selector
          nodes <- tryCatch({
            html_nodes(html_parsed, selector)
          }, error = function(e) NULL)
          
          # If any nodes found, paywall detected
          if (!is.null(nodes) && length(nodes) > 0) {
            return(TRUE)
          }
        }
      }
      
      # Check script blocks for patterns
      if (!is.null(paywall_markers$script_blocks)) {
        # Get all script content
        script_nodes <- html_nodes(html_parsed, "script")
        all_scripts <- paste(html_text(script_nodes), collapse = " ")
        
        # Check each script pattern
        for (pattern in paywall_markers$script_blocks) {
          if (grepl(pattern, all_scripts, ignore.case = TRUE, fixed = TRUE)) {
            return(TRUE)
          }
        }
      }
      
      # Check meta tags
      if (!is.null(paywall_markers$meta_tags)) {
        for (meta_selector in paywall_markers$meta_tags) {
          nodes <- tryCatch({
            html_nodes(html_parsed, meta_selector)
          }, error = function(e) NULL)
          
          if (!is.null(nodes) && length(nodes) > 0) {
            return(TRUE)
          }
        }
      }
      
      # No paywall markers found
      return(FALSE)
    }
    
    # Helper function to apply parser rules
    .apply_parser_rule <- function(html_parsed, rule, json_df = NULL) {
      # Check if rule is valid
      if (is.null(rule) || is.na(rule$selector) || is.null(rule$selector)) {
        return(NA_character_)
      }
      
      # Try to evaluate the rule
      result <- tryCatch({
        # Suppress warnings during evaluation
        suppressWarnings({
          if (!is.null(rule$type) && rule$type == "json" && !is.null(json_df)) {
            # Evaluate JSON-based rule
            eval(parse(text = rule$selector))
          } else {
            # Evaluate HTML-based rule
            # Make html available for the eval
            html <- html_parsed
            eval(parse(text = rule$selector))
          }
        })
      }, error = function(e) {
        NA_character_
      })
      
      # Handle NULL or empty results
      if (is.null(result) || length(result) == 0) {
        return(NA_character_)
      }
      
      # Take first element if multiple
      if (length(result) > 1) {
        result <- result[1]
      }
      
      # Convert to character
      result <- tryCatch(
        as.character(result),
        error = function(e) NA_character_
      )
      
      # Check for empty string
      if (is.na(result) || identical(result, "") || identical(result, character(0))) {
        return(NA_character_)
      }
      
      return(result)
    }
    
    # Main Function logic
    
    # Validate input
    if (!is.list(response_result) || !response_result$success) {
      warning("Invalid response result provided to parser")
      return(make_ret(FALSE, NULL, "bad_input", "Response result invalid"))
    }
    
    # Extract HTML content from response
    html_content <- tryCatch({
      resp_body_string(response_result$httr2_response)
    }, error = function(e) {
      warning("Failed to extract HTML content from response")
      return(make_ret(FALSE, NULL, "html_extract_fail", "Couldn't extract body string"))
    })
    
    # Extract information from response
    request_info <- response_result$request_info
    url    <- request_info$url
    domain <- request_info$domain
    
    input_info <- list(id = request_info$id, domain = domain, url = url)
    
    # Initialize temporary data.table
    temp_dt <- data.table(
      domain = domain,
      url = url,
      timestamp_scraped = Sys.time(),
      date_time = NA_character_,
      author = NA_character_,
      headline = NA_character_,
      text = NA_character_,
      paywall = NA,
      bot_detect = FALSE  # Default to FALSE for now
    )
    
    # Load cached rules
    rules <- .load_cached_rules()
    parser_rules  <- rules$parser[[domain]]
    paywall_rules <- rules$paywall[[domain]]
    
    if (is.null(parser_rules)) {
      func_10_append_error(error_reason = paste("No parser rules for domain:", domain),
                           input_info   = input_info,
                           chunk_name   = chunk_name)
      return(make_ret(FALSE, NULL, "no_rules", "Missing parser rules"))
    }
    
    # Parse HTML
    html_parsed <- tryCatch({
      read_html(html_content)
    }, error = function(e) {
      func_10_append_error(error_reason = paste("HTML parsing failed:", e$message),
                           input_info   = input_info,
                           chunk_name   = chunk_name)
      return(make_ret(FALSE, NULL, "html_parse_fail", "HTML parsing failed"))
    })
    
    # STEP 1: Check for bot detection (placeholder for now)
    
    
    
    # temp_dt$bot_detect remains FALSE
    
    # STEP 2: Check for paywall
    if (!is.null(paywall_rules)) {
      if (isFALSE(paywall_rules$has_paywall)) {
        temp_dt$paywall <- FALSE
      } else {
        temp_dt$paywall <- .check_paywall_markers(html_parsed, paywall_rules$paywall_markers)
      }
    } else {
      temp_dt$paywall <- FALSE
    }
    
    # STEP 3: Extract article elements
    # Check if domain uses JSON parsing
    json_df <- NULL
    if (!is.null(parser_rules$uses_json) && parser_rules$uses_json) {
      # Extract JSON-LD content
      json_txt <- tryCatch({
        html_parsed %>%
          html_elements("script[type='application/ld+json']") %>%
          html_text()
      }, error = function(e) character(0))
      
      # Parse JSON if found
      if (length(json_txt) > 0 && nchar(json_txt[1]) > 0) {
        json_df <- tryCatch({
          fromJSON(json_txt[1])
        }, error = function(e) NULL)
      }
    }
    
    # Apply parser rules to extract fields
    temp_dt$date_time <- .apply_parser_rule(html_parsed, parser_rules$datetime, json_df)
    temp_dt$author <- .apply_parser_rule(html_parsed, parser_rules$author, json_df)
    temp_dt$headline <- .apply_parser_rule(html_parsed, parser_rules$headline, json_df)
    temp_dt$text <- .apply_parser_rule(html_parsed, parser_rules$text, json_df)
    
    # Format datetime if successfully extracted
    if (!is.na(temp_dt$date_time)) {
      formatted_date <- suppressWarnings(tryCatch({
        dt <- as_datetime(temp_dt$date_time)
        if (!is.na(dt)) {
          as.character(dt)
        } else {
          temp_dt$date_time
        }
      }, error = function(e) {
        temp_dt$date_time
      }))
      temp_dt$date_time <- formatted_date
    }
    
    # STEP 4: Check results and route to appropriate function
    # Check if bot detected
    if (temp_dt$bot_detect) {
      # Bot detected - send to retry
      func_10_append_retry(
        retry_reason = "bot_detected",
        url = url,
        chunk_name = chunk_name
      )
      return(invisible(FALSE))
    }
    
    # Count NA fields 
    na_count <- sum(is.na(temp_dt[, .(date_time, author, headline, text)]))
    
    # Build parse_result list 
    parse_result <- list(
      date_time = temp_dt$date_time,
      author    = temp_dt$author,
      headline  = temp_dt$headline,
      text      = temp_dt$text,
      paywall   = temp_dt$paywall
    )
    
    # Routing: success / errors 
    if (na_count == 0) {
      func_10_append_output(parse_result  = parse_result,
                            input_info    = input_info,
                            response_info = list(),
                            chunk_name    = chunk_name)
      return(make_ret(TRUE, parse_result, NULL, "Parsed OK"))
    }
    
    if (na_count > 0 && isTRUE(temp_dt$paywall)) {
      func_10_append_error(error_reason = "paywalled_content",
                           input_info   = input_info,
                           chunk_name   = chunk_name)
      return(make_ret(FALSE, parse_result, "paywalled_content", "Paywall detected, missing fields"))
    }
    
    # Missing fields but no paywall 
    func_10_append_parse_error(parse_result = parse_result,
                               input_info   = input_info,
                               html_content = html_content,
                               chunk_name   = chunk_name)
    return(make_ret(FALSE, parse_result, "missing_fields", "Missing one or more fields"))
  }
})
