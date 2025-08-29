# ==============================================================================
# MODULE: 12 - DATA CLEANING
# ==============================================================================
#
# This module provides functionality to clean datasets based on predefined exclusion
# rules. It removes unwanted links from the main input.rds and the domain-specific
# parse_error RDS files. The exclusion rules include specific URL paths per domain,
# a general list of exact URLs, and URLs that have returned a 410 status code.
#
# This script is designed to be run standalone and is not part of the main
# scraper execution pipeline (run_scraper.Rmd).
#
# ==============================================================================

# Load required packages
library(data.table)
library(stringr)

# --- 1. CONFIGURATION & SETUP ---

get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input       = file.path(base_path, "data", "input"),
    output      = file.path(base_path, "data", "output"),
    logs        = file.path(base_path, "data", "logs"),
    parse_error = file.path(base_path, "data", "input", "parse_error")
  )
}

paths <- get_module_paths()


# --- 2. EXCLUSION LISTS ---

# -- A. Domain-specific URL path exclusions --
# Any URL on that domain containing this path will be removed. 
# (e.g., "shop/" removes "domain.com/shop/anything")
# Is mostly admin related, contains ads or ist non article content 

`augsburger-allgemeine.de_excluded_links` <- c(
  "panorama/panorama-bilder-des-tages-" 
)

`berliner-kurier.de_excluded_links` <- c(
  "ticketshop/"
)

`berliner-zeitung.de_excluded_links` <- c(
  "open-source/", 
  "ticketshop/",
  "topics/",
  "berlin/syndication.383984",
  "berlin/kuendigung.282144"
)

`bild.de_excluded_links` <- c(
  "regional/",
  "tv/",
  "unterhaltung/tv-fernsehformate/"
)

`bnn.de_excluded_links` <- c(
  "thema/"
)

`br.de_excluded_links` <- c(
  "br-fernsehen/", 
  "fernsehen/",
  "sogehtmedien/"
)

`businessinsider` <- c(
  "themen/",
  "insider-picks/",
  "abo/"
)

`derwesten.de_excluded_links` <- c()

`echo24.de_excluded_links` <- c(
  "geschaeftskunden/"
)

`epochtimes.de_excluded_links` <- c()

`faz.net.de_excluded_links` <- c()

`fnp.de_excluded_links` <- c()

`fr.de_excluded_links` <- c()

`frankenpost.de_excluded_links` <- c()

`freiepresse.de_excluded_links` <- c()

`heidelberg24.de_excluded_links` <- c()

`heise.de_excluded_links` <- c(
  "download/",
  "select/", 
  "thema/"
)

`karlsruhe-insider.de_excluded_links` <- c(
  "uebernachtung/"
)

`kreiszeitung.de_excluded_links` <- c()

`manager-magazin.de_excluded_links` <- c()

`mdr.de_excluded_links` <- c(
  "geschichte/"
)

`mopo.de_excluded_links` <- c(
  "sportwetten/",
  "hambrug/polizei/",
  "hambrug/gericht/"
)

`n-tv.de_excluded_links` <- c(
  "infografik/"
)

`ndr.de_excluded_links` <- c(
  "fernsehen/", 
  "geschichte/",
  "kultur/sendungen/"
)

`news.de_excluded_links` <- c()

`newsflash24.de_excluded_links` <- c(
  "blaulicht/",
  "horoskope/",
  "impressum/",
  "kultur-freizeit/",
  "tag/",
  "unterhaltung/podcasts/"
)

`nordkurier.de_excluded_links` <- c()

`noz.de_excluded_links` <- c(
  "audiothek/",
  "kontakt/",
  "service/"
)

`presseportal.de_excluded_links` <- c()

`rollingstone.de_excluded_links` <- c()

`rp-online.de_excluded_links` <- c()

`rtl.de_excluded_links` <- c(
  "sendungen/",
  "smart-tv/",
  "sport/moderatoren/",
  "sport/nfl/draft/"
)

`ruhr24.de_excluded_links` <- c()

`schwaebische.de_excluded_links` <- c()

`shz.de_excluded_links` <- c()

`spiegel.de_excluded_links` <- c()

`stuttgarter-zeitung.de_excluded_links` <- c()

`swp.de_excluded_links` <- c()

`swr.de_excluded_links` <- c()

`swr3.de_excluded_links` <- c()

`t3n.de_excluded_links` <- c(
  "tag/"
)

`tag24.de_excluded_links` <- c(
  "thema/",
  "unternehmen/"
)

`tagesspiegel.de_excluded_links` <- c()

`taz.de_excluded_links` <- c()

`volksstimme.de_excluded_links` <- c()

`wa.de_excluded_links` <- c()

`watson.de_excluded_links` <- c()

`welt.de_excluded_links` <- c(
  "vermischtes/bilder-des-tages/",
  "autor/",
  "themen"
)

`wiwo.de_excluded_links` <- c()

`wz.de_excluded_links` <- c()

`zeit.de_excluded_links` <- c()


# -- B. General exclusion list for exact URLs --
# Add any full URL here to remove it from all datasets.
excluded_links <- c(
  "https://www.augsburger-allgemeine.de/sport/fc-augsburg/fc-augsburg-unter-thorup-jubilaeum-und-chance-auf-ungeschlagene-serie-gegen-gladbach-106034005", 
  
  "https://www.berliner-kurier.de/",
  "https://www.berliner-kurier.de/topics", 
  "https://www.berliner-kurier.de/kuendigung.297245",
  
  "http://www.br.de/franken/inhalt/nachrichten/index.html",
  "http://www.br.de/radio/bayern2/sendungen/weltempfaenger/index.html",
  "http://www.br.de/radio/bayern2/sendungen/bayern-2-am-samstagvormittag/index.html",
  "http://www.br.de/frech-und-frei-naerrisches-aus-franken/index.html",
  "http://www.br.de/radio/bayern2/sendungen/gesundheitsgespraech/index.html",
  "http://www.br.de/puls/programm/internet-girl/index.html",
  "http://www.br.de/naerrische-weinprobe/index.html",
  "http://www.br.de/kinder/hoeren/pumuckl/index.html",
  "http://www.br.de/puls/programm/puls-radio/index.html",
  "http://www.br.de/radio/bayern2/podcasts/uwe-timm-ikarien/index.html",
  "http://www.br.de/kinder/hoeren/index.html",
  "http://www.br.de/fastnacht-in-franken/index.html",
  "http://www.br.de/franken/inhalt/kultur/index.html",
  "http://www.br.de/kinder/schauen/pumuckl/index.html",
  
  "https://www.mopo.de/purple_issue/",
  "https://www.mopo.de/hamburg/gericht/",
  "https://www.mopo.de/hamburg/polizei/",
  
  "https://www.swr3.de/aktuell/nachrichten/t-rex-teen-dino-badlands-100.html",
  "https://www.swr3.de/aktuell/nachrichten/oktopus-wagen-mainz-pfandflaschen-102.html",
  
  "https://www.berliner-zeitung.de/",
  "https://www.berliner-zeitung.de/topics",
  
  "https://www.noz.de/service",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/alice-weidel",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/olaf-scholz",
  "https://www.noz.de/deutschland-welt/politik/bundestagswahl/robert-habeck",
  "https://www.noz.de/lebenswelten/geld-verbraucher/noz-advertorial-vermoegenstag",
  "https://www.noz.de/sport/amateurfussball-os/hallenfussball-os",
  "https://www.noz.de/sport/ergebnisse-tabellen/fussball/kreisliga-os",
  "https://www.noz.de/sport/fussball/champions-league",
  "https://www.noz.de/sport/vfl-osnabrueck/spielplan",
  "https://www.noz.de/sport/vfl-osnabrueck/tabelle"
)


# --- 3. FUNCTIONS ---

# Function to retrieve all URLs that returned a 410 status code.
get_410_links <- function(log_path) {
  message("Searching for links with status code 410 in response_log.rds...")
  
  if (!file.exists(log_path)) {
    warning("response_log.rds not found. Cannot retrieve 410 links.")
    return(character(0))
  }
  
  response_log <- readRDS(log_path)
  setDT(response_log)
  
  links_410 <- response_log[status_code == 410, unique(url)]
  
  if (length(links_410) > 0) {
    message(sprintf("Found %d unique URLs with status code 410.", length(links_410)))
  } else {
    message("No URLs with status code 410 found.")
  }
  
  return(links_410)
}


# Main function to perform the cleaning process.
clean_datasets <- function(paths) {
  message("\n--- Starting Data Cleaning Process ---")
  
  # Step 1: Gather all links to be removed
  response_log_path <- file.path(paths$logs, "response_log.rds")
  empty_410_links <- get_410_links(response_log_path)
  exact_urls_to_remove <- unique(c(excluded_links, empty_410_links))
  message(sprintf("Total of %d unique exact URLs (manual + 410s) will be removed.", length(exact_urls_to_remove)))
  
  # Step 2: Define all target files
  files_to_clean <- c(file.path(paths$input, "input.rds"))
  if (dir.exists(paths$parse_error)) {
    domain_parse_files <- list.files(paths$parse_error, pattern = "\\.rds$", full.names = TRUE)
    files_to_clean <- c(files_to_clean, domain_parse_files)
  }
  message(sprintf("Found %d RDS files to clean.", length(files_to_clean)))
  
  # Step 3: Initialize counters for the final report
  report_counters <- list(
    input = list(exact = 0, path = 0, special = 0),
    parse_error = list(exact = 0, path = 0, special = 0)
  )
  
  # Step 4: Iterate through each file and apply cleaning rules
  for (file_path in files_to_clean) {
    if (!file.exists(file_path)) {
      warning(sprintf("File not found, skipping: %s", basename(file_path)))
      next
    }
    
    message(sprintf("\nProcessing file: %s", basename(file_path)))
    dt <- readRDS(file_path)
    setDT(dt)
    initial_rows <- nrow(dt)
    
    file_type <- ifelse(grepl("input.rds", file_path), "input", "parse_error")
    
    if (initial_rows == 0) {
      message("   File is empty, nothing to do.")
      next
    }
    
    # -- A. Remove exact URL matches --
    rows_before <- nrow(dt)
    if (length(exact_urls_to_remove) > 0) {
      dt <- dt[!url %in% exact_urls_to_remove]
    }
    report_counters[[file_type]]$exact <- report_counters[[file_type]]$exact + (rows_before - nrow(dt))
    
    # -- B. Remove by domain-specific path lists --
    rows_before <- nrow(dt)
    exclusion_lists <- ls(pattern = "_excluded_links$", envir = .GlobalEnv)
    
    current_parse_domain <- if (file_type == "parse_error") {
      sub("_parse_error\\.rds$", "", basename(file_path))
    } else NA_character_
    
    for (list_name in exclusion_lists) {
      domain_with_tld <- str_replace(list_name, "_excluded_links$", "")
      excluded_paths  <- get(list_name, envir = .GlobalEnv)
      if (length(excluded_paths) == 0) next
      
      paths_regex <- paste(excluded_paths, collapse = "|")
      
      host_regex <- gsub("\\.", "\\\\.", domain_with_tld)
      
      # This regex now correctly looks for the domain followed by one of the excluded paths.
      url_pattern_exact_host <- sprintf("^https?://(www\\.)?%s/(%s)", host_regex, paths_regex)
      
      if (file_type == "input") {
        # The comparison now correctly uses the 'domain' column which contains the TLD.
        dt <- dt[!(str_detect(url, url_pattern_exact_host) & domain == domain_with_tld)]
      } else {
        if (!identical(current_parse_domain, domain_with_tld)) next
        dt <- dt[!str_detect(url, url_pattern_exact_host)]
      }
    }
    
    report_counters[[file_type]]$path <- report_counters[[file_type]]$path + (rows_before - nrow(dt))
    
    # -- C. Remove by special domain-specific rules --
    # This section is now corrected to use the full domain name with TLD for comparisons.
    rows_before <- nrow(dt)
    
    # mdr.de: ends with index.html
    dt <- dt[!(domain == 'mdr.de' & str_detect(url, "index\\.html$"))]
    
    # rp-online.de: contains video strings
    dt <- dt[!(domain == 'rp-online.de' & str_detect(url, "_bid-|_vid-|_iid-"))]
    
    # taz.de: contains video strings or is a column
    dt <- dt[!(domain == 'taz.de' & (str_detect(url, "/!t5") | str_detect(url, "taz\\.de/Kolumne-")))]
    
    # br.de: contains "kontakt"
    dt <- dt[!(domain == 'br.de' & str_detect(url, "kontakt"))]
    
    # Rules based on URL path structure
    if(nrow(dt) > 0) {
      # newsflash24.de & rtl.de: exactly one segment after domain
      dt <- dt[!(domain %in% c('newsflash24.de','rtl.de') &
                   grepl("^https?://(?:www\\.)?(newsflash24\\.de|rtl\\.de)/[^/]+/?$", url))]
      
      # tag24.de: exactly two segments after domain
      dt <- dt[!(domain == 'tag24.de' & str_count(str_remove(url, "^https?://[^/]+"), "/") == 2)]
    }
    report_counters[[file_type]]$special <- report_counters[[file_type]]$special + (rows_before - nrow(dt))
    
    # -- D. Report and Save --
    total_removed <- initial_rows - nrow(dt)
    if (total_removed > 0) {
      message(sprintf("   -> Removed %d rows in total from this file.", total_removed))
      saveRDS(dt, file_path)
      message(sprintf("   Saved cleaned data back to %s.", basename(file_path)))
    } else {
      message("   No links matching exclusion criteria were found in this file.")
    }
  }
  
  # --- 5. FINAL REPORT ---
  message("\n--- Data Cleaning Process Finished ---")
  message(paste(rep("=", 50), collapse = ""))
  message("DELETION SUMMARY:")
  message(paste(rep("-", 50), collapse = ""))
  
  message("\nFrom input.rds:")
  message(sprintf("   %d entries deleted by exact URL match (incl. 410s).", report_counters$input$exact))
  message(sprintf("   %d entries deleted by excluded domain paths.", report_counters$input$path))
  message(sprintf("   %d entries deleted by special domain rules.", report_counters$input$special))
  
  message("\nFrom all Parse Error datasets combined:")
  message(sprintf("   %d entries deleted by exact URL match (incl. 410s).", report_counters$parse_error$exact))
  message(sprintf("   %d entries deleted by excluded domain paths.", report_counters$parse_error$path))
  message(sprintf("   %d entries deleted by special domain rules.", report_counters$parse_error$special))
  
  message(paste(rep("=", 50), collapse = ""))
}

clean_datasets(paths)









# Build a URL regex that matches the exact host (with TLD) and any of the given path fragments
.build_url_pattern <- function(domain_with_tld, excluded_paths) {
  host_regex  <- gsub("\\.", "\\\\.", domain_with_tld)
  paths_regex <- paste(excluded_paths, collapse = "|")
  sprintf("^https?://(www\\.)?%s/.*?(%s)", host_regex, paths_regex)
}

# Apply path exclusions WITH strict domain column check (for input/final_data tables)
.apply_path_exclusions_with_domain <- function(dt) {
  exclusion_lists <- ls(pattern = "_excluded_links$", envir = .GlobalEnv)
  to_drop_idx <- integer(0)
  
  for (list_name in exclusion_lists) {
    domain_with_tld <- str_replace(list_name, "_excluded_links$", "")   # e.g., "n-tv.de"
    excluded_paths  <- get(list_name, envir = .GlobalEnv)
    if (length(excluded_paths) == 0) next
    
    url_pattern <- .build_url_pattern(domain_with_tld, excluded_paths)
    hits <- which(stringr::str_detect(dt$url, url_pattern) & dt$domain == domain_with_tld)
    if (length(hits)) to_drop_idx <- c(to_drop_idx, hits)
  }
  unique(to_drop_idx)
}

# Apply special rules (UNCHANGED, as requested) using TLD domains
.apply_special_rules <- function(dt) {
  # Structure-based rules
  if (nrow(dt) > 0) {
    dt[, path_segments := stringr::str_count(stringr::str_remove(url, "^https?://[^/]+"), "/")]
    dt <- dt[!(domain %in% c('newsflash24.de', 'rtl.de') & path_segments == 1)]
    dt <- dt[!(domain == 'tag24.de' & path_segments == 2)]
    dt[, path_segments := NULL]
  }
  # Domain-specific string rules
  dt <- dt[!(domain == 'mdr.de'       & stringr::str_detect(url, "index\\.html$"))]
  dt <- dt[!(domain == 'rp-online.de' & stringr::str_detect(url, "_bid-|_vid-|_iid-"))]
  dt <- dt[!(domain == 'taz.de'       & stringr::str_detect(url, "/!t5"))]
  dt <- dt[!(domain == 'br.de'        & stringr::str_detect(url, "kontakt"))]
  dt
}

# --- Main function: clean final_data and append discarded rows -----------------

clean_final_and_append_discarded <- function(paths) {
  final_path   <- file.path(paths$output, "final_data.rds")
  discarded_path <- file.path(paths$output, "discarded.rds")
  response_log <- file.path(paths$logs, "response_log.rds")
  
  if (!file.exists(final_path)) {
    stop(sprintf("File not found: %s", final_path))
  }
  
  message("\n--- Cleaning final_data.rds and appending discarded rows ---")
  dt <- readRDS(final_path); data.table::setDT(dt)
  initial_n <- nrow(dt)
  
  # (A) Exact URL exclusions: manual list + 410s from response log
  manual_excluded <- get0("excluded_links", envir = .GlobalEnv, ifnotfound = character(0))
  links_410       <- get_410_links(response_log)
  exact_remove    <- unique(c(manual_excluded, links_410))
  
  discard_idx <- integer(0)
  if (length(exact_remove)) {
    discard_idx <- c(discard_idx, which(dt$url %in% exact_remove))
  }
  
  # (B) Path exclusions WITH domain column check
  discard_idx <- c(discard_idx, .apply_path_exclusions_with_domain(dt))
  discard_idx <- unique(discard_idx)
  
  # (C) Special rules — compute additional discards by applying rules to a copy
  dt_after_special <- .apply_special_rules(data.table::copy(dt))
  if (nrow(dt_after_special) < nrow(dt)) {
    # Identify which rows would be removed by special rules
    kept_urls <- dt_after_special$url
    extra_idx <- which(!(dt$url %in% kept_urls))
    discard_idx <- unique(c(discard_idx, extra_idx))
  }
  
  # (D) Build discarded table and append to discarded.rds (create if missing)
  discarded_now <- if (length(discard_idx)) dt[sort(discard_idx)] else dt[0]
  if (nrow(discarded_now) > 0) {
    discarded_now[, source_file := "final_data.rds"]
    if (file.exists(discarded_path)) {
      old <- readRDS(discarded_path); data.table::setDT(old)
      combined <- data.table::rbindlist(list(old, discarded_now), fill = TRUE, use.names = TRUE)
      saveRDS(combined, discarded_path)
      message(sprintf("Appended %d rows to %s (total now: %d).",
                      nrow(discarded_now), basename(discarded_path), nrow(combined)))
    } else {
      saveRDS(discarded_now, discarded_path)
      message(sprintf("Created %s with %d rows.", basename(discarded_path), nrow(discarded_now)))
    }
  } else {
    message("No rows to archive into discarded.rds.")
  }
  
  # (E) Remove discarded rows from final_data.rds and save
  dt_clean <- if (length(discard_idx)) dt[-sort(discard_idx)] else dt
  saveRDS(dt_clean, final_path)
  message(sprintf("final_data.rds: %d -> %d rows", initial_n, nrow(dt_clean)))
}

# --- Execute for provided paths ------------------------------------------------
paths <- get_module_paths()
clean_final_and_append_discarded(paths)

