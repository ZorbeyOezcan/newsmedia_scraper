library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")


overview <- create_parse_error_overview()

####
setup_local_rules()

# wiwo.de

# Load full data 
# load_parse_error("wiwo.de")


# Sample data
sample_htmls("wiwo.de", 30)

# Test sample 
parse_html_local(wiwo_sample)
# all wrong

aaa_test <- pb_deliver("https://www.wiwo.de/unternehmen/auto/jahresprognose-hyundai-rechnet-mit-weniger-umsatzwachstum/30180466.html")



new_wiwo_parser_code <- '
function(html) {
  library(rvest)
  library(lubridate)
  library(jsonlite)
  library(stringr)

  # --- Helper Function ---
  html_search <- function(html, selectors, attributes = NULL, all = TRUE, n = 1L) {
    if (all) {
        res <- rvest::html_elements(html, paste0(selectors, collapse = ","))
    } else {
        res <- NULL
        i <- 1L
        l <- length(selectors)
        while (length(res) < 1 && i <= l) {
            res <- rvest::html_elements(html, selectors[i])
            i <- i + 1
        }
    }
    
    want_text <- "text" %in% attributes
    if (want_text) attributes <- setdiff(attributes, "text")
    
    out <- rvest::html_attrs(res) %>%
        unlist(recursive = FALSE) %>%
        subset(., names(.) %in% attributes) %>%
        unname()
    
    if (want_text) out <- c(out, rvest::html_text2(res))
    
    if (is.null(out)) return(NA_character_)
    return(utils::head(out, n))
  }

  # --- Extraction Logic ---
  
  # Initialize variables
  datetime <- as.POSIXct(NA)
  author <- NA_character_
  headline <- NA_character_
  text <- NA_character_

  # 1. DATETIME
  # Priority 1: Clean Meta Tag (ISO format) -> "2025-02-05T10:27:10+01:00"
  tryCatch({
    dt_str <- html_search(html, c("meta[property=\'article:published_time\']"), attributes = "content", all = FALSE)
    if (!is.na(dt_str)) {
      datetime <- lubridate::as_datetime(dt_str)
    } 
    
    # Priority 2: Text from <app-story-date> -> "23.01.2025 - 10:33 Uhr"
    if (is.na(datetime)) {
      dt_text <- html_search(html, c("app-story-date"), attributes = "text", all = FALSE)
      if (!is.na(dt_text)) {
        # Clean string: remove "Uhr" and whitespace
        dt_clean <- gsub("Uhr", "", dt_text)
        dt_clean <- trimws(dt_clean)
        # Parse format "23.01.2025 - 10:33"
        datetime <- lubridate::parse_date_time(dt_clean, orders = "d.m.Y - H:M", tz = "Europe/Berlin")
      }
    }
  }, error = function(e) NULL)

  # 2. AUTHOR
  # Priority 1: Meta Tag -> "Kevin Gallant"
  author <- html_search(html, c("meta[property=\'article:author\']"), attributes = "content", all = FALSE)

  # 3. HEADLINE
  # Target: <app-header-content-headline>
  headline <- html_search(html, c("app-header-content-headline"), attributes = "text", all = FALSE)
  
  # Fallback if specific tag missing
  if (is.na(headline)) {
      headline <- html_search(html, c("h1"), attributes = "text", all = FALSE)
  }

  # 4. TEXT
  # Target: <app-rich-text>
  # User instruction: "Alles was in den app rich text stellen ist"
  # We grab ALL elements (n=999) and paste them together.
  tryCatch({
      text_paragraphs <- html_search(html, c("app-rich-text"), attributes = "text", all = TRUE, n = 999)
      
      if (length(text_paragraphs) > 0 && !all(is.na(text_paragraphs))) {
         text <- paste(text_paragraphs, collapse = " \\n ")
      }
  }, error = function(e) { text <- NA_character_ })

  list(
    datetime = datetime,
    author = as.character(author),
    headline = as.character(headline),
    text = as.character(text)
  )
}
'

# Apply the parser rule to LOCAL config
edit_parse_rules(
  domain_name = "wiwo",
  rule_type = "parser_rules",
  new_rule = new_wiwo_parser_code
)




# test: 
parse_html_local(wiwo_sample, force_reload = TRUE)
# perf

# check results 
analyze_parse_error(wiwo_sample_parsed_local, original = FALSE)
# perffff



# Load full data 
load_parse_error("wiwo.de")

# parse local 
parse_html_local(wiwo_parse_error)

analyze_parse_error(wiwo_parse_error_parsed_local, original = FALSE)
# perffff# perffff# perffff 

fill_results("wiwo_parse_error_parsed_local")

# Change rule 
apply_new_rule("wiwo", "parser_rules")

show_parse_rules("wiwo.de")




# all compromised with JS only
# 
#
#
# ==============================================================================
# SKRIPT: DELETE FREIEPRESSE COMPLETE
# ==============================================================================

library(data.table)
library(stringr)

# --- 1. KONFIGURATION ---

base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"

paths <- list(
  input          = file.path(base_path, "data", "input", "input.rds"),
  final_data     = file.path(base_path, "data", "output", "final_data.rds"),
  error          = file.path(base_path, "data", "output", "error.rds"),
  retry          = file.path(base_path, "data", "input", "retry.rds"),
  parse_error    = file.path(base_path, "data", "input", "parse_error.rds"),
  response_log   = file.path(base_path, "data", "logs", "response_log.rds"),
  request_log    = file.path(base_path, "data", "logs", "request_log.rds")
)

parse_error_dir <- file.path(base_path, "data", "input", "parse_error")

# --- ANPASSUNG FÜR wiwo.DE ---
target_domain_pattern <- "^(www\\.)?wiwo\\.de(\\.de)?$"
target_url_pattern <- "wiwo\\.de"
# ------------------------------------

# --- 2. FUNKTION ZUM LÖSCHEN ---

clean_file <- function(file_path, file_name) {
  if (!file.exists(file_path)) {
    message(sprintf("[%s] Datei nicht gefunden: Überpringe.", file_name))
    return(NULL)
  }
  
  dt <- readRDS(file_path)
  if (!is.data.table(dt)) dt <- as.data.table(dt)
  
  initial_rows <- nrow(dt)
  if (initial_rows == 0) {
    message(sprintf("[%s] Datei ist leer.", file_name))
    return(NULL)
  }
  
  rows_to_delete <- integer(0)
  
  # 1. Filterung über Domain-Spalte
  if ("domain" %in% names(dt)) {
    idx_domain <- which(grepl(target_domain_pattern, dt$domain, ignore.case = TRUE))
    rows_to_delete <- c(rows_to_delete, idx_domain)
  }
  
  # 2. Filterung über URL-Spalte
  if ("url" %in% names(dt)) {
    idx_url <- which(grepl(target_url_pattern, dt$url, ignore.case = TRUE))
    rows_to_delete <- c(rows_to_delete, idx_url)
  }
  
  rows_to_delete <- unique(rows_to_delete)
  deleted_count <- length(rows_to_delete)
  
  if (deleted_count > 0) {
    dt_clean <- dt[-rows_to_delete]
    saveRDS(dt_clean, file_path)
    message(sprintf("[%s] %d Einträge gelöscht. (Vorher: %d -> Nachher: %d)", 
                    file_name, deleted_count, initial_rows, nrow(dt_clean)))
  } else {
    message(sprintf("[%s] Keine Einträge für wiwo.de gefunden.", file_name))
  }
}

# --- 3. AUSFÜHRUNG ---

message("--- STARTE LÖSCHVORGANG FÜR wiwo.DE ---")

# A. Hauptdateien bereinigen
for (name in names(paths)) {
  clean_file(paths[[name]], name)
}

# B. Domain-spezifische Parse-Error Dateien bereinigen
if (dir.exists(parse_error_dir)) {
  message("\n--- Prüfe Parse-Error Ordner ---")
  pe_files <- list.files(parse_error_dir, pattern = "\\.rds$", full.names = TRUE)
  
  for (f in pe_files) {
    # Wenn der Dateiname explizit "wiwo" enthält -> Datei löschen
    if (grepl("wiwo", basename(f), ignore.case = TRUE)) {
      file.remove(f)
      message(sprintf("[Parse Error Dir] Datei gelöscht: %s", basename(f)))
    } else {
      clean_file(f, basename(f))
    }
  }
}

message("\n--- FERTIG ---")
