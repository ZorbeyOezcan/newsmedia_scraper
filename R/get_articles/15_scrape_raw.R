# ==============================================================================
# SCRIPT: RUN SCRAPER RAW (DEBUG MODE)
# ==============================================================================
# This script executes a simplified scraping run.
# It bypasses the complex orchestration (Modules 03, 04, 05) and sends
# standard HTTP requests.
#
# GOAL: 
# 1. Fetch pages using standard httr2 requests (no rotation, no camouflage).
# 2. Check response status (must be 200).
# 3. Apply BOT DETECTION rules on the HTML content.
# 4. If 200 OK AND No Bot Detected -> Save RAW HTML to 'parse_error'.
# 5. Provide a summary of success rates per domain at the end.
# ==============================================================================
setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")
# --- 1. SETUP & IMPORTS ---
library(data.table)
library(httr2)
library(rvest)
library(stringr)

# Define path helper function (Crucial for standalone execution)
# We define it here so 01_init.R can use it.
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

# Initialize paths immediately so subsequent sources can use them if needed
paths <- get_module_paths()

# Load essential infrastructure modules only
# Note: 01_init.R deletes 'paths' at the end of its execution.
# Note: 09_storage_manager.R might overwrite 'get_module_paths' with a partial version.
source("01_init.R")
source("02_chunk_manager.R")
source("09_storage_manager.R")
source("10_log_manager.R")

# --- CRITICAL FIX ---
# We must REDEFINE the function here because Modules (like 09) overwrite it 
# with incomplete versions that lack 'parsing_config'.
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

# RE-INITIALIZE paths with the full definition
paths <- get_module_paths()

# --- 2. CONFIGURATION ---

# Define domains to process in this raw run
domains_to_process <- c(
  #"wiwo.de",
  #"businessinsider.de",
  "epochtimes.de" 
  # "faz.net"
  # Add other domains here
)

# Load Bot Detection Rules locally for this raw run
# We load them manually here to avoid dependencies on Module 06 functions
bot_rules_path <- file.path(paths$parsing_config, "06_bot_detection_rules.rds")

# Debug output to verify path logic
message("Checking for bot rules at: ", bot_rules_path)

if (length(bot_rules_path) == 0 || is.na(bot_rules_path)) {
  stop("Path configuration error: 'bot_rules_path' is empty. Check paths$parsing_config.")
}

if (!file.exists(bot_rules_path)) {
  stop("Bot detection rules not found. Please run setup_bot_detection_rules() in 06_html_parser.R first.")
}
bot_rules <- readRDS(bot_rules_path)


# --- 3. BUILD CHUNK ---

# Create a chunk with specific domains (or NULL for all)
current_chunk <- func_02_build_chunk(
  domains = domains_to_process, 
  absolute_links = 10, # Number of links
  exclude_domains = NULL
)

# Load the chunk data into the global environment
chunk_dt <- get(current_chunk, envir = .GlobalEnv)

# Initialize data structures for logging
func_09_init_data_structures(current_chunk)

message(sprintf("Successfully created RAW chunk: %s with %d links", current_chunk, nrow(chunk_dt)))


# --- 4. HELPER FUNCTION: BOT DETECTION ---

# Helper function to check HTML content against bot rules
# Returns TRUE if bot detected, FALSE otherwise
check_for_bot <- function(html_content, rules) {
  if (is.na(html_content) || nchar(html_content) == 0) return(FALSE)
  
  html_parsed <- tryCatch(read_html(html_content), error = function(e) NULL)
  if (is.null(html_parsed)) return(FALSE) # Can't parse, so can't detect
  
  # 1. CSS Selector Check
  for (selector in rules$css_selectors) {
    if (length(html_nodes(html_parsed, selector)) > 0) return(TRUE)
  }
  
  # 2. Text Content Check
  title_text <- html_text(html_node(html_parsed, "title"))
  body_text <- html_text(html_node(html_parsed, "body"))
  combined_text <- paste(title_text, body_text)
  
  for (keyword in rules$text_keywords) {
    if (str_detect(combined_text, fixed(keyword, ignore_case = TRUE))) return(TRUE)
  }
  
  # 3. JavaScript Pattern Check
  # (Simplified for raw text check to avoid complex parsing)
  for (pattern in rules$js_patterns) {
    if (str_detect(html_content, fixed(pattern))) return(TRUE)
  }
  
  return(FALSE)
}


# --- 5. EXECUTION LOOP (RAW MODE) ---

total_links <- nrow(chunk_dt)
message(paste(rep("=", 70), collapse = ""))
message("STARTING RAW SCRAPER RUN")
message("Logic: Request -> Check 200 -> Check Bot -> Save Raw HTML -> Next")
message(paste(rep("=", 70), collapse = ""))

# Initialize counter for summary
# We use a simple list to track results per domain
summary_tracker <- list()

if (total_links > 0) {
  for (i in 1:total_links) {
    
    # 1. Get Link Info
    link_info <- chunk_dt[i]
    url <- link_info$url
    domain <- link_info$domain
    id <- link_info$id
    
    message(sprintf("[%d/%d] Requesting: %s", i, total_links, url))
    
    # 2. Execute Raw Request
    request_success <- FALSE
    html_content <- NA_character_
    status_code <- NA_integer_
    result_status <- "UNKNOWN" # For summary
    
    tryCatch({
      req <- request(url) %>% 
        req_timeout(30) %>% 
        req_error(is_error = function(resp) FALSE) 
      
      resp <- req_perform(req)
      status_code <- resp_status(resp)
      
      # 3. Analyze Response
      if (status_code == 200) {
        # Valid HTTP response, now check content
        html_content <- resp_body_string(resp)
        
        # 3b. Check for Bot Detection
        is_bot <- check_for_bot(html_content, bot_rules)
        
        if (is_bot) {
          message(" --> Status: BOT DETECTED (Skipped)")
          result_status <- "BOT_DETECTED"
        } else {
          request_success <- TRUE
          message(" --> Status: 200 OK (Clean - Saved)")
          result_status <- "200_OK"
        }
        
      } else {
        message(sprintf(" --> Status: FAILED (%d) - Skipped", status_code))
        result_status <- paste0("HTTP_", status_code)
      }
      
    }, error = function(e) {
      message(sprintf(" --> Status: NETWORK ERROR (%s)", e$message))
      result_status <- "NETWORK_ERROR"
    })
    
    # Update summary tracker
    if (is.null(summary_tracker[[domain]])) {
      summary_tracker[[domain]] <- list(total = 0, ok = 0, bot = 0, fail = 0)
    }
    summary_tracker[[domain]]$total <- summary_tracker[[domain]]$total + 1
    
    if (result_status == "200_OK") {
      summary_tracker[[domain]]$ok <- summary_tracker[[domain]]$ok + 1
    } else if (result_status == "BOT_DETECTED") {
      summary_tracker[[domain]]$bot <- summary_tracker[[domain]]$bot + 1
    } else {
      summary_tracker[[domain]]$fail <- summary_tracker[[domain]]$fail + 1
    }
    
    # 4. Save Logic
    if (request_success && !is.na(html_content)) {
      
      temp_dt <- data.table(
        id = id,
        domain = domain,
        url = url,
        timestamp_scraped = Sys.time(),
        date_time = as.POSIXct(NA),
        author = NA_character_,
        headline = NA_character_,
        text = NA_character_,
        paywall = as.logical(NA) 
      )
      
      func_10_append_parse_error(
        processed_dt = temp_dt,
        html_content = html_content,
        chunk_name = current_chunk
      )
    }
    
    Sys.sleep(1)
  }
}

message(paste(rep("=", 70), collapse = ""))
message("RAW PROCESSING COMPLETE.")


# --- 6. SUMMARY REPORT ---

message("\n========================================================")
message("                   DOMAIN SUMMARY                       ")
message("========================================================")

for (dom in names(summary_tracker)) {
  stats <- summary_tracker[[dom]]
  
  pct_ok <- round((stats$ok / stats$total) * 100, 1)
  pct_fail <- round(((stats$fail + stats$bot) / stats$total) * 100, 1)
  
  message(sprintf("%s : Total %d links | %s%% OK (%d) | %s%% Non-200/Bot (%d Fail, %d Bot)", 
                  dom, stats$total, pct_ok, stats$ok, pct_fail, stats$fail, stats$bot))
}

message("========================================================")


# --- 7. POST-PROCESSING & SAVING ---

message("\n--- Starting Post-Processing ---")

# 1. Clean Duplicates
func_09_clean_dupes()

# 2. Save Chunk Data
func_09_fill_output(current_chunk)

# 3. Split Parse Errors
func_09_process_parse_errors()

# 4. Update Main Input File
func_09_update_input()

message("Run finished successfully!")