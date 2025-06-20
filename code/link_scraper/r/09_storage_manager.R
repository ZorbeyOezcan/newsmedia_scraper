# ==============================================================================
# MODULE: DATA STORAGE & CHECKPOINT MANAGEMENT
# ==============================================================================
# 
# This module handles all persistent storage operations and maintains system
# checkpoints. It saves successfully parsed articles to structured output files,
# creates checkpoint saves every 1000 processed links, manages the retry queue
# persistence, stores and updates system state between runs, and merges chunk
# outputs into final datasets. The module ensures data integrity and enables
# the system to resume from any point in case of interruption.
#
# RECEIVES FROM:
# - 06_html_parser: Successfully parsed articles
# - 07_response_analyzer: Failed requests for retry queue
# - 04_request_orchestrator: Session states and progress data
# 
# OUTPUTS TO:
# - File system: Persistent storage of all data structures
# - 08_domain_state_manager: Chunk results for analysis
#
# ==============================================================================

# Load required packages
library(data.table)

# Configuration Function
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

# 1: Function to initialize chunk-specific data structures
func_09_init_data_structures <- function(chunk_name) {
  # accept names like "chunk_001", "chunk_123" …
  if (is.null(chunk_name) || !grepl("^chunk_\\d{3}$", chunk_name)) {
    stop("Invalid chunk name. Expected format: 'chunk_001', 'chunk_002', etc.")
  }
  
  chunk_number <- as.integer(gsub("chunk_", "", chunk_name))
  paths        <- get_module_paths()
  
  # ensure folders exist
  dir.create(paths$chunk_logs,    showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$chunk_outputs, showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$chunk_inputs,  showWarnings = FALSE, recursive = TRUE)
  
  # output container
  chunk_output <- data.table(
    id               = integer(),
    domain           = character(),
    url              = character(),
    timestamp_scraped= as.POSIXct(character()),
    date_time        = character(),
    author           = character(),
    headline         = character(),
    text             = character(),
    paywall          = logical()
  )
  
  # retry container
  chunk_retry <- data.table(
    id                  = integer(),
    domain              = character(),
    url                 = character(),
    timestamp_scraped   = as.POSIXct(character()),
    from_chunk          = integer(),
    status_code         = integer(),
    response_headers    = list(),
    response_body       = character(),
    response_time       = numeric(),
    retry_reason        = character(),
    server_date         = as.POSIXct(character()),
    content_type        = character(),
    content_length      = integer(),
    server             = character(),
    user_agent_id       = integer(),
    ip_address          = character(),
    dns_time            = numeric(),
    connect_time        = numeric(),
    total_time          = numeric(),
    curl_error_code     = integer(),
    ssl_verify_result   = integer(),
    redirect_count      = integer(),
    rate_limit_remaining= integer(),
    rate_limit_reset    = as.POSIXct(character()),
    retry_after         = integer()
  )
  
  # error container
  chunk_error <- data.table(
    id           = integer(),
    domain       = character(),
    url          = character(),
    error_reason = character(),
    timestamp    = as.POSIXct(character()),
    session_id   = character(),
    user_agent_id= integer(),
    ip_address   = character(),
    http_status  = integer(),
    error_type   = character()
  )
  
  # parse-error container
  chunk_parse_error <- data.table(
    id                 = integer(),
    domain             = character(),
    url                = character(),
    timestamp_scraped  = as.POSIXct(character()),
    date_time          = character(),
    author             = character(),
    headline           = character(),
    text               = character(),
    paywall            = logical(),
    html_content       = character(),
    parse_error_reason = character(),
    missing_fields     = character()
  )
  
  # request log
  chunk_request_log <- data.table(
    request_id          = integer(),
    id                  = integer(),
    domain              = character(),
    url                 = character(),
    timestamp_sent      = as.POSIXct(character()),
    from_chunk          = integer(),
    user_agent_id       = integer(),
    ip_address          = character(),
    session_id          = character(),
    headers_sent        = list(),
    aggressiveness_level= integer(),
    request_method      = character(),
    timeout_setting     = numeric()
  )
  
  # response log
  chunk_response_log <- data.table(
    response_id        = integer(),
    request_id         = integer(),
    id                 = integer(),
    domain             = character(),
    url                = character(),
    timestamp_received = as.POSIXct(character()),
    from_chunk         = integer(),
    status_code        = integer(),
    response_headers   = list(),
    response_body      = character(),
    response_time      = numeric(),
    server_date        = as.POSIXct(character()),
    content_type       = character(),
    content_length     = integer(),
    server            = character(),
    user_agent_id      = integer(),
    ip_address         = character(),
    dns_time           = numeric(),
    connect_time       = numeric(),
    total_time         = numeric(),
    curl_error_code    = integer(),
    ssl_verify_result  = integer(),
    redirect_count     = integer(),
    rate_limit_remaining= integer(),
    rate_limit_reset   = as.POSIXct(character()),
    retry_after        = integer(),
    cache_status       = character(),
    encoding           = character()
  )
  
  # expose in global env
  assign(paste0(chunk_name, "_output"),        chunk_output,      envir = .GlobalEnv)
  assign(paste0(chunk_name, "_retry"),         chunk_retry,       envir = .GlobalEnv)
  assign(paste0(chunk_name, "_error"),         chunk_error,       envir = .GlobalEnv)
  assign(paste0(chunk_name, "_parse_error"),   chunk_parse_error, envir = .GlobalEnv)
  assign(paste0(chunk_name, "_request_log"),   chunk_request_log, envir = .GlobalEnv)
  assign(paste0(chunk_name, "_response_log"),  chunk_response_log,envir = .GlobalEnv)
  
  # console feedback
  message(sprintf("Chunk data structures initialised for: %s", chunk_name))
}


