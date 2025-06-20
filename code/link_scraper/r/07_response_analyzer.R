# ==============================================================================
# MODULE: RESPONSE ANALYSIS & PATTERN RECOGNITION
# ==============================================================================
# 
# This module analyzes HTTP responses to detect blocking patterns and classify
# response types. It identifies different types of blocks (soft blocks, hard
# blocks, rate limits), detects CAPTCHAs and JavaScript challenges, recognizes
# CloudFlare and other bot protection services, and analyzes response patterns
# across multiple requests. The analyzer provides intelligence about server
# behavior that helps the system adapt its approach and avoid detection by
# learning from response patterns.
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
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