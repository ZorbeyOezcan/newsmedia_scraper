# ==============================================================================
# MODULE: DOMAIN STATE TRACKING & OPTIMIZATION
# ==============================================================================
# 
# This module maintains a comprehensive state for each domain to optimize
# scraping parameters. It tracks success rates for different aggressiveness
# levels per domain, monitors which user agents and headers work best,
# calculates optimal request intervals to avoid rate limiting, and generates
# recommendations for future chunks. The module's intelligence allows the
# system to learn from experience and automatically optimize its approach
# for each domain over time.
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
#
# ==============================================================================

# Load required packages
suppressPackageStartupMessages({
  library(here)
})

# Configuration Function
get_module_paths <- function() {
  base_path <- here::here()
  list(
    input = file.path(base_path, "data", "input"),
    output = file.path(base_path, "data", "output"),
    config = file.path(base_path, "data", "config"),
    state = file.path(base_path, "data", "state"),
    logs = file.path(base_path, "data", "logs")
  )
}