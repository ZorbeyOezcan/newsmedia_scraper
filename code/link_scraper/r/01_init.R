# ==============================================================================
# MODULE: INITIALIZATION & CONFIGURATION
# ==============================================================================
# 
# This module initializes the entire scraping system by loading all necessary 
# configurations, validating the environment setup, and preparing the runtime
# environment. It ensures all required directories exist, loads configuration
# files (user agents, headers, VPN lists), validates system dependencies, and
# sets up the logging framework. This module acts as the entry point that 
# prepares the system for operation and must be run before any other modules.
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