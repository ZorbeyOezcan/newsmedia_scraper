# ==============================================================================
# MODULE: HTTP REQUEST EXECUTION
# ==============================================================================
# 
# This module handles the actual HTTP request execution with all specified
# parameters. It sends HTTP requests using the provided identity parameters
# (user agent, headers, cookies), implements proper timeout and error handling,
# captures complete response data including headers and status codes, and
# measures request performance metrics. The module acts as the interface
# between the orchestration logic and the actual network communication,
# ensuring that all requests are executed exactly as specified by the
# orchestrator.
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