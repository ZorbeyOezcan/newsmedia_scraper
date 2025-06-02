# ==============================================================================
# MODULE: REQUEST ORCHESTRATION & WORKER MANAGEMENT
# ==============================================================================
# 
# This module serves as the central control system for all scraping requests.
# It decides which links should be processed by which workers, determines the
# optimal request parameters (timing, aggressiveness level) based on domain
# history, manages the request queue and worker pool, and dynamically adjusts
# scraping aggressiveness based on server responses. The orchestrator makes
# real-time decisions before each request, balancing speed with safety to
# maximize successful scrapes while avoiding detection.
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