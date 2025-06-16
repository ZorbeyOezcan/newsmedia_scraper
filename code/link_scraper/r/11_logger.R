# ==============================================================================
# MODULE: CENTRALIZED LOGGING SYSTEM
# ==============================================================================
# 
# This module provides unified logging functionality for the entire system.
# It captures all events, errors, and metrics from every module, maintains
# structured log files with consistent formatting, provides real-time console
# output for monitoring, aggregates statistics for performance analysis, and
# ensures that all system behavior is traceable for debugging. The logger is
# essential for monitoring system health and troubleshooting issues.
#
# RECEIVES FROM:
# 
# OUTPUTS TO:
#
# ==============================================================================

# Load required packages
library(data.table)
library(httr2)

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

