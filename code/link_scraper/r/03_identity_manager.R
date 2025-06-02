# ==============================================================================
# MODULE: IDENTITY MANAGEMENT (VPN, USER-AGENTS, HEADERS)
# ==============================================================================
# 
# This module manages all aspects of request identity to maintain realistic
# browsing behavior. It handles the rotation of user agents and headers while
# maintaining session consistency, ensures that identity combinations remain
# realistic for the current VPN, manages cookie jars per domain, and tracks
# which identity combinations have been used. The module prevents suspicious
# patterns like using 50 different user agents from the same IP address and
# ensures that identity changes follow realistic browsing patterns.
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