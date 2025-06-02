# ==============================================================================
# MODULE: RETRY LOGIC & BLACKLIST MANAGEMENT
# ==============================================================================
# 
# This module manages the retry queue and maintains blacklists for problematic
# combinations. It decides which failed requests should be retried and when,
# maintains blacklists for domain-VPN combinations that consistently fail,
# tracks problematic user agent and header combinations, and implements
# graduated retry strategies with increasing delays. The module ensures that
# the system doesn't waste resources on impossible requests while maximizing
# the chances of eventually scraping difficult links.
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