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