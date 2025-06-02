# ==============================================================================
# MODULE: CHUNK CREATION & SAMPLING
# ==============================================================================
# 
# This module handles the intelligent division of the complete link list into
# balanced chunks for processing. It implements stratified sampling to ensure
# proportional domain representation in each chunk, applies multi-level shuffling
# to maximize the temporal distance between requests to the same domain, and
# assigns links to workers using round-robin distribution. The module ensures
# that each chunk represents approximately 1/10th of the total workload while
# maintaining optimal domain distribution for anti-blocking purposes.
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