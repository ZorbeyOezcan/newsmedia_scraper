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
library(data.table)
library(ggplot2)

# 1: Defining the chunk builder function
func_02_build_chunk <- function(chunk_proportion          = 1 / 10,
                                absolute_links            = NULL,
                                domains                   = NULL,
                                exclude_domains           = NULL,
                                exclude_retry_links       = FALSE,
                                exclude_parse_error_links = TRUE, 
                                seed                      = NULL) {
  if (!is.null(seed)) set.seed(seed)             # reproducibility
  
  paths      <- get_module_paths()
  input_file <- file.path(paths$input, "input.rds")
  if (!file.exists(input_file)) stop("Input file not found: ", input_file)
  dt <- as.data.table(readRDS(input_file))
  
  # filter eligible links
  dt <- dt[processed == FALSE]
  if (exclude_retry_links) dt <- dt[retry == FALSE]
  if (exclude_parse_error_links) dt <- dt[parse_error == FALSE] 
  if (!is.null(exclude_domains)) dt <- dt[!domain %in% exclude_domains]
  
  # filter for specific domains if the 'domains' argument is provided
  if (!is.null(domains) && length(domains) > 0) {
    dt <- dt[domain %in% domains]
  }
  
  if (nrow(dt) == 0) stop("No eligible links available for chunk creation.")
  
  # determine chunk size
  chunk_size <- if (!is.null(absolute_links)) {
    min(absolute_links, nrow(dt))
  } else {
    ceiling(nrow(dt) * chunk_proportion)
  }
  if (chunk_size < 1) stop("Chunk size evaluates to < 1 link.")
  
  # proportional domain sampling
  dom_stats <- dt[, .N, by = domain]
  dom_stats[, n_sample := pmin(round(N / sum(N) * chunk_size), N)]
  
  # close rounding gaps
  remainder <- chunk_size - sum(dom_stats$n_sample)
  if (remainder > 0) {
    dom_stats[, gap := N - n_sample]
    add_pool    <- dom_stats[gap > 0]
    add_domains <- sample(add_pool$domain, remainder,
                          prob = add_pool$gap, replace = TRUE)
    for (d in add_domains) dom_stats[domain == d, n_sample := n_sample + 1]
  }
  
  # draw samples
  dt      <- merge(dt, dom_stats[, .(domain, n_sample)], by = "domain")
  sampled <- dt[, .SD[sample(.N, min(n_sample[1], .N))], by = domain]
  
  # round-robin interleaving
  sampled[, seq_id  := seq_len(.N), by = domain]
  dom_order <- sample(unique(sampled$domain))
  sampled[, dom_ord := match(domain, dom_order)]
  setorder(sampled, seq_id, dom_ord)
  sampled[, c("seq_id", "dom_ord", "n_sample") := NULL]
  
  # file & object names
  chunk_dir  <- file.path(paths$input, "chunks")
  dir.create(chunk_dir, showWarnings = FALSE, recursive = TRUE)
  existing   <- list.files(chunk_dir, pattern = "^chunk_(\\d+)\\.rds$")
  next_id    <- if (length(existing)) {
    max(as.integer(sub("^chunk_(\\d+)\\.rds$", "\\1", existing))) + 1
  } else 1
  chunk_name <- sprintf("chunk_%03d", next_id)
  chunk_path <- file.path(chunk_dir, paste0(chunk_name, ".rds"))
  
  # persist & assign
  saveRDS(sampled, chunk_path)
  assign(chunk_name, sampled, envir = .GlobalEnv)
  
  # console summary
  message("Chunk name                 : ", chunk_name)
  message("This chunk contains        : ", nrow(sampled), " links.")
  message("chunk_proportion           : ", ifelse(is.null(absolute_links),
                                                  chunk_proportion, "—"))
  message("absolute_links             : ", ifelse(is.null(absolute_links),
                                                  "—", absolute_links))
  message("domains                    : ",
          ifelse(is.null(domains), "—",
                 paste(domains, collapse = ", ")))
  message("exclude_domains            : ",
          ifelse(is.null(exclude_domains), "FALSE",
                 paste(exclude_domains, collapse = ", ")))
  message("exclude_retry_links        : ", exclude_retry_links)
  message("exclude_parse_error_links  : ", exclude_parse_error_links) # New summary line
  
  
  invisible(chunk_name)
}


# Calling the function: 
# func_02_build_chunk()



##### 



# Function to visualise domain composition for:
#   – the entire input set,
#   – all unprocessed links,
#   – a provided chunk.

func_02_plot_chunk_overview <- function(chunk,
                                        input_path = file.path(get_module_paths()$input,
                                                               "input.rds")) {
  # accept data.table/data.frame or file name
  if (inherits(chunk, c("data.table", "data.frame"))) {
    chunk_dt <- as.data.table(chunk)
  } else {
    if (!grepl("\\.rds$", chunk, ignore.case = TRUE)) chunk <- paste0(chunk, ".rds")
    chunk_file <- file.path(get_module_paths()$input, "chunks", chunk)
    if (!file.exists(chunk_file)) stop("Chunk file not found: ", chunk_file)
    chunk_dt <- as.data.table(readRDS(chunk_file))
  }
  
  dt_all <- as.data.table(readRDS(input_path))
  
  # counts per domain
  all_cnt    <- dt_all[                , .(n = .N), by = domain][,
                                                                 category := "Total links in input"]
  unproc_cnt <- dt_all[processed == FALSE, .(n = .N), by = domain][,
                                                                   category := "Unprocessed links in input"]
  chunk_cnt  <- chunk_dt[              , .(n = .N), by = domain][,
                                                                 category := "Links in chunk"]
  
  plot_dt <- rbindlist(list(all_cnt, unproc_cnt, chunk_cnt))
  
  dom_levels <- plot_dt[category == "Total links in input"][order(-n), domain]
  plot_dt[, domain := factor(domain, levels = dom_levels)]
  
  ggplot(plot_dt, aes(domain, n, fill = category)) +
    geom_col(position = "stack") +
    labs(x = "Domain",
         y = "Number of links",
         fill = "") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, vjust = .5, hjust = 1))
}

# plot_chunk_overview(chunk_01)
