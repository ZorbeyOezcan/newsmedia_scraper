# ==============================================================================
# MODULE: 11 - OVERVIEW & VISUALIZATION MANAGER
# ==============================================================================
#
# This module generates a progress overview for the scraping process.
# It aggregates data from the input, final output, and parse error files
# to create a summary table and a stacked bar chart visualizing the progress.
#
# RECEIVES FROM:
# - data/input/input.rds
# - data/output/final_data.rds
# - data/input/parse_error/ (domain-specific files)
#
# OUTPUTS TO:
# - Console: A summary data.table of the scraping progress.
# - Plots Pane: A ggplot visualization of the progress.
#
# ==============================================================================

# Load required packages for data manipulation and plotting
library(data.table)
library(ggplot2)

get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    input          = file.path(base_path, "data", "input"),
    output         = file.path(base_path, "data", "output"),
    config         = file.path(base_path, "data", "config"),
    parsing_config = file.path(base_path, "data", "config", "06_parsing_config"),
    state          = file.path(base_path, "data", "state"),
    logs           = file.path(base_path, "data", "logs"),
    chunk_outputs  = file.path(base_path, "data", "output", "chunk_outputs"),
    chunk_inputs   = file.path(base_path, "data", "input",  "chunk_inputs"),
    parse_error    = file.path(base_path, "data", "input", "parse_error")
  )
}

# --- Main Function to Generate Progress Report from input.rds ---
func_11_generate_progress_report <- function() {
  
  # --- 1. Define File Paths ---
  paths <- get_module_paths()
  input_path <- file.path(paths$input, "input.rds")
  
  # --- 2. Load the Master Input Dataset ---
  message("Loading master 'input.rds' for report generation...")
  if (!file.exists(input_path)) stop("Error: input.rds not found at the specified path!")
  
  input_dt <- readRDS(input_path)
  setDT(input_dt)
  
  # --- 3. Aggregate All Statistics Directly from input.rds ---
  message("Calculating statistics per domain based on status flags...")
  
  # Logic definitions:
  # - Processed:   processed == TRUE
  # - Parse Error: parse_error == TRUE
  # - Error:       error == TRUE
  # - Remaining:   processed == FALSE & error == FALSE & parse_error == FALSE
  
  summary_dt <- input_dt[, .(
    total_in = .N,
    count_processed   = sum(processed == TRUE, na.rm = TRUE),
    count_parse_error = sum(parse_error == TRUE, na.rm = TRUE),
    count_error       = sum(error == TRUE, na.rm = TRUE),
    count_remaining   = sum(processed == FALSE & error == FALSE & parse_error == FALSE, na.rm = TRUE)
  ), by = domain]
  
  # --- 4. Create and Print the Enhanced Overview Table ---
  overview_table <- summary_dt[, .(
    Domain        = domain,
    `Input Links` = total_in,
    `Processed`   = count_processed,
    `Error`       = count_error,
    `Parse Error` = count_parse_error,
    `Remaining`   = count_remaining
  )]
  
  # Calculate progress rate ((Processed + Error) / Total) for sorting
  overview_table[, `Progress Rate` := round(((`Processed` + `Error`) / `Input Links`) * 100, 2)]
  
  # Sort the final table by Progress Rate descending
  setorder(overview_table, -`Progress Rate`)
  
  message("\n--- Scraping Progress Overview (from input.rds) ---")
  print(overview_table)
  
  # --- 5. Prepare Data for the Plot ---
  
  # Calculate percentages for the stacked bar chart
  summary_dt[, pct_processed   := (count_processed / total_in) * 100]
  summary_dt[, pct_parse_error := (count_parse_error / total_in) * 100]
  summary_dt[, pct_error       := (count_error / total_in) * 100]
  summary_dt[, pct_remaining   := (count_remaining / total_in) * 100]
  
  # Melt the data into a long format for ggplot
  plot_data <- melt(summary_dt,
                    id.vars = "domain",
                    measure.vars = c("pct_processed", "pct_parse_error", "pct_error", "pct_remaining"),
                    variable.name = "status",
                    value.name = "percentage")
  
  # Define the factor levels to control the stacking order in the plot
  # Order: Processed (bottom) -> Parse Error -> Error -> Remaining (top)
  plot_data$status <- factor(plot_data$status,
                             levels = c("pct_remaining", "pct_error", "pct_parse_error", "pct_processed"),
                             labels = c("Remaining", "Error", "Parse Error", "Processed"))
  
  # --- 6. Generate and Print the Final Plot ---
  
  # Order domains in the plot based on the table sorting (Progress Rate)
  plot_data[, domain := factor(domain, levels = rev(overview_table$Domain))]
  
  combined_plot <- ggplot(plot_data, aes(x = percentage, y = domain, fill = status)) +
    geom_col() + # Creates the stacked bar chart
    geom_text(
      # Add labels inside the bars only if the segment is large enough (> 5%)
      aes(label = ifelse(percentage > 5, paste0(round(percentage, 1), "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 3.5,
      color = "white",
      fontface = "bold"
    ) +
    # Define custom colors:
    # Processed   -> Green
    # Parse Error -> Blue
    # Error       -> Orange
    # Remaining   -> Red
    scale_fill_manual(values = c(
      "Processed"   = "#2ca02c",  # Green
      "Parse Error" = "#0072B2",  # Blue
      "Error"       = "#ff7f0e",  # Orange
      "Remaining"   = "#d62728"   # Red
    )) +
    labs(
      title = "Scraping Status Distribution by Domain",
      x = "Percentage (%)",
      y = "Domain",
      fill = "Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      # Left-aligned Y-axis text (domain names)
      axis.text.y = element_text(face = "bold", hjust = 0) 
    )

  invisible(overview_table)
}


# --- Function to Compare Chunk Run Performance ---
func_11_compare_chunk_runs <- function(overview_before, overview_after, chunk_dt) {
  
  # --- 1. Validate Inputs ---
  if (!is.data.table(overview_before) || !is.data.table(overview_after) || !is.data.table(chunk_dt)) {
    stop("All inputs (overview_before, overview_after, chunk_dt) must be data.tables.")
  }
  
  # --- 2. Calculate Totals Before and After ---
  # Note: Column names must match the output of func_11_generate_progress_report above
  
  scraped_before <- sum(overview_before$`Processed`, na.rm = TRUE)
  scraped_after  <- sum(overview_after$`Processed`, na.rm = TRUE)
  
  parse_error_before <- sum(overview_before$`Parse Error`, na.rm = TRUE)
  parse_error_after  <- sum(overview_after$`Parse Error`, na.rm = TRUE)
  
  # Error column is now explicit
  error_before <- sum(overview_before$`Error`, na.rm = TRUE)
  error_after  <- sum(overview_after$`Error`, na.rm = TRUE)
  
  # --- 3. Calculate Deltas (Changes) ---
  
  total_links_in_chunk <- nrow(chunk_dt)
  newly_scraped    <- scraped_after - scraped_before
  new_parse_errors <- parse_error_after - parse_error_before
  new_errors       <- error_after - error_before
  
  # Calculate success rate for this specific chunk run (Successes / Total)
  success_rate <- if (total_links_in_chunk > 0) {
    round((newly_scraped / total_links_in_chunk) * 100, 2)
  } else {
    0
  }
  
  # --- 4. Print Summary ---
  
  message(paste(rep("=", 60), collapse = ""))
  message("--- Chunk Run Performance Summary ---")
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-35s %d", "Total links in chunk:", total_links_in_chunk))
  message(sprintf("%-35s %d", "New Processed (Success):", newly_scraped))
  message(sprintf("%-35s %d", "New Parse Errors:", new_parse_errors))
  message(sprintf("%-35s %d", "New Errors (Block/404):", new_errors))
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-35s %.2f%%", "Chunk Success Rate:", success_rate))
  message(paste(rep("=", 60), collapse = ""))
  
  invisible(list(
    total_links = total_links_in_chunk,
    newly_scraped = newly_scraped,
    new_parse_errors = new_parse_errors,
    new_errors = new_errors,
    success_rate = success_rate
  ))
}


# --- Function to Summarize HTTP Responses for a Chunk ---
func_11_summarize_http_responses <- function(chunk_name) {
  
  # Construct the name of the response log object
  log_object_name <- paste0(chunk_name, "_response_log")
  
  # Check if the log object exists in the global environment
  if (!exists(log_object_name, envir = .GlobalEnv)) {
    message(sprintf("\nResponse log '%s' not found. Cannot generate summary.", log_object_name))
    return(invisible())
  }
  
  # Retrieve the log data.table
  response_log <- get(log_object_name, envir = .GlobalEnv)
  
  # Check if the log is empty
  if (nrow(response_log) == 0) {
    message("\nResponse log is empty. No HTTP responses to summarize.")
    return(invisible())
  }
  
  total_responses <- nrow(response_log)
  
  # Group by the 'response_analysis' column
  summary_dt <- response_log[, .(
    count = .N
  ), by = .(response_type = ifelse(is.na(response_analysis), "NA_in_analysis", response_analysis))]
  
  # Calculate percentage
  summary_dt[, percentage := (count / total_responses) * 100]
  
  # Order by count descending
  setorder(summary_dt, -count)
  
  # --- Print the formatted summary ---
  message(paste(rep("=", 60), collapse = ""))
  message("--- HTTP Response Summary ---")
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-30s %-10s %s", "Response Type", "Count", "Percentage"))
  message(paste(rep("-", 60), collapse = ""))
  
  # Iterate and print each row
  for (i in 1:nrow(summary_dt)) {
    row <- summary_dt[i]
    line <- sprintf("%-30s %-10d %.2f%%",
                    row$response_type,
                    row$count,
                    row$percentage)
    message(line)
  }
  
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-30s %-10d 100.00%%", "Total", total_responses))
  message(paste(rep("=", 60), collapse = ""))
  
  invisible(summary_dt)
}