# ==============================================================================
# MODULE: 13 - OVERVIEW & VISUALIZATION MANAGER
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


# --- Main Function to Generate Progress Report from input.rds ---
func_13_generate_progress_report <- function() {
  
  # --- 1. Define File Paths ---
  paths <- get_module_paths()
  input_path <- file.path(paths$input, "input.rds")
  
  # --- 2. Load the Master Input Dataset ---
  message("Loading master 'input.rds' for report generation...")
  if (!file.exists(input_path)) stop("Error: input.rds not found at the specified path!")
  
  input_dt <- readRDS(input_path)
  setDT(input_dt)
  
  # --- 3. Aggregate All Statistics Directly from input.rds ---
  message("Calculating statistics per domain from input flags...")
  
  summary_dt <- input_dt[, .(
    total_in = .N,
    total_out = sum(processed, na.rm = TRUE),
    total_parse_error = sum(parse_error, na.rm = TRUE)
  ), by = domain]
  
  # --- 4. Create and Print the Enhanced Overview Table ---
  overview_table <- summary_dt[, .(
    Domain = domain,
    `Input Links` = total_in,
    `Scraped Links` = total_out,
    `Parse Error Links` = total_parse_error
  )]
  
  overview_table[, `Progress Rate` := round((`Scraped Links` / `Input Links`) * 100, 2)]
  overview_table[, `Parse Error Percentage` := round((`Parse Error Links` / `Input Links`) * 100, 2)]
  overview_table[, `Combined Percentage` := `Progress Rate` + `Parse Error Percentage`]
  
  # Sort the final table by Progress Rate for the plot ordering
  setorder(overview_table, -`Combined Percentage`)
  
  message("\n--- Scraping Progress Overview (from input.rds) ---")
  print(overview_table)
  
  # --- 5. Prepare Data for the Three-Part Plot ---
  summary_dt[, percent_success := (`total_out` / `total_in`) * 100]
  summary_dt[, percent_parse_error := (`total_parse_error` / `total_in`) * 100]
  summary_dt[, percent_remaining := 100 - percent_success - percent_parse_error]
  
  # Melt the data into a long format for ggplot
  plot_data <- melt(summary_dt,
                    id.vars = "domain",
                    measure.vars = c("percent_success", "percent_parse_error", "percent_remaining"),
                    variable.name = "status",
                    value.name = "percentage")
  
  # Define the order and labels for the legend and stacked bar
  plot_data$status <- factor(plot_data$status,
                             levels = c("percent_remaining", "percent_parse_error", "percent_success"),
                             labels = c("Remaining", "Parse Error", "Success"))
  
  # --- 6. Generate and Print the Final Plot ---
  
  # Order domains in the plot based on the sorted overview_table
  plot_data[, domain := factor(domain, levels = rev(overview_table$Domain))]
  
  combined_plot <- ggplot(plot_data, aes(x = percentage, y = domain, fill = status)) +
    geom_col() + # Creates the stacked bar chart
    geom_text(
      # Add labels inside the bars
      aes(label = ifelse(percentage > 5, paste0(round(percentage, 2), "%"), "")),
      position = position_stack(vjust = 0.5),
      size = 3.5,
      color = "white",
      fontface = "bold"
    ) +
    # Define custom colors for each status
    scale_fill_manual(values = c("Success" = "#2ca02c", 
                                 "Parse Error" = "#0072B2", 
                                 "Remaining" = "#d62728")) +
    labs(
      title = "Overall Scraping Progress by Domain",
      x = "Progress (%)",
      y = "Domain",
      fill = "Status"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
      legend.position = "bottom",
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(face = "bold")
    )
  
  message("\nDisplaying the combined plot...")
  print(combined_plot)
  
  return(overview_table)
}

# Example of how to call the function:
# overview_before_chunk_run <- func_13_generate_progress_report()



# --- Function to Compare Chunk Run Performance ---
func_13_compare_chunk_runs <- function(overview_before, overview_after, chunk_dt) {
  
  # --- 1. Validate Inputs ---
  if (!is.data.table(overview_before) || !is.data.table(overview_after) || !is.data.table(chunk_dt)) {
    stop("All inputs (overview_before, overview_after, chunk_dt) must be data.tables.")
  }
  
  # --- 2. Calculate Totals Before and After ---
  
  # Sum the total scraped links from the overview tables
  scraped_before <- sum(overview_before$`Scraped Links`, na.rm = TRUE)
  scraped_after <- sum(overview_after$`Scraped Links`, na.rm = TRUE)
  
  # Sum the total parse error links from the overview tables
  parse_error_before <- sum(overview_before$`Parse Error Links`, na.rm = TRUE)
  parse_error_after <- sum(overview_after$`Parse Error Links`, na.rm = TRUE)
  
  # --- 3. Calculate Deltas (Changes) during the chunk run ---
  
  total_links_in_chunk <- nrow(chunk_dt)
  newly_scraped <- scraped_after - scraped_before
  new_parse_errors <- parse_error_after - parse_error_before
  
  # Total links that were successfully processed (either scraped or identified as parse error)
  total_processed_in_chunk <- newly_scraped + new_parse_errors
  
  # Calculate the success rate for this specific chunk
  success_rate <- if (total_links_in_chunk > 0) {
    round((total_processed_in_chunk / total_links_in_chunk) * 100, 2)
  } else {
    0 # Avoid division by zero if chunk is empty
  }
  
  # --- 4. Print Formatted Summary to Console ---
  
  message(paste(rep("=", 60), collapse = ""))
  message("--- Chunk Run Performance Summary ---")
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-35s %d", "Total links in chunk:", total_links_in_chunk))
  message(sprintf("%-35s %d", "New links successfully scraped:", newly_scraped))
  message(sprintf("%-35s %d", "New links with parse error:", new_parse_errors))
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("%-35s %.2f%%", "Chunk Success Rate:", success_rate))
  message(paste(rep("=", 60), collapse = ""))
  
  # Return the results invisibly in case they are needed for other purposes
  invisible(list(
    total_links = total_links_in_chunk,
    newly_scraped = newly_scraped,
    new_parse_errors = new_parse_errors,
    success_rate = success_rate
  ))
}
