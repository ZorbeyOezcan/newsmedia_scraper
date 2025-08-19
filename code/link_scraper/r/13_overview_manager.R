# ==============================================================================
# MODULE: 13 - OVERVIEW & VISUALIZATION MANAGER
# ==============================================================================
#
# This module generates a progress overview for the scraping process.
# It aggregates data from the input and final output files to create a
# summary table and a stacked bar chart visualizing the progress for each domain.
#
# RECEIVES FROM:
# - data/input/input.rds
# - data/output/final_data.rds
#
# OUTPUTS TO:
# - Console: A summary data.table of the scraping progress.
# - Plots Pane: A ggplot visualization of the progress.
# - Console: A vector of completed domains for exclusion in subsequent runs.
#
# ==============================================================================

# Load required packages for data manipulation and plotting
library(data.table)
library(ggplot2)

# Helper function to define all necessary paths for the project
if (!exists("get_module_paths")) {
  get_module_paths <- function() {
    # Define the base path for the project directory
    base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
    list(
      input  = file.path(base_path, "data", "input"),
      output = file.path(base_path, "data", "output")
    )
  }
}


# --- Main Function to Generate Progress Report ---
func_13_generate_progress_report <- function() {
  
  # --- 1. Define File Paths ---
  paths <- get_module_paths()
  input_path <- file.path(paths$input, "input.rds")
  output_path <- file.path(paths$output, "final_data.rds")
  
  # --- 2. Load Datasets ---
  message("Loading datasets...")
  if (!file.exists(input_path)) stop("Error: input.rds not found at the specified path!")
  if (!file.exists(output_path)) stop("Error: final_data.rds not found at the specified path!")
  
  input_dt <- readRDS(input_path)
  output_dt <- readRDS(output_path)
  
  # Ensure they are data.tables
  setDT(input_dt)
  setDT(output_dt)
  
  # --- 3. Aggregate and Merge Data ---
  message("Calculating statistics per domain...")
  # Count the total number of links per domain from the input file
  total_counts <- input_dt[, .(total_in = .N), by = domain]
  
  # Count the total number of successfully scraped links per domain from the output file
  success_counts <- output_dt[, .(total_out = .N), by = domain]
  
  # Merge the two counts into a single summary table
  # all.x = TRUE ensures all domains from the input are included, even if none are scraped yet
  summary_dt <- merge(total_counts, success_counts, by = "domain", all.x = TRUE)
  
  # Replace NA with 0 for domains that are in the input but not yet in the output
  summary_dt[is.na(total_out), total_out := 0]
  
  # --- 4. Create and Print Overview Table ---
  overview_table <- summary_dt[, .(
    Domain = domain,
    `Input Links` = total_in,
    `Scraped Links` = total_out
  )]
  overview_table[, `Percent Complete` := round((`Scraped Links` / `Input Links`) * 100, 2)]
  
  # ================================================================ #
  # === MODIFIED LINE: Sort by percentage for the plot visualization ===
  # ================================================================ #
  setorder(overview_table, -`Percent Complete`)
  
  message("\n--- Scraping Progress Overview ---")
  print(overview_table)
  message("----------------------------------\n")
  
  
  # --- 5. Prepare Data for Plotting ---
  summary_dt[, percent_success := (total_out / total_in) * 100]
  summary_dt[, percent_remaining := 100 - percent_success]
  
  # Melt the data into a long format, which is ideal for ggplot's stacked bars
  plot_data <- melt(summary_dt,
                    id.vars = c("domain", "total_in"), # Use total_in for ordering
                    measure.vars = c("percent_success", "percent_remaining"),
                    variable.name = "status",
                    value.name = "percentage")
  
  # Define the order and labels for the legend
  plot_data$status <- factor(plot_data$status,
                             levels = c("percent_remaining", "percent_success"),
                             labels = c("Remaining", "Success"))
  
  # --- 6. Generate a Single Plot for All Domains ---
  message("Generating a combined plot for all domains...")
  
  # Order domains in the plot based on the sorted overview_table (now by percentage)
  # The rev() function ensures that the highest value appears at the top of the plot
  plot_data[, domain := factor(domain, levels = rev(overview_table$Domain))]
  
  combined_plot <- ggplot(plot_data, aes(x = percentage, y = domain, fill = status)) +
    geom_col() + # Creates the stacked bar
    geom_text(aes(label = ifelse(percentage > 5, paste0(round(percentage), "%"), "")),
              position = position_stack(vjust = 0.5),
              size = 3.5,
              color = "white",
              fontface = "bold") +
    scale_fill_manual(values = c("Success" = "#2ca02c", "Remaining" = "#d62728")) +
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
      panel.grid.major.y = element_blank(), # Remove horizontal grid lines for a cleaner look
      panel.grid.minor.x = element_blank(),
      axis.text.y = element_text(face = "bold")
    )
  
  # --- 7. Print the Combined Plot ---
  message("Displaying the combined plot...")
  print(combined_plot)
  
  # --- 8. Generate and Display Exclusion List ---
  # Identify domains that are 100% complete
  completed_domains_vec <- overview_table[`Percent Complete` >= 100, Domain]
  
  if (length(completed_domains_vec) > 0) {
    # dput() creates an easily copy-pastable R object representation
    dput(completed_domains_vec)
    message("------------------------------------------")
  } else {
    message("\n\n--- No domains are 100% complete yet. ---")
  }
  
  invisible(combined_plot)
}

# Example of how to call the function:
# To run this script, ensure 'input.rds' and 'final_data.rds' exist in the correct directories.
# func_13_generate_progress_report()