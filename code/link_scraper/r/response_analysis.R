# ==============================================================================
# SCRIPT: ANALYZE SUCCESS FACTORS (v3 - analyze by chunk)
# ==============================================================================
#
# This script analyzes the request and response logs to determine which
# scraping settings (IP, user agent, etc.) and which chunk run have the
# highest success rate.
#
# Success is defined as an HTTP response with a status code of 200.
#
# v3 Update: Added analysis for success rate by chunk.
#
# ==============================================================================

# --- 1. SETUP ---
# Load required libraries
library(data.table)
library(ggplot2)

# Define the path helper function, just like in your main script
get_module_paths <- function() {
  base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"
  list(
    logs = file.path(base_path, "data", "logs")
  )
}

# --- 2. LOAD DATA ---
message("Loading log files...")
paths <- get_module_paths()

response_log_path <- file.path(paths$logs, "response_log.rds")
request_log_path <- file.path(paths$logs, "request_log.rds")

# Validate that log files exist
if (!file.exists(response_log_path)) {
  stop("FATAL: response_log.rds not found at: ", response_log_path)
}
if (!file.exists(request_log_path)) {
  stop("FATAL: request_log.rds not found at: ", request_log_path)
}

# Read the data.tables
response_dt <- readRDS(response_log_path)
request_dt <- readRDS(request_log_path)

message(sprintf("Loaded %d responses and %d requests.", nrow(response_dt), nrow(request_dt)))


# --- 3. PREPARE DATA ---
message("Merging and preparing data for analysis...")

# FIX: De-duplicate the request log to ensure each request_id is unique.
message(sprintf("De-duplicating request log. Original rows: %d", nrow(request_dt)))
unique_request_dt <- unique(request_dt, by = "request_id")
message(sprintf("Request log rows after de-duplication: %d", nrow(unique_request_dt)))


# Perform the merge using the de-duplicated request log.
merged_dt <- merge(response_dt, unique_request_dt, by = "request_id", all.x = TRUE)

# Remove entries where the merge might have failed (no matching request)
merged_dt <- na.omit(merged_dt, cols = "url.y")


# Define what a "successful" request is. In our case, it's a 200 OK status code.
successful_requests <- merged_dt[status_code == 200]

message(sprintf("Total requests analyzed: %d", nrow(merged_dt)))
message(sprintf("Total successful (200 OK) requests: %d", nrow(successful_requests)))


# --- 4. ANALYSIS FUNCTION ---
# This helper function avoids code repetition. It calculates the success rate for any given column.
analyze_factor <- function(factor_name) {
  
  # Count successful requests for each level of the factor
  success_counts <- successful_requests[, .(successful = .N), by = factor_name]
  
  # Count total requests for each level of the factor
  total_counts <- merged_dt[, .(total = .N), by = factor_name]
  
  # Combine success and total counts
  analysis_dt <- merge(success_counts, total_counts, by = factor_name, all = TRUE)
  
  # Replace NA in 'successful' with 0 (for factors that never had a success)
  analysis_dt[is.na(successful), successful := 0]
  
  # Calculate the success rate
  analysis_dt[, success_rate := successful / total]
  
  # Order the results from best to worst
  setorder(analysis_dt, -success_rate, -total)
  
  return(analysis_dt)
}


# --- 5. EXECUTE ANALYSIS ---
message("\n--- Analyzing Success by IP Address ---")
ip_analysis <- analyze_factor("ip_address.x")
print(ip_analysis)

message("\n--- Analyzing Success by User Agent ID ---")
ua_analysis <- analyze_factor("user_agent_id.x")
print(ua_analysis)

message("\n--- Analyzing Success by Aggressiveness Level ---")
aggressiveness_analysis <- analyze_factor("aggressiveness_level")
print(aggressiveness_analysis)

message("\n--- Analyzing Success by Browser Type ---")
browser_analysis <- analyze_factor("browser_type")
print(browser_analysis)

# NEW: Analysis by chunk
message("\n--- Analyzing Success by Chunk ---")
# The chunk number is in the 'from_chunk.x' column from the response log
chunk_analysis <- analyze_factor("from_chunk.x")
# Make the output prettier by formatting the chunk number
chunk_analysis[, chunk_name := sprintf("chunk_%03d", from_chunk.x)]
print(chunk_analysis)


# --- 6. IDENTIFY "GOLDEN COMBINATION" ---
message("\n--- ======================================== ---")
message("---      THE MOST SUCCESSFUL SETTINGS      ---")
message("--- ======================================== ---\n")

# Get the top performer from each category
best_ip <- ip_analysis[1]
best_ua <- ua_analysis[1]
best_aggro <- aggressiveness_analysis[1]
best_browser <- browser_analysis[1]
best_chunk <- chunk_analysis[1] # NEW

message(sprintf("Best IP Address: %s (Success Rate: %.2f%% over %d requests)",
                best_ip$ip_address.x, best_ip$success_rate * 100, best_ip$total))

message(sprintf("Best User Agent ID: %d (Success Rate: %.2f%% over %d requests)",
                best_ua$user_agent_id.x, best_ua$success_rate * 100, best_ua$total))

message(sprintf("Best Aggressiveness Level: %d (Success Rate: %.2f%% over %d requests)",
                best_aggro$aggressiveness_level, best_aggro$success_rate * 100, best_aggro$total))

message(sprintf("Best Browser Type: %s (Success Rate: %.2f%% over %d requests)",
                best_browser$browser_type, best_browser$success_rate * 100, best_browser$total))

# NEW: Print best chunk
message(sprintf("Best Chunk Run: %s (Success Rate: %.2f%% over %d requests)",
                best_chunk$chunk_name, best_chunk$success_rate * 100, best_chunk$total))


# --- 7. VISUALIZATION ---
message("\n--- Generating plot for top 10 IP addresses ---")

# Take the top 10 IPs for the plot to keep it readable
top_10_ips <- head(ip_analysis, 10)

# Create the plot
ip_plot <- ggplot(top_10_ips, aes(x = reorder(ip_address.x, -success_rate), y = success_rate)) +
  geom_bar(stat = "identity", fill = "#0072B2", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.1f%%\n(%d total)", success_rate * 100, total)),
            vjust = -0.5, color = "black", size = 3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title = "Top 10 Most Successful IP Addresses",
    subtitle = "Success Rate = (Status 200 Responses) / (Total Responses)",
    x = "IP Address",
    y = "Success Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Save the plot to the logs directory
plot_path <- file.path(paths$logs, "ip_success_rate_analysis.png")
ggsave(plot_path, ip_plot, width = 10, height = 7, dpi = 150)

message(sprintf("\nPlot saved successfully to: %s", plot_path))
message("\n--- Analysis Complete ---")