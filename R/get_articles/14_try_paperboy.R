# ==============================================================================
# MODULE: RETRY MANAGER (PAPERBOY IMPLEMENTATION)
# ==============================================================================
# 
# This script handles a specific retry workflow:
# 1. Selects links for specific domains that failed parsing previously
#    (processed == FALSE, error == FALSE, parse_error == TRUE).
# 2. Creates a new, randomized chunk file following the existing naming convention.
#    -> NOW INCLUDES OPTIONAL MANUAL SIZE LIMIT
# 3. Processes these links sequentially using the 'paperboy' package directly.
# 4. Stores results in a temporary dataframe.
# 5. Validates results (checks text/date) and saves valid entries to final output.
# 6. Cleans duplicates and updates the input status.
#
#
####### Only works, if no articles are paywalled 
#
# ==============================================================================

# Load required packages
library(data.table)
library(paperboy)

# Ensure paths function is available (if not already loaded)
if (!exists("get_module_paths")) {
  get_module_paths <- function() {
    base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper" # Update if needed
    list(
      input = file.path(base_path, "data", "input"),
      output = file.path(base_path, "data", "output"), 
      chunks = file.path(base_path, "data", "input", "chunks")
    )
  }
}

# Source 09_storage_manager.R for cleanup functions if available
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/09_storage_manager.R")

# --- CONFIGURATION: DEFINE DOMAINS & SIZE HERE ---

domains_of_interest <- c(
  "badische-zeitung.de"
)
max_retry_links <- 300


# ==============================================================================
# PART 1: CHUNK CREATION AND RANDOMIZATION
# ==============================================================================

message("\n=== STARTING PART 1: CHUNK CREATION ===")

# 1. Load Input Data
paths <- get_module_paths()
input_file <- file.path(paths$input, "input.rds")

if (!file.exists(input_file)) stop("input.rds not found.")
input_dt <- readRDS(input_file)

# 2. Filter Links
# Logic: Domain match AND Not Processed AND Not Error AND Parse Error is TRUE
filtered_dt <- input_dt[
  domain %in% domains_of_interest & 
    processed == FALSE & 
    error == FALSE & 
    parse_error == FALSE # for now 
]

if (nrow(filtered_dt) == 0) {
  stop("No links found matching the criteria (processed=F, error=F, parse_error=T) for the selected domains.")
}

message(sprintf("Found %d total candidate links for retry.", nrow(filtered_dt)))

# 3. Randomize Order FIRST
set.seed(Sys.time()) # Ensure randomness
filtered_dt <- filtered_dt[sample(.N)]

# 4. Apply Manual Size Limit (if configured)
if (!is.null(max_retry_links) && is.numeric(max_retry_links)) {
  if (nrow(filtered_dt) > max_retry_links) {
    message(sprintf("Limit active: Cutting chunk from %d down to %d links.", nrow(filtered_dt), max_retry_links))
    filtered_dt <- head(filtered_dt, max_retry_links)
  } else {
    message(sprintf("Limit active (%d), but found fewer links (%d). Taking all.", max_retry_links, nrow(filtered_dt)))
  }
} else {
  message("No limit set. Taking all candidate links.")
}

# 5. Determine New Chunk Name (Robust Path Handling)
chunk_dir <- if (!is.null(paths$chunks)) {
  paths$chunks
} else if (!is.null(paths$chunk_inputs)) {
  paths$chunk_inputs
} else {
  file.path(paths$input, "chunks") 
}

if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)

existing_chunks <- list.files(chunk_dir, pattern = "^chunk_\\d+\\.rds$")

if (length(existing_chunks) > 0) {
  chunk_ids <- as.integer(gsub("chunk_|\\.rds", "", existing_chunks))
  next_id <- max(chunk_ids, na.rm = TRUE) + 1
} else {
  next_id <- 1
}

new_chunk_name <- sprintf("chunk_%03d", next_id)
new_chunk_path <- file.path(chunk_dir, paste0(new_chunk_name, ".rds"))

# 6. Save Chunk
saveRDS(filtered_dt, new_chunk_path)
assign(new_chunk_name, filtered_dt, envir = .GlobalEnv)

message(sprintf("Successfully created new chunk: %s with %d links.", new_chunk_name, nrow(filtered_dt)))
message(sprintf("Saved to: %s", new_chunk_path))


# ==============================================================================
# PART 2: PAPERBOY EXECUTION
# ==============================================================================

message("\n=== STARTING PART 2: PROCESSING WITH PAPERBOY ===")

# Initialize list to store results
results_list <- list()
total_links <- nrow(filtered_dt)

# Iterate through links one by one
for (i in 1:total_links) {
  current_url <- filtered_dt$url[i]
  
  message(paste(rep("-", 60), collapse = ""))
  message(sprintf("[%d/%d] Processing: %s", i, total_links, current_url))
  
  # Execute pb_deliver
  result <- tryCatch({
    paperboy::pb_deliver(current_url, verbose = TRUE)
  }, error = function(e) {
    message(sprintf("Error processing %s: %s", current_url, e$message))
    return(NULL)
  })
  
  # Check if we got a valid result
  if (!is.null(result) && is.data.frame(result)) {
    
    # --- STRICT TYPE ENFORCEMENT ---
    cols_to_force_char <- c("url", "expanded_url", "domain", "status", "text", "headline", "author")
    
    for (col in cols_to_force_char) {
      if (col %in% names(result)) {
        result[[col]] <- as.character(result[[col]])
      } else {
        result[[col]] <- NA_character_
      }
    }
    
    if ("datetime" %in% names(result)) {
      if (all(is.na(result$datetime))) {
        result$datetime <- as.POSIXct(NA)
      } else if (!inherits(result$datetime, "POSIXt")) {
        result$datetime <- tryCatch(as.POSIXct(result$datetime), error = function(e) as.POSIXct(NA))
      }
    } else {
      result$datetime <- as.POSIXct(NA)
    }
    
    if ("misc" %in% names(result)) {
      result$misc <- NULL
    }
    
    # Add Metadata
    result$original_url <- as.character(current_url)
    result$retry_chunk <- as.character(new_chunk_name)
    result$timestamp_retry <- Sys.time()
    
    # Store in list
    results_list[[i]] <- result
    
  } else {
    message("--> No result returned.")
  }
  
  # Optional: Small sleep to be polite (even if aggressive)
  Sys.sleep(1.5)
}

# Combine all results into one dataframe (temp_df)
message("\n=== AGGREGATING RESULTS ===")

if (length(results_list) > 0) {
  temp_df <- tryCatch({
    data.table::rbindlist(results_list, fill = TRUE, use.names = TRUE)
  }, error = function(e) {
    message("Critical Error in rbindlist: ", e$message)
    return(data.table())
  })
  
  assign("temp_df", temp_df, envir = .GlobalEnv)
  
  if (nrow(temp_df) > 0) {
    message(sprintf("Finished. 'temp_df' created with %d rows.", nrow(temp_df)))
  } else {
    message("Warning: temp_df is empty despite having results in list.")
  }
  
} else {
  message("Warning: temp_df was not created because no valid results were returned.")
  temp_df <- data.table()
}


# ==============================================================================
# PART 3: VALIDATION AND SAVING TO FINAL OUTPUT
# ==============================================================================

message("\n=== STARTING PART 3: VALIDATION AND SAVING ===")

if (exists("temp_df") && nrow(temp_df) > 0) {
  
  if (!exists("filtered_dt")) stop("filtered_dt is missing. Cannot map IDs.")
  
  dt_results <- as.data.table(temp_df)
  
  mapped_results <- merge(
    dt_results, 
    filtered_dt[, .(id, domain, url)], 
    by.x = "original_url", 
    by.y = "url", 
    all.x = TRUE,
    sort = FALSE,
    suffixes = c(".paperboy", ".input")
  )
  
  if (!"text" %in% names(mapped_results)) mapped_results[, text := NA_character_]
  if (!"datetime" %in% names(mapped_results)) mapped_results[, datetime := as.POSIXct(NA)]
  if (!"author" %in% names(mapped_results)) mapped_results[, author := NA_character_]
  if (!"headline" %in% names(mapped_results)) mapped_results[, headline := NA_character_]
  
  valid_rows <- mapped_results[
    !is.na(text) & text != "" & 
      !is.na(datetime)
  ]
  
  message(sprintf("Validation: %d valid articles found out of %d attempts.", nrow(valid_rows), nrow(dt_results)))
  
  if (nrow(valid_rows) > 0) {
    
    get_col <- function(dt, possible_names) {
      for (name in possible_names) {
        if (name %in% names(dt)) return(dt[[name]])
      }
      return(rep(NA, nrow(dt))) 
    }
    
    final_entries <- data.table(
      id                = as.integer(valid_rows$id),
      domain            = as.character(get_col(valid_rows, c("domain.input", "domain", "domain.paperboy"))), 
      url               = as.character(valid_rows$original_url),
      timestamp_scraped = Sys.time(),                    
      date_time         = as.POSIXct(valid_rows$datetime), 
      author            = as.character(valid_rows$author),
      headline          = as.character(valid_rows$headline),
      text              = as.character(valid_rows$text),
      paywall           = FALSE                          
    )
    
    final_output_path <- file.path(paths$output, "final_data.rds")
    
    if (file.exists(final_output_path)) {
      existing_final <- readRDS(final_output_path)
      updated_final <- rbind(existing_final, final_entries, fill = TRUE)
      saveRDS(updated_final, final_output_path)
      message(sprintf("SUCCESS: Appended %d rows to final_data.rds", nrow(final_entries)))
      
    } else {
      warning("final_data.rds not found. Creating new file.")
      saveRDS(final_entries, final_output_path)
      message(sprintf("SUCCESS: Created final_data.rds with %d rows", nrow(final_entries)))
    }
    
  } else {
    message("No rows met the validation criteria. Nothing saved to final output.")
  }
  
} else {
  message("temp_df is empty. Skipping validation and saving.")
}


# ==============================================================================
# PART 4: CLEANUP AND UPDATES
# ==============================================================================

message("\n=== STARTING PART 4: CLEANUP ===")

if (exists("func_09_clean_dupes")) {
  message("Running func_09_clean_dupes()...")
  func_09_clean_dupes()
} else {
  warning("func_09_clean_dupes function not found. Did you source 09_storage_manager.R?")
}

if (exists("func_09_update_input")) {
  message("Running func_09_update_input()...")
  func_09_update_input()
} else {
  warning("func_09_update_input function not found. Did you source 09_storage_manager.R?")
}

message(paste(rep("=", 60), collapse = ""))
message("RETRY WORKFLOW FINISHED SUCCESSFULLY.")
message(paste(rep("=", 60), collapse = ""))


# ==============================================================================
# PART 5: REPORT FAILED LINKS
# ==============================================================================

message("\n=== STARTING PART 5: REPORTING FAILED LINKS ===")

if (exists("valid_rows") && nrow(valid_rows) > 0) {
  successful_urls <- unique(valid_rows$original_url)
} else {
  successful_urls <- character(0)
}

# Only check failed links from the CURRENT filtered batch
if (exists("filtered_dt") && nrow(filtered_dt) > 0) {
  all_attempted_urls <- unique(filtered_dt$url)
} else {
  all_attempted_urls <- character(0)
}

failed_urls <- setdiff(all_attempted_urls, successful_urls)

if (length(failed_urls) > 0) {
  message(sprintf("\nFound %d failed links.", length(failed_urls)))
  message("Copy the lines below for your next script:\n")
  message("-------------------------------------------------------")
  for (url in failed_urls) {
    cat(sprintf('"%s",\n', url))
  }
  message("-------------------------------------------------------")
} else {
  message("\nGreat success! 100% of links in this chunk were scraped and validated.")
}

