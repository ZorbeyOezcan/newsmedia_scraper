library(data.table)
library(rvest)

# Main function to re-process parse errors for a specific domain
reprocess_parse_errors <- function(domain_name_with_tld) {
  
  # Use the full domain name for file paths, and a cleaned version for rule lookups
  domain_name <- sub("\\..*$", "", domain_name_with_tld)
  
  message(paste(rep("=", 60), collapse = ""))
  message(sprintf("--- Starting re-processing for domain: %s ---", domain_name_with_tld))
  message(paste(rep("=", 60), collapse = ""))
  
  # --- 1. SETUP AND PATH DEFINITION ---
  paths <- get_module_paths()
  # Use the full domain name with TLD to construct the file path
  parse_error_file <- file.path(paths$parse_error, paste0(domain_name_with_tld, "_parse_error.rds"))
  final_data_file <- file.path(paths$output, "final_data.rds")
  input_file <- file.path(paths$input, "input.rds")
  
  # --- 2. LOAD PARSE ERROR DATA ---
  if (!file.exists(parse_error_file)) {
    warning(sprintf("Parse error file not found for '%s' at path: %s. Aborting.", domain_name_with_tld, parse_error_file))
    return(invisible(FALSE))
  }
  parse_error_dt <- readRDS(parse_error_file)
  
  if (!is.data.table(parse_error_dt) || nrow(parse_error_dt) == 0) {
    message("Parse error file is empty. Nothing to re-process.")
    return(invisible(TRUE))
  }
  message(sprintf("Found %d entries to re-process.", nrow(parse_error_dt)))
  
  # --- 3. LOAD PARSING RULES ---
  parser_rules_path <- file.path(paths$parsing_config, "06_parser_rules_fetched.rds")
  if (!file.exists(parser_rules_path)) {
    stop("FATAL: Parser rule file not found. Run setup_parser_rules() first.")
  }
  parser_rules <- readRDS(parser_rules_path)
  # Use the cleaned domain name for the rule lookup
  parser_entry <- parser_rules[[domain_name]]
  
  if (is.null(parser_entry) || !isTRUE(parser_entry$success)) {
    stop(sprintf("No valid parsing function found for domain '%s'.", domain_name))
  }
  parsing_func <- parser_entry$parse_function
  
  # --- 4. INITIALIZE CONTAINERS ---
  successful_reparse <- list()
  failed_reparse <- list()
  
  # --- 5. LOOP AND RE-PROCESS EACH ENTRY ---
  for (i in 1:nrow(parse_error_dt)) {
    row_data <- parse_error_dt[i, ]
    message(sprintf("[%d/%d] Re-parsing: %s", i, nrow(parse_error_dt), row_data$url))
    
    html_content <- row_data$html_content
    if (is.na(html_content) || nchar(html_content) == 0) {
      failed_reparse[[length(failed_reparse) + 1]] <- row_data
      next
    }
    
    html_parsed <- tryCatch(rvest::read_html(html_content), error = function(e) NULL)
    if (is.null(html_parsed)) {
      failed_reparse[[length(failed_reparse) + 1]] <- row_data
      next
    }
    
    # Apply the parsing function
    extracted_data <- parsing_func(html_parsed)
    
    # Create a temporary data.table with the same structure as the final output
    temp_dt <- data.table(
      id = row_data$id,
      domain = row_data$domain,
      url = row_data$url,
      timestamp_scraped = row_data$timestamp_scraped,
      date_time = as.POSIXct(NA),
      author = NA_character_,
      headline = NA_character_,
      text = NA_character_,
      paywall = row_data$paywall
    )
    
    # Populate with parsed data, ensuring values are sanitized
    if (!is.null(extracted_data$datetime) && !all(is.na(extracted_data$datetime))) {
      temp_dt$date_time <- extracted_data$datetime[1]
    }
    temp_dt$author   <- .sanitize_value(extracted_data$author)
    temp_dt$headline <- .sanitize_value(extracted_data$headline)
    temp_dt$text     <- .sanitize_value(extracted_data$text)
    
    # --- APPLY SPECIAL RULE FOR augsburger-allgemeine.de ---
    if (domain_name == "augsburger-allgemeine" &&
        !is.na(temp_dt$headline) && !is.na(temp_dt$date_time) &&
        (is.na(temp_dt$text) || nchar(trimws(temp_dt$text)) == 0) &&
        (is.na(temp_dt$author) || nchar(trimws(temp_dt$author)) == 0)) {
      
      message("  -> Applying special dpa-rule for 'augsburger-allgemeine'.")
      temp_dt[, text := headline]
      temp_dt[, headline := NA_character_]
      temp_dt[, author := "dpa"]
    }
    
    # --- 6. VALIDATE AND ROUTE THE RESULT ---
    if (!is.na(temp_dt$date_time) && !is.na(temp_dt$text) && nchar(trimws(temp_dt$text)) > 0) {
      successful_reparse[[length(successful_reparse) + 1]] <- temp_dt
    } else {
      failed_reparse[[length(failed_reparse) + 1]] <- row_data
    }
  }
  
  # --- 7. SAVE RESULTS ---
  # Handle successful reparses
  if (length(successful_reparse) > 0) {
    success_dt <- rbindlist(successful_reparse, use.names = TRUE, fill = TRUE)
    message(sprintf("\nSuccessfully re-parsed %d articles. Appending to final output.", nrow(success_dt)))
    
    # Append to final_data.rds
    final_dt <- readRDS(final_data_file)
    updated_final_dt <- rbindlist(list(final_dt, success_dt), use.names = TRUE, fill = TRUE)
    saveRDS(updated_final_dt, final_data_file)
    
    # Update input.rds
    input_dt <- readRDS(input_file)
    input_dt[url %in% success_dt$url, `:=`(processed = TRUE, parse_error = FALSE)]
    saveRDS(input_dt, input_file)
    message("Master 'input.rds' and 'final_data.rds' have been updated.")
  } else {
    message("\nNo articles could be successfully re-parsed.")
  }
  
  # Handle failed reparses
  if (length(failed_reparse) > 0) {
    failed_dt <- rbindlist(failed_reparse, use.names = TRUE, fill = TRUE)
    message(sprintf("%d articles still failed. Updating the parse error file.", nrow(failed_dt)))
    saveRDS(failed_dt, parse_error_file)
  } else {
    message("All articles were successfully re-parsed. Clearing the parse error file.")
    # Save an empty data.table with the same structure
    empty_dt <- parse_error_dt[0, ]
    saveRDS(empty_dt, parse_error_file)
  }
  
  message(paste(rep("=", 60), collapse = ""))
  message("--- Re-processing finished. ---")
  message(paste(rep("=", 60), collapse = ""))
  
  return(invisible(TRUE))
}

# Helper function to sanitize extracted values (local to this script)
.sanitize_value <- function(value) {
  if (is.null(value) || length(value) == 0 || all(is.na(value))) return(NA_character_)
  value <- value[1] # Always take the first element
  return(as.character(value))
}

reprocess_parse_errors("augsburger-allgemeine.de")


func_09_update_input()
