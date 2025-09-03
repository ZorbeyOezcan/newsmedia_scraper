source("parse_error_inspector.R")
library(paperboy)

####

parse_error_overview <- create_parse_error_overview()


####


# ndr.de

# analyze and load fully 
analyze_parse_error("ndr.de", original = TRUE)

# Load full data 
load_parse_error("ndr.de")

# Sample data
sample_htmls("ndr.de", 10)

# Test sample 
parse_html_local(ndr_sample, force_reload = TRUE)
analyze_parse_error(ndr_sample_parsed_local, original = FALSE)


# Test all 
parse_html_local(ndr_parse_error)
analyze_parse_error(ndr_parse_error_parsed_local, original = FALSE)



# compare to paper boy 
aaa_test <- pb_deliver("https://www.ndr.de/nachrichten/mecklenburg-vorpommern/Internationale-Hacker-sollen-hinter-Cyberangriff-auf-Bergen-stecken,hackerangriff200.html")
# geht auch nicht 

show_parse_rules("ndr.de")

new_ndr_rule_hybrid <- r"(
function(html) {
    # Required libraries
    library(rvest)
    library(lubridate)
    library(jsonlite)
    library(stringr)

    # Initialize variables
    datetime <- as.POSIXct(NA)
    author <- NA_character_
    headline <- NA_character_
    text <- NA_character_

    # Helper function from your framework
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
      datetime <<- datetime_val
      author <<- author_val
      headline <<- headline_val
      text <<- text_val
      invisible(NULL)
    }

    tryCatch({
      # --- STEP 1: Attempt to parse everything from the primary JSON-LD source ---
      json_nodes <- rvest::html_elements(html, 'script[type=\"application/ld+json\"]')
      for (node in json_nodes) {
        json_txt <- rvest::html_text(node)
        json_data <- tryCatch(jsonlite::fromJSON(json_txt, flatten = TRUE), error = function(e) NULL)
        
        if (!is.null(json_data) && ("articleBody" %in% names(json_data) || "headline" %in% names(json_data))) {
          datetime <- lubridate::as_datetime(json_data$datePublished)
          headline <- json_data$headline
          author   <- json_data$author.name
          text     <- json_data$articleBody
          break 
        }
      }
      
      # --- STEP 2: Fallback for Text and Author if they are still missing ---
      # Check if text is NA or an empty string after the JSON attempt
      if (is.null(text) || is.na(text) || nchar(trimws(text)) == 0) {
          text <- html %>%
            rvest::html_elements("article p, article h2") %>%
            rvest::html_text2() %>%
            paste(collapse = "\n")
      }
      
      # Fallback for author
      if (is.null(author) || is.na(author) || nchar(trimws(author)) == 0) {
          author <- html %>%
            rvest::html_element("meta[name='author']") %>%
            rvest::html_attr('content')
      }

      # Use your helper to assign the final values
      s_n_list(datetime, author, headline, text)
      
      # Return the final list
      list(
        datetime = datetime,
        author = as.character(author),
        headline = as.character(headline),
        text = as.character(text)
      )
    }, error = function(e) {
      # Error handling
      list(
        datetime = as.POSIXct(NA),
        author = NA_character_,
        headline = NA_character_,
        text = NA_character_,
        error = paste("Parser execution error:", as.character(e$message))
      )
    })
}
)"

# --- Execute this command to update your local rule ---
edit_parse_rules(
  domain_name = "ndr.de", 
  rule_type = "parser_rules", 
  new_rule = new_ndr_rule_hybrid
)

show_parse_rules("ndr.de")
# reset_rule("ndr.de", "parser_rules")


# test: 
parse_html_local(ndr_sample, force_reload = TRUE)
analyze_parse_error(ndr_sample_parsed_local, original = FALSE)


parse_html_local(ndr_parse_error, force_reload = TRUE)



# Check: 
analyze_parse_error(ndr_parse_error_parsed_local, original = FALSE)


fill_results("ndr_parse_error_parsed_local")


# Change rule 
apply_new_rule("ndr", "parser_rules")

show_parse_rules("ndr.de")










export_html_samples <- function(sample_dt, output_folder = "html_inspection") {
  
  # --- Input Validation ---
  # Check if the input is a data frame or data table
  if (!is.data.frame(sample_dt)) {
    stop("Input 'sample_dt' must be a data.frame or data.table.")
  }
  
  # Check if the required 'html_content' and 'id' columns exist
  if (!"html_content" %in% names(sample_dt) || !"id" %in% names(sample_dt)) {
    stop("The input data table must contain 'html_content' and 'id' columns.")
  }
  
  # --- Directory Setup ---
  # Create the output directory if it doesn't exist
  if (!dir.exists(output_folder)) {
    message(sprintf("Creating output directory: '%s'", output_folder))
    dir.create(output_folder, recursive = TRUE)
  }
  
  message(sprintf("Exporting %d HTML files to folder '%s'...", nrow(sample_dt), output_folder))
  
  # --- Loop and Export ---
  # Iterate over each row of the data table
  for (i in 1:nrow(sample_dt)) {
    # Get the unique ID and the HTML content from the current row
    row_id <- sample_dt$id[i]
    html_content <- sample_dt$html_content[i]
    
    # Create a unique and descriptive filename
    # e.g., "ndr_sample_id_12345.html"
    file_name <- sprintf("ndr_sample_id_%s.html", row_id)
    file_path <- file.path(output_folder, file_name)
    
    # Write the HTML content to the file
    # Using 'useBytes = TRUE' can help prevent encoding issues
    writeLines(html_content, file_path, useBytes = TRUE)
  }
  
  # --- Final Message ---
  message("Export complete.")
  message(sprintf("You can find the files in your R working directory under: '%s'", output_folder))
  
  # Return the path to the folder invisibly
  invisible(normalizePath(output_folder))
}

# --- HOW TO USE ---
# Make sure your 'ndr_sample' data table is loaded in your R environment

# Call the function to export the files
export_html_samples(ndr_sample)








