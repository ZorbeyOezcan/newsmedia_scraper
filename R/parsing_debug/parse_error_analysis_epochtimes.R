library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")


overview <- create_parse_error_overview()

####
setup_local_rules()

# epochtimes.de

# Load full data 
load_parse_error("epochtimes.de")

aaa_test <- pb_deliver("https://www.epochtimes.de/gesellschaft/blaulicht-kategorie/gewalttat-in-brandenburg-zwei-tote-und-zwei-schwerverletzte-a5002886.html")
# Sample data
sample_htmls("epochtimes.de", 30)

# Test sample 
parse_html_local(epochtimes_sample)
# all wrong


new_epoch_rules <- list(
  has_paywall = TRUE,
  paywall_markers = list(
    css_selectors = c(
      "div.template-wrapper",
      "div.signup-form-wrapper",
      "div.poll-wrapper",
      "div.intro-text",
      "form#email-form",
      "div.poll-result",
      "input.email-input",
      "div.already-user"
    ),
    # Keywords found in the overlay text
    text_keywords = c(
      "Sofort weiterlesen als Newsletter-Empfänger",
      "Jetzt kostenfrei anmelden und Artikel lesen",
      "Mit der Anmeldung erklären Sie sich einverstanden",
      "Complete our survey"
    )
  )
)

# 4. Apply the new rule to the LOCAL configuration
# This allows us to test it without breaking the main scraper immediately
edit_parse_rules(
  domain_name = "epochtimes",
  rule_type = "paywall_rules",
  new_rule = new_epoch_rules
)



show_parse_rules("epochtimes.de")

new_epoch_parser_code <- '
function(html) {
  library(rvest)
  library(lubridate)
  library(jsonlite)
  library(stringr)

  # --- Helper Function (Must be included inside) ---
  html_search <- function(html, selectors, attributes = NULL, all = TRUE, n = 1L) {
    if (all) {
        res <- rvest::html_elements(html, paste0(selectors, collapse = ","))
    } else {
        res <- NULL
        i <- 1L
        l <- length(selectors)
        while (length(res) < 1 && i <= l) {
            res <- rvest::html_elements(html, selectors[i])
            i <- i + 1
        }
    }
    
    want_text <- "text" %in% attributes
    if (want_text) attributes <- setdiff(attributes, "text")
    
    out <- rvest::html_attrs(res) %>%
        unlist(recursive = FALSE) %>%
        subset(., names(.) %in% attributes) %>%
        unname()
    
    if (want_text) out <- c(out, rvest::html_text2(res))
    
    if (is.null(out)) return(NA_character_)
    return(utils::head(out, n))
  }

  # --- Extraction Logic ---
  
  # Initialize variables
  datetime <- as.POSIXct(NA)
  author <- NA_character_
  headline <- NA_character_
  text <- NA_character_
  
  # 1. METADATA via JSON-LD (Primary method for Author/Date/Headline)
  tryCatch({
    json_scripts <- rvest::html_elements(html, "script[type=\'application/ld+json\']")
    if (length(json_scripts) > 0) {
       for (js in json_scripts) {
          json_text <- rvest::html_text(js)
          json_data <- tryCatch(jsonlite::fromJSON(json_text), error = function(e) NULL)
          
          # Check if valid article JSON
          if (!is.null(json_data) && 
              (isTRUE(json_data[["@type"]] == "NewsArticle") || 
               isTRUE(json_data[["@type"]] == "Article") ||
               "headline" %in% names(json_data))) {
               
               if (!is.null(json_data$datePublished)) datetime <- lubridate::as_datetime(json_data$datePublished)
               if (!is.null(json_data$headline)) headline <- json_data$headline
               
               if (!is.null(json_data$author)) {
                  if (is.list(json_data$author) && "name" %in% names(json_data$author)) {
                    author <- paste(json_data$author$name, collapse = ", ")
                  } else if (is.character(json_data$author)) {
                    author <- json_data$author
                  }
               }
               # If we found data, break (assuming first valid article JSON is correct)
               if (!is.na(author) || !is.na(headline)) break
          }
       }
    }
  }, error = function(e) NULL)

  # 2. CSS FALLBACKS (If JSON missing)
  tryCatch({
      if (is.na(datetime)) {
        dt_str <- html_search(html, c("meta[property=\'article:published_time\']", "meta[name=\'date\']", "time"), attributes = "content", all = FALSE)
        if (!is.na(dt_str)) datetime <- as.POSIXct(dt_str)
      }

      if (is.na(headline)) {
        headline <- html_search(html, c("h1", ".article-title", "header h1"), attributes = "text", all = FALSE)
      }

      if (is.na(author)) {
        author <- html_search(html, c("meta[name=\'author\']", ".author-name", ".byline", "a[rel=\'author\']"), attributes = "content", all = FALSE)
        if (is.na(author)) author <- html_search(html, c(".author-name", ".byline", "a[rel=\'author\']", "div[class*=\'author\']"), attributes = "text", all = FALSE)
      }
  }, error = function(e) NULL)

  # 3. TEXT (Custom Logic for Epoch Times)
  # Target specific container class: "mb-5 xl:mb-[15px]"
  # CSS Selector note: Special characters like ":" and "[]" must be escaped with 4 backslashes inside this string definition.
  # We grab ALL matching containers (n=999) and join them.
  tryCatch({
      text_paragraphs <- html_search(html, c("div.mb-5.xl\\\\:mb-\\\\[15px\\\\]"), attributes = "text", all = TRUE, n = 999)
      
      if (length(text_paragraphs) > 0 && !all(is.na(text_paragraphs))) {
         text <- paste(text_paragraphs, collapse = " \\n ")
      } else {
         # Fallback: Try old selectors if the new class structure is missing
         text <- html_search(html, c("div.template-wrapper", "div.post_content", "div.article-content", "article"), attributes = "text", all = FALSE)
      }
  }, error = function(e) { text <- NA_character_ })

  list(
    datetime = datetime,
    author = as.character(author),
    headline = as.character(headline),
    text = as.character(text)
  )
}
'

# Apply the parser rule to LOCAL config
edit_parse_rules(
  domain_name = "epochtimes",
  rule_type = "parser_rules",
  new_rule = new_epoch_parser_code
)

# test: 
parse_html_local(epochtimes_sample, force_reload = TRUE)
# perf

# check results 
analyze_parse_error(epochtimes_sample_parsed_local, original = FALSE)
# perffff



# Load full data 
load_parse_error("epochtimes.de")

# parse local 
parse_html_local(epochtimes_parse_error)

analyze_parse_error(epochtimes_parse_error_parsed_local, original = FALSE)
# perffff# perffff# perffff 

fill_results("epochtimes_parse_error_parsed_local")

# Change rule 
apply_new_rule("epochtimes", "parser_rules")

show_parse_rules("epochtimes.de")

