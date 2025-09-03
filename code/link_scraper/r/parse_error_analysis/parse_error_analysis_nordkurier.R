source("parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()


####


# nordkurier.de

# analyze and load fully 
analyze_parse_error("nordkurier.de", original = TRUE)

# Load full data 
# load_parse_error("nordkurier.de")

# Sample data
sample_htmls("nordkurier.de", 10)

# Test sample 
parse_html_local(nordkurier_sample)
# 

# compare to paper boy 
aaa_test <- pb_deliver("https://www.nordkurier.de/regional/mecklenburg-vorpommern/bundespolizei-in-mv-im-dauerstress-zahl-der-uberstunden-auf-hoechststand-3314784")
# geht auch nicht 

show_parse_rules("nordkurier.de")

neue_nordkurier_regel <- '
function(html) {
    library(rvest); library(lubridate); library(jsonlite); library(stringr)
    datetime <- as.POSIXct(NA); author <- NA_character_; headline <- NA_character_; text <- NA_character_
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
        datetime <<- datetime_val; author <<- author_val; headline <<- headline_val; text <<- text_val; invisible(NULL)
    }
    tryCatch({
        # VERSUCH 1: META TAG (SEHR ZUVERLÄSSIG)
        tryCatch({
            dt_val <- html %>% rvest::html_element("meta[property=\'article:published_time\']") %>% rvest::html_attr("content")
            if (!is.na(dt_val) && nchar(dt_val) > 0) datetime <- lubridate::as_datetime(dt_val)
        }, error = function(e) {})

        # VERSUCH 2: JSON-LD SCRIPT (FALLS META FEHLSCHLÄGT)
        if (is.na(datetime)) {
            tryCatch({
                json_txt <- html %>% rvest::html_elements("script[type=\'application/ld+json\']") %>% rvest::html_text()
                if (length(json_txt) >= 2) {
                    json_df <- jsonlite::fromJSON(json_txt[2], flatten = TRUE)
                    if (!is.null(json_df$datePublished)) datetime <- lubridate::as_datetime(json_df$datePublished)
                    if (is.na(headline) && !is.null(json_df$headline)) headline <- json_df$headline
                    if (is.na(author) && !is.null(json_df$author.name)) author <- toString(json_df$author.name)
                }
            }, error = function(e) {})
        }

        # VERSUCH 3: SICHTBARER TIME-TAG (LETZTER VERSUCH)
        if (is.na(datetime)) {
            tryCatch({
                dt_text <- html %>% rvest::html_element("time.tw-text-neutral-10") %>% rvest::html_text2()
                if (!is.na(dt_text) && nchar(dt_text) > 0) datetime <- lubridate::dmy_hm(dt_text)
            }, error = function(e) {})
        }

        # Extraktion für die restlichen Felder
        text <- html %>% rvest::html_elements("p.paragraph") %>% rvest::html_text2() %>% paste(collapse = "\n")
        if (is.na(headline)) {
          headline <- html %>% rvest::html_element("h1") %>% rvest::html_text2()
        }

        s_n_list(datetime, author, headline, text)
        list(datetime = datetime, author = as.character(author), headline = as.character(headline), text = as.character(text))
    }, error = function(e) {
        list(datetime = as.POSIXct(NA), author = NA_character_, headline = NA_character_, text = NA_character_, error = paste("Parser execution error:", as.character(e$message)))
    })
}
'
  
# Apply 
edit_parse_rules(
  domain_name = "nordkurier.de",
  rule_type = "parser_rules",
  new_rule = neue_nordkurier_regel
)

show_parse_rules("nordkurier.de")
# reset_rule("nordkurier.de", "parser_rules")


# test: 
parse_html_local(nordkurier_sample)

# Compare to paper boy: 
compare_results(nordkurier_sample_parsed_local)
# Perfect 

# now all: 
parse_html_local(nordkurier_parse_error)

# Check: 
analyze_parse_error(nordkurier_parse_error_parsed_local, original = FALSE)



fill_results("nordkurier_parse_error_parsed_local")


# Change rule 
apply_new_rule("nordkurier", "parser_rules")

show_parse_rules("nordkurier.de")

