source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()

####

# Zeit.de

# analyze and load fully 
analyze_parse_error("zeit.de", original = TRUE)

# Load full data 
load_parse_error("zeit.de")

# Sample data
sample_htmls("zeit.de", 10)

# Test sample 
parse_html_local(zeit_sample)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://www.zeit.de/news/2025-02/09/autofahrer-prallt-gegen-baum-und-stirbt")
# works 


show_parse_rules("zeit.de")

new_rule_zeit <- '
function(html) {
    library(rvest); library(lubridate); library(jsonlite); library(stringr)
    datetime <- as.POSIXct(NA); author <- NA_character_; headline <- NA_character_; text <- NA_character_
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
        datetime <<- datetime_val; author <<- author_val; headline <<- headline_val; text <<- text_val; invisible(NULL)
    }
    tryCatch({
        dt_val <- html %>%
            rvest::html_element(".metadata__date>time") %>%
            rvest::html_attr("datetime")

        if (is.na(dt_val) || dt_val == "") {
            dt_val <- html %>%
                rvest::html_element("meta[name=\\"date\\"]") %>%
                rvest::html_attr("content")
        }
        
        if (!is.na(dt_val)) {
            datetime <- lubridate::as_datetime(dt_val)
        }

        headline <- html %>%
            rvest::html_element("[property=\\"og:title\\"]") %>%
            rvest::html_attr("content")

        author <- html %>%
            rvest::html_element("[rel=\\"author\\"],.metadata__source") %>%
            rvest::html_text2() %>%
            toString()

        text <- html %>%
            rvest::html_elements(".article-body p") %>%
            rvest::html_text2() %>%
            paste(collapse = "\n")

        s_n_list(datetime, author, headline, text)
        list(datetime = datetime, author = as.character(author), headline = as.character(headline), text = as.character(text))
    }, error = function(e) {
        list(datetime = as.POSIXct(NA), author = NA_character_, headline = NA_character_, text = NA_character_, error = paste("Parser execution error:", as.character(e$message)))
    })
}
'

# Apply 
edit_parse_rules("zeit.de", "parser_rules", new_rule_zeit)
show_parse_rules("zeit.de")
# reset_rule("zeit.de", "parser_rules")

# test: 
parse_html_local(zeit_sample)

# Compare to paper boy: 
compare_results(zeit_sample_parsed_local)
# Perfect 

# now all: 
parse_html_local(zeit_parse_error)

# Check: 
analyze_parse_error(zeit_parse_error_parsed_local, original = FALSE)



fill_results("zeit_parse_error_parsed_local")


# Change rule 
apply_new_rule("zeit", "parser_rules")

show_parse_rules("zeit.de")

#### cleanup of "live" links: 

# All links that contain "Liveblog" contain no content
# 
# this is code for printing all those fautly links into the console, to put them in "Data Cleaner"
zeit_parse_error <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/zeit.de_parse_error.rds")

# Filter URLs that contain "liveblog" (anywhere in the string)
liveblog_urls <- zeit_parse_error[grepl("live", url, ignore.case = TRUE), url]

# 3. Print the results to the console
cat(paste0('"', liveblog_urls, '",'), sep = "\n")

# the rest are videos: 
# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', zeit_parse_error$url, '",'), sep = "\n")
