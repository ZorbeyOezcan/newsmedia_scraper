library(paperboy)

####

overview <- create_parse_error_overview()

####

# badische-zeitung.de

# analyze and load fully 
analyze_parse_error("badische-zeitung.de", original = TRUE)

# Load full data 
load_parse_error("badische-zeitung.de")

# Test 
parse_html_local(`badische-zeitung_parse_error`)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://www.badische-zeitung.de/news/2025-02/09/autofahrer-prallt-gegen-baum-und-stirbt")
# works 


show_parse_rules("badische-zeitung.de")

new_rule_badische-zeitung <- '
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
edit_parse_rules("badische-zeitung.de", "parser_rules", new_rule_badische-zeitung)
show_parse_rules("badische-zeitung.de")
# reset_rule("badische-zeitung.de", "parser_rules")

# test: 
parse_html_local(badische-zeitung_sample)

# Compare to paper boy: 
compare_results(badische-zeitung_sample_parsed_local)
# Perfect 

# now all: 
parse_html_local(badische-zeitung_parse_error)

# Check: 
analyze_parse_error(badische-zeitung_parse_error_parsed_local, original = FALSE)



fill_results("badische-zeitung_parse_error_parsed_local")


# Change rule 
apply_new_rule("badische-zeitung", "parser_rules")

show_parse_rules("badische-zeitung.de")

