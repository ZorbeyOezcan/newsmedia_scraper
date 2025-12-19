library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")

####

overview <- create_parse_error_overview()


####


# nordkurier.de

# analyze and load fully 
analyze_parse_error("nordkurier.de", original = TRUE)

# Load full data 
# load_parse_error("nordkurier.de")

show_parse_rules("nordkurier.de")

neue_nordkurier_regel <- '
function(html) {
    library(rvest); library(lubridate); library(jsonlite); library(stringr)
    
    # Initiale Variablen
    datetime <- as.POSIXct(NA); author <- NA_character_; headline <- NA_character_; text <- NA_character_
    
    # Helper Scope Function
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
        datetime <<- datetime_val; author <<- author_val; headline <<- headline_val; text <<- text_val; invisible(NULL)
    }

    tryCatch({
        # --- VERSUCH 1: META TAGS (Datum) ---
        if (is.na(datetime)) {
            tryCatch({
                dt_val <- html %>% rvest::html_element("meta[property=\'article:published_time\']") %>% rvest::html_attr("content")
                if (!is.na(dt_val) && nchar(dt_val) > 0) datetime <- lubridate::as_datetime(dt_val)
            }, error = function(e) {})
        }

        # --- VERSUCH 2: JSON-LD (Iterativ) ---
        tryCatch({
            json_scripts <- html %>% rvest::html_elements("script[type=\'application/ld+json\']") %>% rvest::html_text()
            
            for (j_txt in json_scripts) {
                tryCatch({
                    json_df <- jsonlite::fromJSON(j_txt, flatten = TRUE)
                    
                    if (is.na(datetime) && !is.null(json_df$datePublished)) {
                        datetime <- lubridate::as_datetime(json_df$datePublished)
                    }
                    
                    if (is.na(author)) {
                        if (!is.null(json_df$author.name)) {
                            author <- toString(json_df$author.name)
                        } else if (!is.null(json_df$author) && is.data.frame(json_df$author) && !is.null(json_df$author$name)) {
                             author <- toString(json_df$author$name)
                        }
                    }
                    
                    if (is.na(headline) && !is.null(json_df$headline)) headline <- json_df$headline

                }, error = function(e) {})
                if (!is.na(datetime) && !is.na(author)) break
            }
        }, error = function(e) {})

        # --- VERSUCH 3: HTML SELEKTOR (Fallback Datum) ---
        if (is.na(datetime)) {
            tryCatch({
                time_node <- html %>% rvest::html_element("time[aria-label*=\'Veröffentlicht\']")
                if (length(time_node) > 0) {
                    dt_text <- rvest::html_text2(time_node)
                    if (!is.na(dt_text) && nchar(dt_text) > 0) {
                        datetime <- lubridate::dmy_hm(dt_text, tz = "Europe/Berlin")
                    }
                }
            }, error = function(e) {})
        }

        # --- VERSUCH 4: HTML SELEKTOR (Fallback Author - NEU) ---
        # Basiert auf deinem Screenshot: Link enthält "/autoren/"
        if (is.na(author)) {
            tryCatch({
                # Suche nach einem Link, der auf ein Autorenprofil verweist
                author_node <- html %>% rvest::html_element("a[href*=\'/autoren/\']")
                
                if (length(author_node) > 0) {
                    author_text <- rvest::html_text2(author_node)
                    if (!is.na(author_text) && nchar(author_text) > 0) {
                        author <- author_text
                    }
                }
                
                # Fallback: Suche nach Text nach "Von:" (falls kein Link existiert)
                if (is.na(author)) {
                     # XPath: Suche span mit "Von:", nimm das folgende Element
                     author_node_alt <- html %>% rvest::html_element(xpath = "//span[contains(text(), \'Von:\')]/following-sibling::*[1]")
                     if (length(author_node_alt) > 0) {
                        author <- rvest::html_text2(author_node_alt)
                     }
                }
            }, error = function(e) {})
        }

        # --- EXTRAKTION RESTLICHE FELDER ---
        if (is.na(text)) {
            text <- html %>% rvest::html_elements("p.paragraph") %>% rvest::html_text2() %>% paste(collapse = "\n")
        }
        if (is.na(headline)) {
            headline <- html %>% rvest::html_element("h1") %>% rvest::html_text2()
        }
        if (is.na(author)) { # Letzter Versuch Meta
             author_meta <- html %>% rvest::html_element("meta[name=\'author\']") %>% rvest::html_attr("content")
             if (!is.na(author_meta)) author <- author_meta
        }

        s_n_list(datetime, author, headline, text)
        list(datetime = datetime, author = as.character(author), headline = as.character(headline), text = as.character(text))
        
    }, error = function(e) {
        list(datetime = as.POSIXct(NA), author = NA_character_, headline = NA_character_, text = NA_character_, error = paste("Parser execution error:", as.character(e$message)))
    })
}
'

edit_parse_rules(domain_name = "nordkurier.de", rule_type = "parser_rules", new_rule = neue_nordkurier_regel)


show_parse_rules("nordkurier.de")

# load full parse error 
load_parse_error("nordkurier.de")

sample_htmls("nordkurier.de", 10)

# Test sample 
parse_html_local(nordkurier_sample)

# check results 
analyze_parse_error(nordkurier_sample_parsed_local, original = FALSE)
# perffff



# parse local 
parse_html_local(nordkurier_parse_error)

analyze_parse_error(nordkurier_parse_error_parsed_local, original = FALSE)
# perffff# perffff# perffff 

# fill results
fill_results("nordkurier_parse_error_parsed_local")

# apply 
apply_new_rule("nordkurier.de", "parser_rules")




