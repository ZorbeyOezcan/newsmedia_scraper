library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")



####

overview <- create_parse_error_overview()

####

# bild.de

# analyze and load fully 
analyze_parse_error("bild.de", original = TRUE)

sample_htmls("bild.de", 30)



# Load full data 
load_parse_error("bild.de")

# Test 
parse_html_local(`bild_sample`)


# compare to paper boy 
aaa_test <- pb_deliver("https://www.bild.de/sport/fussball/bochum-bvb-kovac-erklaert-warum-die-dortmund-stars-nach-der-pleite-schweigen-67b0d986a918eb195a71cb8e#fromWall")
# works 


show_parse_rules("bild.de")

new_bild_rule <- 'function(html) {
    library(rvest)
    library(lubridate)
    library(jsonlite)
    library(stringr)

    datetime <- as.POSIXct(NA)
    author <- NA_character_
    headline <- NA_character_
    text <- NA_character_

    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
        datetime <<- datetime_val
        author <<- author_val
        headline <<- headline_val
        text <<- text_val
        invisible(NULL)
    }

    tryCatch({
        # --- STRATEGIE 1: JSON-LD (PRIORITÄT) ---
        json_nodes <- html %>% rvest::html_elements("script[type=\\"application/ld+json\\"]")
        
        if (length(json_nodes) > 0) {
            json_texts <- rvest::html_text(json_nodes)
            
            # Wir iterieren durch alle gefundenen JSON-Skripte
            for (j_txt in json_texts) {
                # Sicher parsen (falls JSON kaputt ist, nicht abstürzen)
                json_data <- tryCatch(jsonlite::fromJSON(j_txt), error = function(e) NULL)
                
                if (!is.null(json_data)) {
                    # Check A: Direktes Feld im Root
                    if (!is.null(json_data$datePublished)) {
                        datetime <- lubridate::as_datetime(json_data$datePublished)
                        break # Gefunden! Schleife beenden.
                    }
                    
                    # Check B: Manchmal sind Daten in einem "@graph" Array versteckt
                    if (!is.null(json_data$`@graph`)) {
                        # Extrahiere alle datePublished Felder aus dem Graphen
                        dates <- unlist(lapply(json_data$`@graph`, function(x) x$datePublished))
                        if (!is.null(dates) && length(dates) > 0) {
                            datetime <- lubridate::as_datetime(dates[1])
                            break # Gefunden!
                        }
                    }
                }
            }
        }

        # --- STRATEGIE 2: CSS FALLBACK ---
        # Nur ausführen, wenn wir im JSON nichts gefunden haben (datetime ist noch NA)
        if (is.na(datetime)) {
            datetime <- html %>%
                rvest::html_element("time.datetime--article, time.fig__caption__meta__date") %>%
                rvest::html_attr("datetime") %>%
                lubridate::as_datetime()
        }

        # --- HEADLINE ---
        headline <- html %>%
            rvest::html_elements(".document-title__headline") %>%
            rvest::html_text() %>%
            head(1)

        # --- AUTHOR ---
        author <- html %>%
            rvest::html_elements(".author__name, .authors__name") %>%
            rvest::html_text() %>%
            toString()

        # --- TEXT ---
        text <- html %>%
            rvest::html_elements(".article-body, div[itemprop=\\"articleBody\\"]") %>%
            rvest::html_text() %>%
            paste(collapse = "\n")

        s_n_list(datetime, author, headline, text)
        
        list(
            datetime = datetime,
            author = as.character(author),
            headline = as.character(headline),
            text = as.character(text)
        )
    }, error = function(e) {
        list(
            datetime = as.POSIXct(NA),
            author = NA_character_,
            headline = NA_character_,
            text = NA_character_,
            error = paste("Parser execution error:", as.character(e$message))
        )
    })
}'


edit_parse_rules(domain_name = "bild.de", rule_type = "parser_rules", new_rule = new_bild_rule)


show_parse_rules("bild.de")

parse_html_local(bild_sample, force_reload = TRUE)


load_parse_error("bild.de")


parse_html_local(bild_parse_error, force_reload = TRUE)

analyze_parse_error(bild_parse_error_parsed_local, original = FALSE)


fill_results("bild_parse_error_parsed_local")


apply_new_rule("bild.de", "parser_rules")

