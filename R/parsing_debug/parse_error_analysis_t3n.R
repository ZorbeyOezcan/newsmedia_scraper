library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")


overview <- create_parse_error_overview()

####

# t3n.de

# analyze and load fully 
analyze_parse_error("t3n.de", original = TRUE)

# nichts klappt 

# Load full data 
load_parse_error("t3n.de")

# Sample data
sample_htmls("t3n.de", 10)

# Test sample 
parse_html_local(t3n_sample)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://t3n.de/news/android-16-design-aenderungen-in-beta-versteckt-1685450")
# all wrong too 


show_parse_rules("t3n.de")

neue_t3n_regel <- '
function(html) {
    library(rvest); library(lubridate); library(jsonlite); library(stringr)
    
    # Initiale Variablen
    datetime <- as.POSIXct(NA); author <- NA_character_; headline <- NA_character_; text <- NA_character_
    
    # Helper Scope Function
    s_n_list <- function(datetime_val = NA, author_val = NA_character_, headline_val = NA_character_, text_val = NA_character_) {
        datetime <<- datetime_val; author <<- author_val; headline <<- headline_val; text <<- text_val; invisible(NULL)
    }

    tryCatch({
        # --- STRATEGIE 1: DATALAYER (JavaScript Variable) ---
        # t3n nutzt einen sehr ausführlichen dataLayer. Das ist die sauberste Quelle.
        tryCatch({
            # Suche alle Script-Tags
            scripts <- html %>% rvest::html_elements("script") %>% rvest::html_text()
            # Finde das Script mit "dataLayer.push" und "article.headline"
            dl_script <- scripts[str_detect(scripts, "dataLayer\\\\.push") & str_detect(scripts, "article\\\\.headline")]
            
            if (length(dl_script) > 0) {
                # Wir extrahieren das JSON-Objekt aus dem JS-Code mit Regex
                # Regex Korrektur: 4 Backslashes fuer Literale Klammern/Punkte im Funktions-String
                json_str <- str_match(dl_script[1], "dataLayer\\\\.push\\\\((\\\\{.*?\\\\})\\\\);")[2]
                
                if (!is.na(json_str)) {
                    # JSON parsen
                    meta_data <- jsonlite::fromJSON(json_str)
                    
                    # Headline
                    if (!is.null(meta_data$article.headline)) headline <- meta_data$article.headline
                    
                    # Author
                    if (!is.null(meta_data$article.author)) author <- meta_data$article.author
                    
                    # Datum (ISO 8601 Format: 2025-05-02T16:30:18+00:00)
                    if (!is.null(meta_data$article.publication_date)) {
                        datetime <- lubridate::ymd_hms(meta_data$article.publication_date)
                    }
                }
            }
        }, error = function(e) {})

        # --- STRATEGIE 2: HTML SELEKTOREN (Fallback) ---
        
        # Headline Fallback
        if (is.na(headline)) {
            headline <- html %>% rvest::html_element(".c-article__headline h1, .c-article__headline h2") %>% rvest::html_text2()
        }
        
        # Datum Fallback (<time datetime="...">)
        if (is.na(datetime)) {
            dt_node <- html %>% rvest::html_element("time[datetime]") %>% rvest::html_attr("datetime")
            if (!is.na(dt_node)) datetime <- lubridate::ymd_hms(dt_node)
        }
        
        # Author Fallback
        if (is.na(author)) {
            author <- html %>% rvest::html_element("a[href*=\'/redaktion/\'] strong, a.u-link-simple strong") %>% rvest::html_text2()
        }

        # --- TEXT EXTRAKTION (Cleaned) ---
        html_clean <- html
        
        # Entferne stoerende Elemente
        html_clean %>% rvest::html_elements(".tg-crosslinks, .c-suggestNews-container, .c-ad-container") %>% xml2::xml_remove()
        html_clean %>% rvest::html_elements(".c-consent-overlay, script, style") %>% xml2::xml_remove()
        
        paragraphs <- html_clean %>% rvest::html_elements("p") %>% rvest::html_text2()
        
        # Filter: Leere Paragraphen und typische Boilerplate-Texte entfernen
        paragraphs <- paragraphs[nchar(paragraphs) > 20] 
        paragraphs <- paragraphs[!str_detect(paragraphs, "^Anzeige$")] 
        paragraphs <- paragraphs[!str_detect(paragraphs, "Inhalte anzeigen")]
        
        text <- paste(paragraphs, collapse = "\n")

        s_n_list(datetime, author, headline, text)
        list(datetime = datetime, author = as.character(author), headline = as.character(headline), text = as.character(text))
        
    }, error = function(e) {
        list(datetime = as.POSIXct(NA), author = NA_character_, headline = NA_character_, text = NA_character_, error = paste("Parser execution error:", as.character(e$message)))
    })
}
'


edit_parse_rules(domain_name = "t3n.de", rule_type = "parser_rules", new_rule = neue_t3n_regel)


show_parse_rules("t3n.de")
# reset_rule("t3n.de", "parser_rules")

# test: 
parse_html_local(t3n_sample)

# check results 
analyze_parse_error(t3n_sample_parsed_local, original = FALSE)
# perffff


# Load full data 
load_parse_error("t3n.de")

# parse local 
parse_html_local(t3n_parse_error)

analyze_parse_error(t3n_parse_error_parsed_local, original = FALSE)
# perffff# perffff# perffff 



# now all: 
parse_html_local(t3n_parse_error)

# Check: 
analyze_parse_error(t3n_parse_error_parsed_local, original = FALSE)



fill_results("t3n_parse_error_parsed_local")


# Change rule 
apply_new_rule("t3n", "parser_rules")

show_parse_rules("t3n.de")

