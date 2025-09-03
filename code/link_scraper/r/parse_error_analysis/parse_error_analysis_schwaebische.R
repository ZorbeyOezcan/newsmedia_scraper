library(paperboy)

####

overview <- create_parse_error_overview()

####

# schwaebische.de: 

# analyze and load fully 
analyze_parse_error("schwaebische.de")
load_parse_error("schwaebische.de")
sample_htmls("schwaebische.de", 100)

# compare to paper boy 
aaa_test <- pb_deliver("https://www.schwaebische.de/regional/oberschwaben/vogt/crosslaeufer-starten-in-vogt-ins-neue-jahr-3221041")
# klappt auch nicht 


show_parse_rules("schwaebische.de")

# neue regel definieren: 
new_rule_schwaebische <- '
function(html) {
  library(rvest); library(lubridate); library(jsonlite); library(stringr)
  datetime <- as.POSIXct(NA); author <- NA_character_; headline <- NA_character_; text <- NA_character_
  
  # helper function to assign extracted values to outer variables
  s_n_list <- function(datetime_val=NA, author_val=NA_character_, headline_val=NA_character_, text_val=NA_character_) {
    datetime <<- datetime_val; author <<- author_val; headline <<- headline_val; text <<- text_val; invisible(NULL)
  }
  
  tryCatch({
    # try extracting structured metadata from JSON-LD
    json_txt <- rvest::html_elements(html, "script[type=\\"application/ld+json\\"]") %>% rvest::html_text2()
    if (!isTRUE(is.na(json_txt)) && length(json_txt) >= 2) {
      json_df <- jsonlite::fromJSON(json_txt[2])
      if (!is.null(json_df$datePublished)) {
        datetime <- suppressWarnings(lubridate::as_datetime(json_df$datePublished))
      }
      headline <- json_df$headline
      if (!is.null(json_df$author) && !is.null(json_df$author$name)) author <- toString(json_df$author$name)
    }
    
    # fallback: parse <time class="tw-text-neutral-10">08.01.2025, 11:50</time>
    if (is.na(datetime)) {
      dt_txt <- html %>% rvest::html_element("time.tw-text-neutral-10") %>% rvest::html_text2()
      if (length(dt_txt) && nzchar(dt_txt)) {
        dt_txt <- gsub("\\\\s*,\\\\s*", " ", dt_txt)  # normalize "08.01.2025, 11:50" → "08.01.2025 11:50"
        dt_try <- suppressWarnings(lubridate::dmy_hm(dt_txt, tz="UTC"))
        if (!is.na(dt_try)) datetime <- dt_try
      }
    }
    
    # extract main content text
    text <- html %>%
      rvest::html_elements(".tw-text-title-md, p.paragraph, h2.tw-mb-4") %>%
      rvest::html_text2() %>%
      paste(collapse="\\n")
    
    s_n_list(datetime, author, headline, text)
    list(datetime=datetime, author=as.character(author), headline=as.character(headline), text=as.character(text))
    
  }, error=function(e) {
    # on error, return NA values and the error message
    list(datetime=as.POSIXct(NA), author=NA_character_, headline=NA_character_, text=NA_character_,
         error=paste("Parser execution error:", as.character(e$message)))
  })
}
'

# anwenden 
edit_parse_rules("schwaebische.de", "parser_rules", new_rule_schwaebische)

# Checken
show_parse_rules("schwaebische.de")

# Testen 
parse_html_local("schwaebische_sample")
analyze_parse_error(schwaebische_sample_parsed_local, original = FALSE)
# Klappt 

# jetzt auf alle: 
parse_html_local("schwaebische_parse_error")

# Testen: 
analyze_parse_error(schwaebische_parse_error_parsed_local, original = FALSE)

# In den output hauen 
fill_results("schwaebische_parse_error_parsed_local")

# Regeln anpassen 
apply_new_rule("schwaebische", "parser_rules")

show_parse_rules("schwaebische.de")
