library(paperboy)
source("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r/parse_error_analysis/parse_error_inspector.R")


overview <- create_parse_error_overview()

####

# freiepresse.de

# analyze and load fully 
analyze_parse_error("freiepresse.de", original = TRUE)

# nichts klappt 

# Load full data 
load_parse_error("freiepresse.de")

# Sample data
sample_htmls("freiepresse.de", 10)

# Test sample 
parse_html_local(freiepresse_sample)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://www.freiepresse.de/sport/lokalsport/zwickau/warum-fortschritt-lichtenstein-in-der-fussball-sachsenklasse-auf-einem-abstiegsplatz-steht-artikel13663324")
# all wrong too 


show_parse_rules("freiepresse.de")

neue_freiepresse_regel <- list(
    has_paywall = TRUE,
    is_all_paywall = FALSE, # Paywall ist nicht mehr auf "alle" gesetzt
    paywall_markers = list(
        css_selectors = c(
            "a[data-action='subscribe']", # Der spezifische Button aus deinem HTML
            "a[href*='abo.freiepresse.de']" # Sicherheitshalber auch Links zum Abo-Shop
        )
    )
)


# Anwenden der Regel mit der edit_parse_rules Funktion
edit_parse_rules(
    domain_name = "freiepresse.de",
    rule_type = "paywall_rules",
    new_rule = neue_freiepresse_regel
)


show_parse_rules("freiepresse.de")
# reset_rule("freiepresse.de", "parser_rules")

# test: 
parse_html_local(freiepresse_sample, force_reload = TRUE)



# check results 
analyze_parse_error(freiepresse_sample_parsed_local, original = FALSE)
# perffff


# Load full data 
load_parse_error("freiepresse.de")

# parse local 
parse_html_local(freiepresse_parse_error)



##### all paywalled
##### 
##### # ==============================================================================
# SKRIPT: DELETE frp. COMPLETE
# ==============================================================================
# Dieses Skript entfernt radikal alle Einträge, die zu FAZ gehören, aus
# allen System-Dateien (input, output, logs, error, retry, parse_error).
# ==============================================================================

library(data.table)
library(stringr)

# --- 1. KONFIGURATION ---

# Basispfad definieren (wie in deinen anderen Modulen)
base_path <- "/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper"

# Pfade zu den Dateien
paths <- list(
    input          = file.path(base_path, "data", "input", "input.rds"),
    final_data     = file.path(base_path, "data", "output", "final_data.rds"),
    error          = file.path(base_path, "data", "output", "error.rds"),
    retry          = file.path(base_path, "data", "input", "retry.rds"),
    parse_error    = file.path(base_path, "data", "input", "parse_error.rds"),
    # Logs sind optional, aber gut zu bereinigen, falls gewünscht
    response_log   = file.path(base_path, "data", "logs", "response_log.rds"),
    request_log    = file.path(base_path, "data", "logs", "request_log.rds")
)

# Ordner für Domain-spezifische Parse-Errors
parse_error_dir <- file.path(base_path, "data", "input", "parse_error")

# Zieldomains, die gelöscht werden sollen (Regex für maximale Trefferquote)
# Matcht: faz.net, www.faz.net, faz.net.de
target_domain_pattern <- "^(www\\.)?freiepresse\\.de(\\.de)?$"

# Zusätzlich URL-Pattern, falls die Domain-Spalte leer/falsch ist
target_url_pattern <- "freiepresse\\.de"

# --- 2. FUNKTION ZUM LÖSCHEN ---

clean_file <- function(file_path, file_name) {
    if (!file.exists(file_path)) {
        message(sprintf("[%s] Datei nicht gefunden: Überpringe.", file_name))
        return(NULL)
    }
    
    # Datei laden
    dt <- readRDS(file_path)
    
    # Sicherstellen, dass es eine data.table ist
    if (!is.data.table(dt)) {
        dt <- as.data.table(dt)
    }
    
    initial_rows <- nrow(dt)
    
    if (initial_rows == 0) {
        message(sprintf("[%s] Datei ist leer.", file_name))
        return(NULL)
    }
    
    rows_to_delete <- integer(0)
    
    # 1. Filterung über Domain-Spalte (falls vorhanden)
    if ("domain" %in% names(dt)) {
        # Suche nach faz.net oder faz.net.de
        idx_domain <- which(grepl(target_domain_pattern, dt$domain, ignore.case = TRUE))
        rows_to_delete <- c(rows_to_delete, idx_domain)
    }
    
    # 2. Filterung über URL-Spalte (Sicherheitsnetz)
    if ("url" %in% names(dt)) {
        idx_url <- which(grepl(target_url_pattern, dt$url, ignore.case = TRUE))
        rows_to_delete <- c(rows_to_delete, idx_url)
    }
    
    rows_to_delete <- unique(rows_to_delete)
    deleted_count <- length(rows_to_delete)
    
    if (deleted_count > 0) {
        # Zeilen löschen
        dt_clean <- dt[-rows_to_delete]
        
        # Speichern
        saveRDS(dt_clean, file_path)
        message(sprintf("[%s] %d FAZ-Einträge gelöscht. (Vorher: %d -> Nachher: %d)", 
                        file_name, deleted_count, initial_rows, nrow(dt_clean)))
    } else {
        message(sprintf("[%s] Keine FAZ-Einträge gefunden.", file_name))
    }
}

# --- 3. AUSFÜHRUNG ---

message("--- STARTE LÖSCHVORGANG FÜR FAZ.NET ---")

# A. Hauptdateien bereinigen
for (name in names(paths)) {
    clean_file(paths[[name]], name)
}

# B. Domain-spezifische Parse-Error Dateien bereinigen
if (dir.exists(parse_error_dir)) {
    message("\n--- Prüfe Parse-Error Ordner ---")
    pe_files <- list.files(parse_error_dir, pattern = "\\.rds$", full.names = TRUE)
    
    for (f in pe_files) {
        # Wir schauen, ob der Dateiname schon "faz" enthält, dann können wir sie direkt löschen
        if (grepl("faz", basename(f), ignore.case = TRUE)) {
            file.remove(f)
            message(sprintf("[Parse Error Dir] Datei gelöscht: %s", basename(f)))
        } else {
            # Andernfalls Inhalt prüfen (falls Links falsch einsortiert wurden)
            clean_file(f, basename(f))
        }
    }
}

message("\n--- FERTIG ---")