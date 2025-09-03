source("parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()

####

# faz.net

# analyze and load fully 
analyze_parse_error("faz.net", original = TRUE)

# Load full data 
# load_parse_error("faz.net")

# Sample data
sample_htmls("faz.net", 10)

# Test sample 
parse_html_local(faz_sample)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://www.faz.net/aktuell/feuilleton/kolumnen/muslimisch-juedisches-abendbrot/biodeutsch-ist-unwort-des-jahres-warum-wir-uns-fuer-den-begriff-ausgesprochen-haben-110242997.html")
# paywall ist das problem 

show_parse_rules("faz.net")


# Pfad zu den lokalen Regeln holen
paths <- get_module_paths()
local_rules_path <- file.path(paths$parsing_config, "06_paywall_rules_generated_local.rds")

# Regeln laden
aktuelle_regeln <- readRDS(local_rules_path)

# Die spezifische Regel für die Domain (z.B. "faz") in eine neue Variable kopieren
faz_paywall_regel <- aktuelle_regeln$faz

# Den neuen CSS-Selektor an den Vektor der bestehenden Selektoren anhängen
faz_paywall_regel$paywall_markers$css_selectors <- c(
  faz_paywall_regel$paywall_markers$css_selectors, 
  "h3[data-external-selector='paywall-label']"
)

edit_parse_rules(
  domain_name = "faz.de", 
  rule_type = "paywall_rules", 
  new_rule = faz_paywall_regel
)



# Apply 
edit_parse_rules("faz.net", "parser_rules", new_rule_faz)
show_parse_rules("faz.net")
# reset_rule("faz.net", "parser_rules")

# test: 
parse_html_local(faz_sample)

# Compare to paper boy: 
compare_results(faz_sample_parsed_local)
# Perfect 

# now all: 
parse_html_local(faz_parse_error)

# Check: 
analyze_parse_error(faz_parse_error_parsed_local, original = FALSE)



fill_results("faz_parse_error_parsed_local")


# Change rule 
apply_new_rule("faz", "parser_rules")

show_parse_rules("faz.net")

