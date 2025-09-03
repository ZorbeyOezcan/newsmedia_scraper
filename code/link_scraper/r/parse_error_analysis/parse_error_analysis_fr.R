source("parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()

####

# fr.de: 

# analyze and load fully 
analyze_parse_error("fr.de")
load_parse_error("fr.de")
sample_htmls("fr.de", 100)

# compare to paper boy 
aaa_test <- pb_deliver("https://www.fr.de/hessen/markanter-glaette-aktuell-vorsicht-beim-autofahren-dwd-warnt-in-hessen-vor-zr-93515601.html")
# klappt

compare_results("fr_parse_error_parsed_local")

show_parse_rules("fr.de")



# Testen 
parse_html_local("fr_sample")
analyze_parse_error(fr_sample_parsed_local, original = FALSE)
# Klappt 

# jetzt auf alle: 
parse_html_local("fr_parse_error")

# Testen: 
analyze_parse_error(fr_parse_error_parsed_local, original = FALSE)

# In den output hauen 
fill_results("fr_parse_error_parsed_local")

# Regeln anpassen 
apply_new_rule("fr", "parser_rules")

show_parse_rules("fr.de")

