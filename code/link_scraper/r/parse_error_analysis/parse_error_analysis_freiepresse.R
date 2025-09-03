source("parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()

####

# freiepresse.de

# analyze and load fully 
analyze_parse_error("freiepresse.de", original = TRUE)

# Load full data 
# load_parse_error("freiepresse.de")

# Sample data
sample_htmls("freiepresse.de", 10)

# Test sample 
parse_html_local(freiepresse_sample)
analyze_parse_error(freiepresse_sample, original = FALSE)
# all wrong

# compare to paper boy 
aaa_test <- pb_deliver("https://www.freiepresse.de/vogtland/auerbach/asylheim-fuer-minderjaehrige-ellefeld-fordert-nach-straftaten-durch-bewohner-konsequenzen-zustaende-sind-inakzeptabel-artikel13687554")
# works 


show_parse_rules("freiepresse.de")

new_rule_freiepresse <- '

'

# Apply 
edit_parse_rules("freiepresse.de", "parser_rules", new_rule_freiepresse)
show_parse_rules("freiepresse.de")
# reset_rule("freiepresse.de", "parser_rules")

# test: 
parse_html_local(freiepresse_sample)






# Compare to paper boy: 
compare_results(freiepresse_sample_parsed_local)
# Perfect 

# now all: 
parse_html_local(freiepresse_parse_error)

# Check: 
analyze_parse_error(freiepresse_parse_error_parsed_local, original = FALSE)



fill_results("freiepresse_parse_error_parsed_local")


# Change rule 
apply_new_rule("freiepresse", "parser_rules")

show_parse_rules("freiepresse.de")

