library(paperboy)

####

overview <- create_parse_error_overview()

####

# spiegel.de: 

# analyze and load fully 
analyze_parse_error("spiegel.de")

load_parse_error("spiegel.de")

sample_htmls("spiegel.de", 10)

show_parse_rules("spiegel.de")

# Testen nach neuer search html func in 06: 

# Testen 
parse_html_local("spiegel_sample")
analyze_parse_error(spiegel_sample_parsed_local, original = FALSE)
# Klappt 

# jetzt auf alle: 
parse_html_local("spiegel_parse_error")

# Testen: 
analyze_parse_error(spiegel_parse_error_parsed_local, original = FALSE)

# In den output hauen 
fill_results("spiegel_parse_error_parsed_local")
