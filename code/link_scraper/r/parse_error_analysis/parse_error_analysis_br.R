source("parse_error_inspector.R")
library(paperboy)

####

overview <- create_parse_error_overview()

####

# br.de

# analyze and load fully 
analyze_parse_error("br.de", original = TRUE)

# Load full data 
load_parse_error("br.de")

# All Trash, can be deleted
