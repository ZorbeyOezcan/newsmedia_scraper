
# Ensure right WD
setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")

# Sourcing functions: 
source("02_chunk_manager.R")
source("03_identity_manager.R")
source("06_html_parser.R")

# Initializing VPN 
func_03_initialzie_vpn_connnection()

# Building chunk 
func_02_build_chunk()

# Loading chunk 
chunk <<- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/chunks/chunk_01.rds")
input <<- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/input.rds")

# Getting Chunk overview
func_02_plot_chunk_overview("chunk_01")

