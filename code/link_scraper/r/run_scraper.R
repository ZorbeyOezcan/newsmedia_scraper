# Ensure right WD
setwd("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/r")

# Sourcing functions: 

## Ensure all structures exist and are correct
source("01_init.R")

## Get Chunk Builder 
source("02_chunk_manager.R")

## Get VPN Update and Session Builder
source("03_identity_manager.R")

## Session Management functions
source("04_request_orchestrator.R")

## Request execution
source("05_request_executor.R")

## Get Parse Function
source("06_html_parser.R")

# Initializing VPN 
func_03_initialzie_vpn_connnection()

# Building chunk 
func_02_build_chunk(absolute_links = 100)

# Plotting Chunk overview
func_02_plot_chunk_overview("chunk_01")

# Initialize Session pool
func_04_initialize_session_pool()


