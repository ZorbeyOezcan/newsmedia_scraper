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

## Get storing Functions 
source("09_storage_manager.R")

## Get Log Functions
source("10_log_manager.R")




# Establish VPN connection & write log entry
func_03_initialzie_vpn_connnection()  # open VPN tunnel and log to vpn log 

# Build a new chunk 
func_02_build_chunk(absolute_links = 100)

# detect newest chunk obj. 
chunk_objs <- ls(pattern = "^chunk_\\d{3}$", envir = .GlobalEnv)
if (length(chunk_objs) == 0) stop("Kein Chunk-Objekt geladen.")

current_chunk <- chunk_objs[
  which.max(as.integer(sub("^chunk_(\\d{3})$", "\\1", chunk_objs)))
]

# Plot chunk overview
func_02_plot_chunk_overview(current_chunk)      

# init chunk specific dts. 
func_09_init_data_structures(current_chunk)

