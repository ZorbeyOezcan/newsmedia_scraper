# all other links are Photos and can be deleted. 
# 

parse_error_taz <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/taz.de_parse_error.rds")

# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', parse_error_taz$url, '",'), sep = "\n")