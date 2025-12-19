# all other links are Videos and can be deleted. 
# 

parse_error_swr <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/swr.de_parse_error.rds")

# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', parse_error_swr$url, '",'), sep = "\n")