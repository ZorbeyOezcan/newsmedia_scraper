# all other links are Videos and can be deleted. 
# 

parse_error_tagesspiegel <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/tagesspiegel.de_parse_error.rds")

# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', parse_error_tagesspiegel$url, '",'), sep = "\n")
