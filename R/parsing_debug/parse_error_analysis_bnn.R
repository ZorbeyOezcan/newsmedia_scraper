# all other links are Videos, images or paywalled and can be deleted. 
# 

parse_error_bnn <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/bnn.de_parse_error.rds")

# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', parse_error_bnn$url, '",'), sep = "\n")