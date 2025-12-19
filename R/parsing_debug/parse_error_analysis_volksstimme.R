# all other links are NAvigational and can be deleted. 
# 

parse_error_volksstimme <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/volksstimme.de_parse_error.rds")

# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', parse_error_volksstimme$url, '",'), sep = "\n")