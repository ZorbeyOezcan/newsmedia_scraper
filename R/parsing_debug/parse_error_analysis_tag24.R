# All links that contain no numbers carry no content
# 
# this is code for printing all those fautly links into the console, to put them in "Data Cleaner"
tag24_parse_error <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/tag24.de_parse_error.rds")

# Filter URLs that contain no numbers (anywhere in the string)
no_number_urls <- tag24_parse_error[!grepl("[0-9]", gsub("tag24", "", url)), url]

# print 
cat(paste0('"', no_number_urls, '",'), sep = "\n")


# all other links are Videos and can be deleted. 
# 


# Print them into the cosole to exclude them via data Cleaner
cat(paste0('"', tag24_parse_error$url, '",'), sep = "\n")
