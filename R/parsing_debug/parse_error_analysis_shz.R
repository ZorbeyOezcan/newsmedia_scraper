# All links that contain no numbers carry no content
# 
# this is code for printing all those fautly links into the console, to put them in "Data Cleaner"
shz_parse_error <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/shz.de_parse_error.rds")

# Filter URLs that contain no numbers (anywhere in the string)
no_number_urls <- shz_parse_error[!grepl("[0-9]", gsub("shz", "", url)), url]

# print 
cat(paste0('"', no_number_urls, '",'), sep = "\n")
