# All links that contain "Liveblog" contain no content
# 
# this is code for printing all those fautly links into the console, to put them in "Data Cleaner"
wz_parse_error <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/wz.de_parse_error.rds")

# Filter URLs that contain "liveblog" (anywhere in the string)
liveblog_urls <- wz_parse_error[grepl("liveblog", url, ignore.case = TRUE), url]

# 3. Print the results to the console
cat(paste0('"', liveblog_urls, '",'), sep = "\n")