# All links that are not ending in ".html" are admin pages or navigation pages 
# 
# this is code for printing all those fautly links into the console, to put them in "Data Cleaner"
rtl_parse_error <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error/rtl.de_parse_error.rds")

# Filter URLs that do NOT end with ".html"
non_html_urls <- rtl_parse_error[!grepl("\\.html$", url, ignore.case = TRUE), url]

# Print the filtered URLs 
cat(paste0('"', non_html_urls, '",'), sep = "\n")