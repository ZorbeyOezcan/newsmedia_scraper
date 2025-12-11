results_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/output/final_data.rds")
input_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/input.rds")
error_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/output/error.rds")
parse_error_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/parse_error.rds")
retry_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/input/retry.rds")
response_log_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/response_log.rds")
request_log_test <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/request_log.rds")
vpn_log <- readRDS("/Users/zorbeyozcan/newsmedia_scraper/code/link_scraper/data/logs/03_vpn_log.rds")


library(dplyr)

# Zähle die Vorkommen jeder einzigartigen Domain
domain_counts_parse_error <- parse_error_test[, .N, by = domain]

# Zeige die Ergebnisse an (standardmäßig nach Anzahl absteigend sortiert)
print(domain_counts_parse_error[order(-N)])


# Zähle die Vorkommen jeder einzigartigen Domain
domain_counts_results <- results_test[, .N, by = domain]

# Zeige die Ergebnisse an (standardmäßig nach Anzahl absteigend sortiert)
print(domain_counts_results[order(-N)])



# Gesamtanzahl der Einträge
total_n <- nrow(input_test)

# Anzahl der Einträge mit processed == TRUE
processed_true_n <- sum(input_test$processed, na.rm = TRUE)

# Anzahl der Einträge mit processed == FALSE
processed_false_n <- sum(input_test$processed == FALSE, na.rm = TRUE)


# Zählt die Zeilen, bei denen eine der beiden Bedingungen zutrifft
input_test[processed == TRUE | parse_error == TRUE, .N]

