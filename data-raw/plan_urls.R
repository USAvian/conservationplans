## code to prepare `plan-urls` dataset goes here
plan_urls <- read.csv("./data-raw/plan_urls.csv")

# overwrite package data object
usethis::use_data(plan_urls, overwrite = TRUE)
