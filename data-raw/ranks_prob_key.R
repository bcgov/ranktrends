library(readr)
ranks_prob_key <- read_csv("data-raw/ranks_prob_key.csv")
usethis::use_data(ranks_prob_key, overwrite = TRUE)
