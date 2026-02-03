library(tidyverse)
library(janitor)


# batch.csv was created on 2026-02-03, from public items in the Loughborough Research Repository'

all_data <- read.csv("batch.csv")

LB_data <- all_data %>%
  clean_names() %>%
  filter(item_type == "journal contribution") %>% 
  filter(str_detect(school, 
                    "Business and Economics|Loughborough Business School"))

journal_counts <- LB_data %>%
  clean_names() %>%
  filter(!is.na(issn) & issn != "") %>%
  count(issn, published_in, sort = TRUE)

write.csv(journal_counts, "business_journal_counts.csv", row.names = FALSE)