library(tidyverse)
library(janitor)
library(openalexR)

# batch.csv was created on 2026-02-03, from public items in the Loughborough Research Repository'

all_data <- read.csv("batch.csv")


# Loughborough Business School --------------------------------------------

LB_data <- all_data %>%
  clean_names() %>%
  filter(item_type == "journal contribution") %>% 
  filter(str_detect(school, 
                    "Business and Economics|Loughborough Business School"))

journal_counts <- LB_data %>%
  filter(!is.na(issn) & issn != "") %>%
  group_by(issn) %>%
  summarise(
    articles = n(),
    journal_title = first(published_in),
    .groups = "drop"
  )

write.csv(journal_counts_with_topics, "business_journal_counts.csv", row.names = FALSE)

# Aeronautical, Automotive, Chemical and Materials Engineering -----------------

aacme_data <- all_data %>%
  clean_names() %>%
  filter(item_type == "journal contribution") %>% 
  filter(str_detect(school, 
                    "Aeronautical, Automotive, Chemical and Materials Engineering"))

journal_counts <- aacme_data %>%
  filter(!is.na(issn) & issn != "") %>%
  group_by(issn) %>%
  summarise(
    articles = n(),
    journal_title = first(published_in),
    .groups = "drop"
  )

write.csv(journal_counts_with_topics, "aacme_journal_counts.csv", row.names = FALSE)

# Mechanical, Electrical and Manufacturing Engineering -----------------

meme_data <- all_data %>%
  clean_names() %>%
  filter(item_type == "journal contribution") %>% 
  filter(str_detect(school, 
                    "Mechanical, Electrical and Manufacturing Engineering"))

journal_counts <- meme_data %>%
  filter(!is.na(issn) & issn != "") %>%
  group_by(issn) %>%
  summarise(
    articles = n(),
    journal_title = first(published_in),
    .groups = "drop"
  )

write.csv(journal_counts_with_topics, "wolfson_journal_counts.csv", row.names = FALSE)

# OpenAlex for subjects ---------------------------------------------------

issns <- journal_counts %>%
  pull(1) 

# journals_raw = Journals on OpenAlex
journals_raw <- oa_fetch(
  entity = "sources",
  issn = issns,
  verbose = TRUE
)

journals_concepts <- journals_raw %>% 
  select(id, display_name, issn_l, issn, topics) 

journals_topics_long <- journals_concepts %>%
  filter(lengths(topics) > 0) %>%
  unnest(topics, names_sep = "_")

journals_subfields <- journals_topics_long %>%
  filter(topics_type == "subfield") %>%
  select(
    journal_title = display_name,
    issn_l,
    subfield = topics_display_name
  )

journals_subfields_collapsed <- journals_subfields %>%
  group_by(journal_title, issn_l) %>%
  summarise(
    subfields = paste(sort(unique(subfield)), collapse = ", "),
    .groups = "drop"
  )

# journals_subfields_collapsed = Journals with subfields

journal_counts_with_topics <- journal_counts %>%
  left_join(
    journals_subfields_collapsed,
    by = c("issn" = "issn_l")
  ) %>% 
  filter(!is.na(subfields) & subfields != "") %>%
  select(
    journal_title.y,   # from your data
    issn,
    articles,
    subfields
  ) %>%
  arrange(desc(articles))

# journal_counts_with_topics = Unique entries
