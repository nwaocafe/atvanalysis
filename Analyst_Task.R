library(readr)
library(dplyr)
library(stringr)
library(janitor)
library(tidytext)
library(ggplot2)
data_2024 <- read_csv("FY24 IT Survey Data - SAMPLE.csv")
data_2025 <- read_csv("FY25 IT Survey Data - SAMPLE.csv")
data_2024 <- clean_names(data_2024)
data_2025 <- clean_names(data_2025) 

# Add a column for the year
data_2024$year <- 2024
data_2025$year <- 2025

data_2024 <- data_2024 %>% rename(response = response_2024)
data_2025 <- data_2025 %>% rename(response = response_2025)

# Combine the datasets
all_data <- bind_rows(data_2024, data_2025)

# Remove NA, trim whitespace, and convert to lowercase
all_data <- all_data %>%
  filter(!is.na(response)) %>%
  mutate(response = str_trim(tolower(response)))
words <- all_data %>%
  unnest_tokens(word, response) %>%
  anti_join(stop_words)  # remove common words like “the”, “and”

words %>%
  count(word, sort = TRUE) %>%
  top_n(20) %>%
  ggplot(aes(reorder(word, n), n)) +
  geom_col() +
  coord_flip() +
  labs(title = "Top Words in Classroom Tech Feedback", x = "Word", y = "Frequency")

library(ggplot2)
library(ggwordcloud)

ggplot(word_counts_filtered, aes(label = word, size = n)) +
  geom_text_wordcloud_area(rm_outside = TRUE) +  # This removes words that don't fit
  scale_size_area(max_size = 10) +
  theme_minimal()

ggsave("wordcloud.png", width = 8, height = 6)

top_keywords <- "wifi"
library(dplyr)
library(stringr)

# Example: Assuming your dataset is called 'all_data' and the column with responses is 'response'
filtered_responses <- all_data %>%
  filter(str_detect(response, paste(top_keywords, collapse = "|")))  # Filter rows containing any of the top keywords

# View the filtered responses
View(filtered_responses) # or just use View(filtered_responses) for a full view


