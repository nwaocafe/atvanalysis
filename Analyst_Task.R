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

#data_2024 <- data_2024 %>% rename(response = response_2024)
#data_2025 <- data_2025 %>% rename(response = response_2025)

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
  top_n(25) %>%
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

zoom_feedback <- all_data %>%
  filter(str_detect(response, regex("zoom", ignore_case = TRUE)))
View(zoom_feedback)

# Load libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)

# 1. Filter responses that mention "zoom"
zoom_feedback <- all_data %>%
  filter(str_detect(response, regex("zoom", ignore_case = TRUE)))

# 2. Tokenize the responses into words
zoom_words <- zoom_feedback %>%
  unnest_tokens(word, response)

# 3. Remove stop words (built-in + custom)
data("stop_words")
custom_stop <- c("zoom", "pepperdine", "google")  # adjust as needed

zoom_words_clean <- zoom_words %>%
  filter(!word %in% stop_words$word) %>%   # built-in stop words
  filter(!word %in% custom_stop)           # custom stop words

# 4. Count word frequency
zoom_word_counts <- zoom_words_clean %>%
  count(word, sort = TRUE)

# 5. Plot the top 10 most frequent words
zoom_word_counts %>%
  slice_max(n, n = 10) %>%
  ggplot(aes(x = reorder(word, n), y = n)) +
  geom_col(fill = "#0073C2FF") +
  coord_flip() +
  labs(title = "Top 10 Words in Zoom-Related Feedback",
       x = "Word", y = "Frequency") +
  theme_minimal()

