library(tidyverse)
library(tidytext)

# Read in data
episode_VI_data <- read.table(file = "data/SW_EpisodeVI.txt", colClasses = "character") %>% 
  as_tibble() %>% 
  mutate(character = str_to_lower(character),
         dialogue = str_to_lower(dialogue))

episode_V_data <- read.table(file = "data/SW_EpisodeV.txt", colClasses = "character") %>% 
  as_tibble() %>% 
  mutate(character = str_to_lower(character),
         dialogue = str_to_lower(dialogue))

# Get top 10 characters by most no of lines
most_lines_by_characters <- 
  episode_VI_data %>% 
  group_by(character) %>% 
  count() %>%
  arrange(desc(n)) %>% 
  ungroup() %>% 
  top_n(10)

# Bar graph of characters with most lines  
most_lines_by_characters %>% 
  ggplot(mapping = aes(x = reorder(character, n), y = n)) +
  geom_col() + 
  coord_flip() +
  xlab("")

# Split lines into words
script_by_word <- 
  episode_VI_data %>% 
  unnest_tokens(word, dialogue) %>% 
  anti_join(stop_words)

number_of_words_by_character <- 
  script_by_word %>% 
  group_by(character) %>% 
  count()

word_frequency_by_character <- 
  script_by_word %>% 
  group_by(word, character) %>% 
  count() %>% 
  left_join(number_of_words_by_character, by = "character") %>% 
  select(word, character, n = n.x, total_n = n.y) %>% 
  mutate(freq = n / total_n) %>% 
  select(-(n:total_n))

word_to_character_lookup <- 
  word_frequency_by_character %>% 
  ungroup() %>% 
  group_by(word) %>%
  top_n(1) %>% 
  distinct(word, .keep_all = TRUE)

# Function to determine who's talking
whos_talking <- function(sentence){
  
most_likely <-
    sentence %>% 
    unnest_tokens(word, dialogue) %>% 
    inner_join(word_to_character_lookup, "word") %>% 
    anti_join(stop_words, "word") %>% 
    group_by(character) %>% 
    count(sort = TRUE)

most_likely[[1, 1]]
}

episode_VI_data %>% mutate(prediction =  whos_talking(dialogue))%>% View()
