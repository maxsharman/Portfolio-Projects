# Clear console and environment
if(!is.null(dev.list())) dev.off()  
rm(list = ls())
cat("\014")

# Loading Packages
library(tidyverse)
library(stringr)
library(tidytext)
library(stringr)
library(knitr)
library(ggplot2)

# Reading Starwars Episode 4 Script into R
text <- read.delim('SW_EpisodeIV.txt')
View(text)

# Exploring the Text File
head(text)
str(text)
tail(text)
summary(text)

# Tokenize the text also removing stop words and removing numbers

Tokenized_script <- text %>%
  mutate(text = as.character(text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) 

View(Tokenized_script)

# Counting word occurrences 
word_frequency <- Tokenized_script %>% 
  count(word, sort = TRUE)

# Top 15 words
top_15_words <- word_frequency %>% head(15)
View(top_15_words)

# Visual Bar chart for the top 15 most occurring words

word_plot <- ggplot(top_15_words, aes(word, n, fill = word)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('Top 15 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency")
  
ggsave("word_plot.png", plot = word_plot, width = 6, height = 4)

# Script Sentiment counts. Positive and Negative

Sentiment_count <-Tokenized_script %>% 
  inner_join(get_sentiments(), by = "word") %>% 
  count(sentiment)

kable(Sentiment_count)


# Visual Bar Chart For the Sentiment Analysis
sentiment_plot <- ggplot(Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts")

ggsave("sentiment_plot.png", plot = sentiment_plot, width = 6, height = 4)

# Extracting the all the speakers/characters from the script.
# I will replace commas in the text to avoid having names separated.

Characters <- text %>% 
  mutate(character.names = str_extract_all(character.dialogue, "\\b[A-Z][A-Z]+\\b")) %>% 
  filter(lengths(character.names) > 0) %>% 
  mutate(character.names = str_replace_all(character.names, ",", ""))

View(Characters)

# Exploring Character Names
unique(Characters['character.names'])

count(unique(Characters['character.names']))

# Counting Character Occurrences 
character_counts <- table(unlist(Characters$character.names))

character_counts <- sort(character_counts, decreasing = TRUE)

print(character_counts)

# Creating data frame for character_counts
character_counts_df <- as.data.frame(character_counts)

# Sorting for Top 5 Speakers
top_characters <- character_counts_df %>% 
  head(5)

kable(top_characters)

# Visual Bar Chart For Character Script Appearances

character_plot <- ggplot(top_characters, aes(Var1, Freq, fill = Var1)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('Star Wars Ep 4 Top 5 Character Script Appearances') +
  xlab("Characters") +
  ylab("Appearances") +
  labs(fill = "Characters")

ggsave("character_plot.png", plot = character_plot, width = 6, height = 4)

