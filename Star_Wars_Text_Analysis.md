Star-Wars-EP4-Script-Text-Analysis
================
maxsharman

#### Loading Packages

``` r
library(textstem)
```

    ## Warning: package 'textstem' was built under R version 4.3.3

    ## Loading required package: koRpus.lang.en

    ## Warning: package 'koRpus.lang.en' was built under R version 4.3.3

    ## Loading required package: koRpus

    ## Warning: package 'koRpus' was built under R version 4.3.3

    ## Loading required package: sylly

    ## Warning: package 'sylly' was built under R version 4.3.3

    ## For information on available language packages for 'koRpus', run
    ## 
    ##   available.koRpus.lang()
    ## 
    ## and see ?install.koRpus.lang()

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.3

    ## Warning: package 'ggplot2' was built under R version 4.3.3

    ## Warning: package 'tidyr' was built under R version 4.3.3

    ## Warning: package 'dplyr' was built under R version 4.3.3

    ## Warning: package 'stringr' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter()   masks stats::filter()
    ## ✖ dplyr::lag()      masks stats::lag()
    ## ✖ readr::tokenize() masks koRpus::tokenize()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(stringr)
library(tidytext)
```

    ## Warning: package 'tidytext' was built under R version 4.3.2

``` r
library(stringr)
library(knitr)
```

    ## Warning: package 'knitr' was built under R version 4.3.2

``` r
library(ggplot2)
library(wordcloud)
```

    ## Warning: package 'wordcloud' was built under R version 4.3.3

    ## Loading required package: RColorBrewer

#### Reading Star Wars Episode 4 Script into R

``` r
script <- read.delim('SW_EpisodeIV.txt') 
```

#### Exploring the Text File

``` r
str(script) 
```

    ## 'data.frame':    1010 obs. of  1 variable:
    ##  $ character.dialogue: chr  "1 THREEPIO Did you hear that?  They've shut down the main reactor.  We'll be destroyed for sure.  This is madness!" "2 THREEPIO We're doomed!" "3 THREEPIO There'll be no escape for the Princess this time." "4 THREEPIO What's that?" ...

# Text Analysis On The Whole Script

#### Extracting Character Names

``` r
Characters <- script %>% 
  mutate(character.names = str_extract_all(character.dialogue, "\b[A-Z][A-Z]+\b")) %>% 
  filter(lengths(character.names) > 0) %>% 
  mutate(character.names = str_replace_all(character.names, ",", ""))
```

    ## Warning: There was 1 warning in `mutate()`.  
    ## ℹ In argument: `character.names = str_replace_all(character.names, ",", "")`.  
    ## Caused by warning in `stri_replace_all_regex()`:  
    ## ! argument is not an atomic vector; coercing

#### Finding Top 5 Character Name Occurances and thier Frequency Counts

``` r
character_dialogue_counts <- table(unlist(Characters$character.names));

character_dialogue_counts <- sort(character_dialogue_counts, decreasing = TRUE);

character_counts_df <- as.data.frame(character_dialogue_counts);

top_characters <- character_counts_df %>% head(5);

kable(top_characters);
```

| Var1     | Freq |
|:---------|-----:|
| LUKE     |  253 |
| HAN      |  153 |
| THREEPIO |  118 |
| BEN      |   82 |
| LEIA     |   57 |
|
#### Plotting the Top 5 Occuring Characters And their Dialogue Frequency

``` r
Characters <- ggplot(top_characters, aes(Var1, Freq, fill = Var1)) +
geom_bar(stat = 'identity', color = "Black") + ggtitle('Star Wars Ep 4
Top 5 Character Dialogue Frequency') + xlab("Characters") +
ylab("Frequency") + labs(fill = "Characters") + theme_minimal() +
scale_fill_brewer(palette = "Blues");

print(Characters);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

``` r
character_names <- c(
  "luke", "han", "threepio", "ben", "leia", "vader", "red leader", "biggs", "tarkin", "owen",
  "trooper", "gold leader", "wedge", "officer", "red ten");

Tokenized_script <- script %>%
  mutate(text = as.character(character.dialogue)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% character_names);
```

    ## Joining with `by = join_by(word)`

#### Counting word occurrences

``` r
Tokenized_script <- Tokenized_script %>% mutate(lemmatized_text = lemmatize_words(word));
Tokenized_script <- drop(Tokenized_script['lemmatized_text']);

word_frequency <- Tokenized_script %>% 
  count(lemmatized_text, sort = TRUE);
```

#### Sorting for the top 10 most frequent words

``` r
top_10_words <- word_frequency %>% head(10);
top_10_words <- top_10_words %>% rename(Frequency = n);
```

#### Visual Bar chart for the top 10 most frequent words

``` r
Word_occurrences <- ggplot(top_10_words, aes(lemmatized_text, Frequency, fill = Frequency)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('Top 10 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue");
  
print(Word_occurrences);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->

``` r
wordcloud(words = word_frequency$lemmatized_text, freq = word_frequency$n, min.freq = 10,
          random.order = FALSE, colors = brewer.pal(8, "BrBG"));
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-10-2.png)<!-- -->

#### Script Sentiment counts

``` r
Sentiment_count <- Tokenized_script %>% 
  inner_join(get_sentiments(), c("lemmatized_text" = "word")) %>% 
  count(sentiment);

kable(Sentiment_count);
```

| sentiment |   n |
|:----------|----:|
| negative  | 357 |
| positive  | 423 |

#### Visual Bar Chart For the Sentiment Analysis

``` r
Sentiment_count <- ggplot(Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues");

print(Sentiment_count);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

# Text Analysis By Character

``` r
Luke_lines <- script %>% 
  filter(grepl("LUKE", character.dialogue));

Vader_lines <- script %>% 
  filter(grepl("VADER", character.dialogue));


Threepio_lines <- script %>% 
  filter(grepl("THREEPIO", character.dialogue));
```

#### LUKE

``` r
Tokenized_Luke_lines <- Luke_lines %>% 
  mutate(text = as.character(character.dialogue)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% character_names);
```

    ## Joining with `by = join_by(word)`

``` r
Tokenized_Luke_lines <- Tokenized_Luke_lines %>% mutate(lemmatized_text = lemmatize_words(word));
Tokenized_Luke_lines <- drop(Tokenized_Luke_lines['lemmatized_text']);

word_frequency_Luke_lines <- Tokenized_Luke_lines %>% 
  count(lemmatized_text, sort = TRUE);

top_10_words_Luke_lines <- word_frequency_Luke_lines %>% head(10);
top_10_words_Luke_lines <- top_10_words_Luke_lines %>% rename(Frequency = n);
```

#### VADER

``` r
Tokenized_Vader_lines <- Vader_lines %>% 
  mutate(text = as.character(character.dialogue)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% character_names);
```

    ## Joining with `by = join_by(word)`

``` r
Tokenized_Vader_lines <- Tokenized_Vader_lines %>% mutate(lemmatized_text = lemmatize_words(word));
Tokenized_Vader_lines <- drop(Tokenized_Vader_lines['lemmatized_text']);

word_frequency_Vader_lines <- Tokenized_Vader_lines %>% 
  count(lemmatized_text, sort = TRUE);

top_10_words_Vader_lines <- word_frequency_Vader_lines %>% head(10);
top_10_words_Vader_lines <- top_10_words_Vader_lines %>% rename(Frequency = n);
```

#### THREEPIO

``` r
Tokenized_Threepio_lines <- Threepio_lines %>% 
  mutate(text = as.character(character.dialogue)) %>% 
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) %>% 
  filter(!word %in% character_names);
```

    ## Joining with `by = join_by(word)`

``` r
Tokenized_Threepio_lines <- Tokenized_Threepio_lines %>% mutate(lemmatized_text = lemmatize_words(word));
Tokenized_Threepio_lines <- drop(Tokenized_Threepio_lines['lemmatized_text']);

word_frequency_Threepio_lines <- Tokenized_Threepio_lines %>% 
  count(lemmatized_text, sort = TRUE);

top_10_words_Threepio_lines <- word_frequency_Threepio_lines %>% head(10);
top_10_words_Threepio_lines <- top_10_words_Threepio_lines %>% rename(Frequency = n);
```

#### Most Frequent Words By Character

``` r
 ggplot(top_10_words_Luke_lines, aes(lemmatized_text, Frequency, fill = Frequency)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('LUKE Top 10 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency") +
  theme_minimal() +
  scale_fill_gradient(low = "lightblue", high = "darkblue");

wordcloud(words = word_frequency_Luke_lines$lemmatized_text, freq = word_frequency_Luke_lines$n, min.freq = 3,
          random.order = FALSE, colors = c('steelblue', 'darkblue', 'navy', 'midnightblue'));

```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
wordcloud(words = word_frequency_Luke_lines$lemmatized_text, freq = word_frequency_Luke_lines$n, min.freq = 3,
          random.order = FALSE, colors = c('steelblue', 'darkblue', 'navy', 'midnightblue'))
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-17-2.png)<!-- -->

``` r
ggplot(top_10_words_Vader_lines, aes(lemmatized_text, Frequency, fill = Frequency)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('VADER Top 10 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency") +
  theme_minimal() +
  scale_fill_gradient(low = "red", high = "darkred");
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

``` r
wordcloud(words = word_frequency_Vader_lines$lemmatized_text, freq = word_frequency_Vader_lines$n, min.freq = 2,
          random.order = FALSE,  colors = c('firebrick', 'darkred', 'indianred'))
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-18-2.png)<!-- -->

``` r
 ggplot(top_10_words_Threepio_lines, aes(lemmatized_text, Frequency, fill = Frequency)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('THREEPIO Top 10 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency") +
  theme_minimal() +
  scale_fill_gradient(low = "lightyellow", high = "yellow");
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
wordcloud(words = word_frequency_Threepio_lines$lemmatized_text, freq = word_frequency_Threepio_lines$n, min.freq = 3,
          random.order = FALSE, colors = c('darkorange', 'gold', 'darkgoldenrod'))
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-19-2.png)<!-- -->

``` r
VADER_Sentiment_count <- Tokenized_Vader_lines %>% 
  inner_join(get_sentiments(), c("lemmatized_text" = "word")) %>% 
  count(sentiment);

LUKE_Sentiment_count <- Tokenized_Luke_lines %>% 
  inner_join(get_sentiments(), c("lemmatized_text" = "word")) %>% 
  count(sentiment);

THREEPIO_Sentiment_count <- Tokenized_Threepio_lines %>% 
  inner_join(get_sentiments(), c("lemmatized_text" = "word")) %>% 
  count(sentiment);
```

#### Sentiment Count By Character

``` r
LUKE_Sentiment_count_plot <- ggplot(LUKE_Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("LUKE Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts") +
  theme_minimal() +
  scale_fill_manual(values = c("skyblue", 'blue'));

print(LUKE_Sentiment_count_plot);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
VADER_Sentiment_count_plot <- ggplot(VADER_Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("VADER Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts") +
  theme_minimal() +
  scale_fill_manual(values = c("red", 'darkred'));

print(VADER_Sentiment_count_plot);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
THREEPIO_Sentiment_count_plot <- ggplot(THREEPIO_Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("THREEPIO Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts") +
  theme_minimal() +
  scale_fill_manual(values = c("yellow", 'lightyellow'));

print(THREEPIO_Sentiment_count_plot);
```

![](Star_Wars_Text_Analysis_files/figure-gfm/unnamed-chunk-23-1.png)<!-- -->
