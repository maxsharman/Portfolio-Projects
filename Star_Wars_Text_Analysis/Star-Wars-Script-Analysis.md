Star-Wars-EP4-Script-Text-Analysis
================
maxsharman

Loading Packages

``` r
library(rmarkdown)
```

    ## Warning: package 'rmarkdown' was built under R version 4.3.2

``` r
library(tidyverse)
```

    ## Warning: package 'tidyverse' was built under R version 4.3.2

    ## Warning: package 'ggplot2' was built under R version 4.3.2

    ## Warning: package 'dplyr' was built under R version 4.3.2

    ## Warning: package 'stringr' was built under R version 4.3.2

    ## Warning: package 'lubridate' was built under R version 4.3.2

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.4.4     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
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
```

Reading Star Wars Episode 4 Script into R

``` r
text <- read.delim('SW_EpisodeIV.txt') 
```

Exploring the Text File

``` r
head(text) 
```

    ##                                                                                                    character.dialogue
    ## 1  1 THREEPIO Did you hear that?  They've shut down the main reactor.  We'll be destroyed for sure.  This is madness!
    ## 2                                                                                            2 THREEPIO We're doomed!
    ## 3                                                        3 THREEPIO There'll be no escape for the Princess this time.
    ## 4                                                                                             4 THREEPIO What's that?
    ## 5 5 THREEPIO I should have known better than to trust the logic of a half-sized thermocapsulary dehousing assister...
    ## 6                                           6 LUKE Hurry up!  Come with me!  What are you waiting for?!  Get in gear!

``` r
str(text) 
```

    ## 'data.frame':    1010 obs. of  1 variable:
    ##  $ character.dialogue: chr  "1 THREEPIO Did you hear that?  They've shut down the main reactor.  We'll be destroyed for sure.  This is madness!" "2 THREEPIO We're doomed!" "3 THREEPIO There'll be no escape for the Princess this time." "4 THREEPIO What's that?" ...

``` r
tail(text) 
```

    ##                                                                                               character.dialogue
    ## 1005                                                     1005 LEIA Hey, I knew there was more to you than money.
    ## 1006                                                                                           1006 LUKE Oh, no!
    ## 1007               1007 THREEPIO Oh, my!  Artoo!  Can you hear me?  Say something!You can repair him, can't you?
    ## 1008                                                        1008 TECHNICIAN We'll get to work on him right away.
    ## 1009 1009 THREEPIO You must repair him!  Sir, if any of my circuits or gears will help, I'll gladly donate them.
    ## 1010                                                                               1010 LUKE He'll be all right.

``` r
summary(text)
```

    ##  character.dialogue
    ##  Length:1010       
    ##  Class :character  
    ##  Mode  :character

Tokenize the text also removing stop words and numbers

``` r
Tokenized_script <- text %>%
  mutate(text = as.character(text)) %>%
  unnest_tokens(word, text) %>% 
  anti_join(get_stopwords()) %>% 
  filter(!str_detect(word, "\\d+")) 
```

    ## Joining with `by = join_by(word)`

Counting word occurrences

``` r
word_frequency <- Tokenized_script %>% 
  count(word, sort = TRUE)
```

Sorting for the top 9 most occuring words

``` r
top_9_words <- word_frequency %>% head(9)
```

Visual Bar chart for the top 9 most occurring words

``` r
Word_occurrences <- ggplot(top_9_words, aes(word, n, fill = word)) +
  geom_bar(stat = 'identity', color = "Black") +
  ggtitle('Top 9 Most Frequent Words') +
  xlab("word") +
  ylab("Frequency") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")
  
print(Word_occurrences)
```

![](Star-Wars-Script-Analysis_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

Script Sentiment counts. Positive and Negative

``` r
Sentiment_count <-Tokenized_script %>% 
  inner_join(get_sentiments(), by = "word") %>% 
  count(sentiment)

kable(Sentiment_count)
```

| sentiment |      n |
|:----------|-------:|
| negative  | 346430 |
| positive  | 418140 |

Visual Bar Chart For the Sentiment Analysis

``` r
Sentiments <- ggplot(Sentiment_count, aes(sentiment, n, fill = sentiment)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = n), vjust = -0.3, color = "Black") +
  ggtitle("Star Wars Ep4 Sentiment Analysis") +
  xlab("Sentiments") +
  ylab("Counts") +
  theme_minimal() +
  scale_fill_brewer(palette = "Blues")

print(Sentiments)
```

![](Star-Wars-Script-Analysis_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

Extracting all the characters from the script. Here, I replace commas in
the text to avoid having names separated.

``` r
Characters <- text %>% 
  mutate(character.names = str_extract_all(character.dialogue, "\\b[A-Z][A-Z]+\\b")) %>% 
  filter(lengths(character.names) > 0) %>% 
  mutate(character.names = str_replace_all(character.names, ",", ""))
```

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `character.names = str_replace_all(character.names, ",", "")`.
    ## Caused by warning in `stri_replace_all_regex()`:
    ## ! argument is not an atomic vector; coercing

Exploring Character Names.

``` r
unique(Characters['character.names'])
```

    ##                                  character.names
    ## 1                                       THREEPIO
    ## 6                                           LUKE
    ## 11                       c("IMPERIAL" "OFFICER")
    ## 12                                         VADER
    ## 13                          c("REBEL" "OFFICER")
    ## 16                                       TROOPER
    ## 22                            c("CHIEF" "PILOT")
    ## 23                                       CAPTAIN
    ## 26                                         WOMAN
    ## 27                                         FIXER
    ## 28                                         CAMIE
    ## 31                                         BIGGS
    ## 35                                          DEAK
    ## 42                                          LEIA
    ## 46                                     COMMANDER
    ## 50                         c("SECOND" "OFFICER")
    ## 95                          c("FIRST" "TROOPER")
    ## 96                         c("SECOND" "TROOPER")
    ## 101                                         BERU
    ## 103                                         OWEN
    ## 168                             c("AUNT" "BERU")
    ## 230                                          BEN
    ## 280                                        TAGGE
    ## 281                                        MOTTI
    ## 283                                       TARKIN
    ## 326                                    BARTENDER
    ## 331                                     CREATURE
    ## 332                                        HUMAN
    ## 342                                          HAN
    ## 364                                       GREEDO
    ## 387                               c("LUKE" "XP")
    ## 390                                        JABBA
    ## 472                          c("OFFICER" "CASS")
    ## 510  c("VOICE" "OVER" "DEATH" "STAR" "INTERCOM")
    ## 511                                      OFFICER
    ## 515                                        VOICE
    ## 532              c("GANTRY" "OFFICER" "TX" "TX")
    ## 533                        c("GANTRY" "OFFICER")
    ## 561                           c("THREEPIO" "AA")
    ## 604                        c("INTERCOM" "VOICE")
    ## 623                   c("INTERCOM" "VOICE" "AA")
    ## 637                         c("TROOPER" "VOICE")
    ## 677                         c("FIRST" "OFFICER")
    ## 798                                      WILLARD
    ## 802         c("DEATH" "STAR" "INTERCOM" "VOICE")
    ## 803                                      DODONNA
    ## 804                           c("GOLD" "LEADER")
    ## 808                                        WEDGE
    ## 813                                          MAN
    ## 827                            c("RED" "LEADER")
    ## 834                                        CHIEF
    ## 840             c("MASSASSI" "INTERCOM" "VOICE")
    ## 842                               c("RED" "TEN")
    ## 843                             c("RED" "SEVEN")
    ## 845                                      PORKINS
    ## 846                              c("RED" "NINE")
    ## 848                            c("RED" "ELEVEN")
    ## 868                         c("ASTRO" "OFFICER")
    ## 874                       c("CONTROL" "OFFICER")
    ## 905                             c("GOLD" "FIVE")
    ## 908                              c("GOLD" "TWO")
    ## 913                                      WINGMAN
    ## 984                            c("BASE" "VOICE")
    ## 1008                                  TECHNICIAN

Counting Character Occurrences.

``` r
character_counts <- table(unlist(Characters$character.names))

character_counts <- sort(character_counts, decreasing = TRUE)

print(character_counts)
```

    ## 
    ##                                        LUKE 
    ##                                         253 
    ##                                         HAN 
    ##                                         153 
    ##                                    THREEPIO 
    ##                                         118 
    ##                                         BEN 
    ##                                          82 
    ##                                        LEIA 
    ##                                          57 
    ##                                       VADER 
    ##                                          41 
    ##                           c("RED" "LEADER") 
    ##                                          37 
    ##                                       BIGGS 
    ##                                          34 
    ##                                      TARKIN 
    ##                                          28 
    ##                                        OWEN 
    ##                                          25 
    ##                                     TROOPER 
    ##                                          19 
    ##                          c("GOLD" "LEADER") 
    ##                                          14 
    ##                                       WEDGE 
    ##                                          14 
    ##                                     OFFICER 
    ##                                          11 
    ##                              c("RED" "TEN") 
    ##                                           8 
    ##                            c("GOLD" "FIVE") 
    ##                                           7 
    ##                            c("AUNT" "BERU") 
    ##                                           6 
    ##        c("DEATH" "STAR" "INTERCOM" "VOICE") 
    ##                                           6 
    ##                        c("FIRST" "TROOPER") 
    ##                                           6 
    ##                                     DODONNA 
    ##                                           6 
    ##                                      GREEDO 
    ##                                           6 
    ##                                       JABBA 
    ##                                           6 
    ##                       c("INTERCOM" "VOICE") 
    ##                                           5 
    ##                                       HUMAN 
    ##                                           4 
    ##                                       MOTTI 
    ##                                           4 
    ##                                       TAGGE 
    ##                                           4 
    ##                                   BARTENDER 
    ##                                           3 
    ##            c("MASSASSI" "INTERCOM" "VOICE") 
    ##                                           3 
    ##                       c("SECOND" "TROOPER") 
    ##                                           3 
    ##                                   COMMANDER 
    ##                                           3 
    ##                                       VOICE 
    ##                                           3 
    ##                             c("GOLD" "TWO") 
    ##                                           2 
    ##                     c("IMPERIAL" "OFFICER") 
    ##                                           2 
    ##                             c("RED" "NINE") 
    ##                                           2 
    ##                                       CAMIE 
    ##                                           2 
    ##                                       CHIEF 
    ##                                           2 
    ##                                       FIXER 
    ##                                           2 
    ##                                     WILLARD 
    ##                                           2 
    ##                                     WINGMAN 
    ##                                           2 
    ##                                        BERU 
    ##                                           1 
    ##                        c("ASTRO" "OFFICER") 
    ##                                           1 
    ##                           c("BASE" "VOICE") 
    ##                                           1 
    ##                          c("CHIEF" "PILOT") 
    ##                                           1 
    ##                      c("CONTROL" "OFFICER") 
    ##                                           1 
    ##                        c("FIRST" "OFFICER") 
    ##                                           1 
    ##             c("GANTRY" "OFFICER" "TX" "TX") 
    ##                                           1 
    ##                       c("GANTRY" "OFFICER") 
    ##                                           1 
    ##                  c("INTERCOM" "VOICE" "AA") 
    ##                                           1 
    ##                              c("LUKE" "XP") 
    ##                                           1 
    ##                         c("OFFICER" "CASS") 
    ##                                           1 
    ##                        c("REBEL" "OFFICER") 
    ##                                           1 
    ##                           c("RED" "ELEVEN") 
    ##                                           1 
    ##                            c("RED" "SEVEN") 
    ##                                           1 
    ##                       c("SECOND" "OFFICER") 
    ##                                           1 
    ##                          c("THREEPIO" "AA") 
    ##                                           1 
    ##                        c("TROOPER" "VOICE") 
    ##                                           1 
    ## c("VOICE" "OVER" "DEATH" "STAR" "INTERCOM") 
    ##                                           1 
    ##                                     CAPTAIN 
    ##                                           1 
    ##                                    CREATURE 
    ##                                           1 
    ##                                        DEAK 
    ##                                           1 
    ##                                         MAN 
    ##                                           1 
    ##                                     PORKINS 
    ##                                           1 
    ##                                  TECHNICIAN 
    ##                                           1 
    ##                                       WOMAN 
    ##                                           1

Creating data frame for character_counts.

``` r
character_counts_df <- as.data.frame(character_counts)
```

Sorting for Top 5 Character Script Appearances.

``` r
top_characters <- character_counts_df %>% head(5)

kable(top_characters)
```

| Var1     | Freq |
|:---------|-----:|
| LUKE     |  253 |
| HAN      |  153 |
| THREEPIO |  118 |
| BEN      |   82 |
| LEIA     |   57 |

Visual Bar Chart For Character Script Appearances.

``` r
Characters <- ggplot(top_characters, aes(Var1, Freq, fill = Var1)) +
geom_bar(stat = 'identity', color = "Black") + ggtitle('Star Wars Ep 4
Top 5 Character Script Appearances') + xlab("Characters") +
ylab("Appearances") + labs(fill = "Characters") + theme_minimal() +
scale_fill_brewer(palette = "Blues")

print(Characters)
```

![](Star-Wars-Script-Analysis_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
