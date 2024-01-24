    # Loading Packages
    library(rmarkdown)
    library(tidyverse)
    library(stringr)
    library(tidytext)
    library(stringr)
    library(knitr)
    library(ggplot2)

    knitr::opts_chunk$set(fig.path='Figs/')


    # Reading Star Wars Episode 4 Script into R
    text <- read.delim('SW_EpisodeIV.txt')
    View(text)

    # Exploring the Text File
    head(text)

    ##                                                                                                    character.dialogue
    ## 1  1 THREEPIO Did you hear that?  They've shut down the main reactor.  We'll be destroyed for sure.  This is madness!
    ## 2                                                                                            2 THREEPIO We're doomed!
    ## 3                                                        3 THREEPIO There'll be no escape for the Princess this time.
    ## 4                                                                                             4 THREEPIO What's that?
    ## 5 5 THREEPIO I should have known better than to trust the logic of a half-sized thermocapsulary dehousing assister...
    ## 6                                           6 LUKE Hurry up!  Come with me!  What are you waiting for?!  Get in gear!

    str(text)

    ## 'data.frame':    1010 obs. of  1 variable:
    ##  $ character.dialogue: chr  "1 THREEPIO Did you hear that?  They've shut down the main reactor.  We'll be destroyed for sure.  This is madness!" "2 THREEPIO We're doomed!" "3 THREEPIO There'll be no escape for the Princess this time." "4 THREEPIO What's that?" ...

    tail(text)

    ##                                                                                               character.dialogue
    ## 1005                                                     1005 LEIA Hey, I knew there was more to you than money.
    ## 1006                                                                                           1006 LUKE Oh, no!
    ## 1007               1007 THREEPIO Oh, my!  Artoo!  Can you hear me?  Say something!You can repair him, can't you?
    ## 1008                                                        1008 TECHNICIAN We'll get to work on him right away.
    ## 1009 1009 THREEPIO You must repair him!  Sir, if any of my circuits or gears will help, I'll gladly donate them.
    ## 1010                                                                               1010 LUKE He'll be all right.

    summary(text)

    ##  character.dialogue
    ##  Length:1010       
    ##  Class :character  
    ##  Mode  :character

    # Tokenize the text also removing stop words and removing numbers

    Tokenized_script <- text %>%
      mutate(text = as.character(text)) %>%
      unnest_tokens(word, text) %>% 
      anti_join(get_stopwords()) %>% 
      filter(!str_detect(word, "\\d+")) 

    ## Joining with `by = join_by(word)`

    View(Tokenized_script)

    # Counting word occurrences 
    word_frequency <- Tokenized_script %>% 
      count(word, sort = TRUE)

    # Top 9 words
    top_9_words <- word_frequency %>% head(9)
    View(top_9_words)

    # Visual Bar chart for the top 15 most occurring words

    Word_occurrences <- ggplot(top_9_words, aes(word, n, fill = word)) +
      geom_bar(stat = 'identity', color = "Black") +
      ggtitle('Top 9 Most Frequent Words') +
      xlab("word") +
      ylab("Frequency") +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")
      
    print(Word_occurrences)

![](Star-Wars-Text-Analysis-project_files/figure-markdown_strict/unnamed-chunk-1-1.png)

    # Script Sentiment counts. Positive and Negative

    Sentiment_count <-Tokenized_script %>% 
      inner_join(get_sentiments(), by = "word") %>% 
      count(sentiment)

    kable(Sentiment_count)

<table>
<thead>
<tr class="header">
<th style="text-align: left;">sentiment</th>
<th style="text-align: right;">n</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">negative</td>
<td style="text-align: right;">346430</td>
</tr>
<tr class="even">
<td style="text-align: left;">positive</td>
<td style="text-align: right;">418140</td>
</tr>
</tbody>
</table>

    # Visual Bar Chart For the Sentiment Analysis
    Sentiments <- ggplot(Sentiment_count, aes(sentiment, n, fill = sentiment)) +
      geom_bar(stat = "identity", color = "black") +
      geom_text(aes(label = n), vjust = -0.3, color = "Black") +
      ggtitle("Star Wars Ep4 Sentiment Analysis") +
      xlab("Sentiments") +
      ylab("Counts") +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")

    print(Sentiments)

![](Star-Wars-Text-Analysis-project_files/figure-markdown_strict/unnamed-chunk-1-2.png)

    # Extracting all the characters from the script.
    # I will replace commas in the text to avoid having names separated.

    Characters <- text %>% 
      mutate(character.names = str_extract_all(character.dialogue, "\\b[A-Z][A-Z]+\\b")) %>% 
      filter(lengths(character.names) > 0) %>% 
      mutate(character.names = str_replace_all(character.names, ",", ""))

    ## Warning: There was 1 warning in `mutate()`.
    ## ℹ In argument: `character.names = str_replace_all(character.names, ",", "")`.
    ## Caused by warning in `stri_replace_all_regex()`:
    ## ! argument is not an atomic vector; coercing

    View(Characters)

    # Exploring Character Names
    unique(Characters['character.names'])

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

    count(unique(Characters['character.names']))

    ##    n
    ## 1 64

    # Counting Character Occurrences 
    character_counts <- table(unlist(Characters$character.names))

    character_counts <- sort(character_counts, decreasing = TRUE)

    print(character_counts)

    ## 
    ##                                        LUKE                                         HAN                                    THREEPIO 
    ##                                         253                                         153                                         118 
    ##                                         BEN                                        LEIA                                       VADER 
    ##                                          82                                          57                                          41 
    ##                           c("RED" "LEADER")                                       BIGGS                                      TARKIN 
    ##                                          37                                          34                                          28 
    ##                                        OWEN                                     TROOPER                          c("GOLD" "LEADER") 
    ##                                          25                                          19                                          14 
    ##                                       WEDGE                                     OFFICER                              c("RED" "TEN") 
    ##                                          14                                          11                                           8 
    ##                            c("GOLD" "FIVE")                            c("AUNT" "BERU")        c("DEATH" "STAR" "INTERCOM" "VOICE") 
    ##                                           7                                           6                                           6 
    ##                        c("FIRST" "TROOPER")                                     DODONNA                                      GREEDO 
    ##                                           6                                           6                                           6 
    ##                                       JABBA                       c("INTERCOM" "VOICE")                                       HUMAN 
    ##                                           6                                           5                                           4 
    ##                                       MOTTI                                       TAGGE                                   BARTENDER 
    ##                                           4                                           4                                           3 
    ##            c("MASSASSI" "INTERCOM" "VOICE")                       c("SECOND" "TROOPER")                                   COMMANDER 
    ##                                           3                                           3                                           3 
    ##                                       VOICE                             c("GOLD" "TWO")                     c("IMPERIAL" "OFFICER") 
    ##                                           3                                           2                                           2 
    ##                             c("RED" "NINE")                                       CAMIE                                       CHIEF 
    ##                                           2                                           2                                           2 
    ##                                       FIXER                                     WILLARD                                     WINGMAN 
    ##                                           2                                           2                                           2 
    ##                                        BERU                        c("ASTRO" "OFFICER")                           c("BASE" "VOICE") 
    ##                                           1                                           1                                           1 
    ##                          c("CHIEF" "PILOT")                      c("CONTROL" "OFFICER")                        c("FIRST" "OFFICER") 
    ##                                           1                                           1                                           1 
    ##             c("GANTRY" "OFFICER" "TX" "TX")                       c("GANTRY" "OFFICER")                  c("INTERCOM" "VOICE" "AA") 
    ##                                           1                                           1                                           1 
    ##                              c("LUKE" "XP")                         c("OFFICER" "CASS")                        c("REBEL" "OFFICER") 
    ##                                           1                                           1                                           1 
    ##                           c("RED" "ELEVEN")                            c("RED" "SEVEN")                       c("SECOND" "OFFICER") 
    ##                                           1                                           1                                           1 
    ##                          c("THREEPIO" "AA")                        c("TROOPER" "VOICE") c("VOICE" "OVER" "DEATH" "STAR" "INTERCOM") 
    ##                                           1                                           1                                           1 
    ##                                     CAPTAIN                                    CREATURE                                        DEAK 
    ##                                           1                                           1                                           1 
    ##                                         MAN                                     PORKINS                                  TECHNICIAN 
    ##                                           1                                           1                                           1 
    ##                                       WOMAN 
    ##                                           1

    # Creating data frame for character_counts
    character_counts_df <- as.data.frame(character_counts)

    # Sorting for Top 5 Character Script Appearances 
    top_characters <- character_counts_df %>% 
      head(5)

    kable(top_characters)

<table>
<thead>
<tr class="header">
<th style="text-align: left;">Var1</th>
<th style="text-align: right;">Freq</th>
</tr>
</thead>
<tbody>
<tr class="odd">
<td style="text-align: left;">LUKE</td>
<td style="text-align: right;">253</td>
</tr>
<tr class="even">
<td style="text-align: left;">HAN</td>
<td style="text-align: right;">153</td>
</tr>
<tr class="odd">
<td style="text-align: left;">THREEPIO</td>
<td style="text-align: right;">118</td>
</tr>
<tr class="even">
<td style="text-align: left;">BEN</td>
<td style="text-align: right;">82</td>
</tr>
<tr class="odd">
<td style="text-align: left;">LEIA</td>
<td style="text-align: right;">57</td>
</tr>
</tbody>
</table>

    # Visual Bar Chart For Character Script Appearances

    Characters <- ggplot(top_characters, aes(Var1, Freq, fill = Var1)) +
      geom_bar(stat = 'identity', color = "Black") +
      ggtitle('Star Wars Ep 4 Top 5 Character Script Appearances') +
      xlab("Characters") +
      ylab("Appearances") +
      labs(fill = "Characters") +
      theme_minimal() +
      scale_fill_brewer(palette = "Blues")

    print(Characters)

![](Star-Wars-Text-Analysis-project_files/figure-markdown_strict/unnamed-chunk-1-3.png)
