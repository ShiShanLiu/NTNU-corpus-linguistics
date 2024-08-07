# CH4 Corpus analysis : to start
# ==================
# Exercise 4.1 
# ==================
library(tidyverse)
library(quanteda)
library(readtext)
library(tidytext)
library(ggrepel)
us_corp <- corpus(data_corpus_inaugural)
docvars(us_corp, "country") <- "US"

require(ggplot2)
origin_us <- us_corp %>%
  summary %>%
  ggplot(aes(x = Year, y = Tokens, group = 1, label = us_corp$President)) +
    geom_line(color = "grey") +
    geom_point(aes(color = us_corp$President)) + 
    theme_bw()

origin_us + geom_text_repel()
# ==================
# END
# ==================
# ==================
# Exercise 4.2
# ==================
us_corp_tokens <- tokens(us_corp)

country.df <- kwic(us_corp_tokens, "^(c|C)ountr(y|.+s)$", valuetype = "regex") %>%
  data.frame
country.df

ggplot(data = country.df, aes(x = docname)) +
  geom_bar() + 
  ylim(0,  30) + 
  labs(x = "Year and President", y = "Number of \"COUNTRY\" Uses") +
  theme(axis.text.x = element_text(angle = 90))
  
# ==================
# all: 345
# END country: 308, 
# ==================
# ==================
# Exercise 4.3
# ==================
us_tidy <- tidy(us_corp)
us_corp_bigram <- us_tidy %>%
  unnest_tokens(
    output = bigram,
    input = text,
    token = "ngrams",
    n = 2
  )

us_corp_bigram %>%
  filter(str_detect(bigram ,"^we .*?", )) %>%
  count(Year, President, bigram) %>%
  group_by(President, Year) %>%
  filter(n == max(n)) %>%
  arrange(desc(Year))
# ==================
# END
# ==================
# ==================
# Exercise 4.4
# ==================

# ==================
# END
# ==================
# ==================
# Exercise 4.5
# ==================
us_trigrams <- us_tidy %>%
  unnest_tokens(trigrams, text, token = "ngrams", n = 3)

us_trigrams %>%
  count(President, trigrams) %>%
  group_by(President) %>%
  top_n(3, n) %>%
  arrange(President, desc(n)) %>%
  filter(str_detect(President, "Trump|Clinton|Adams"))
# ==================
# END
# ==================
# ==================
# Exercise 4.6
# ==================
us_fourgrams <- us_tidy %>%
  unnest_tokens(ngrams, text, token = "ngrams", n = 4)

us_fourgrams %>%
  group_by(ngrams) %>%
  summarize(FREQ = n(), DISPERSION = n_distinct(Year)) %>%
  filter(DISPERSION >= 5) %>%
  arrange(desc(FREQ))
# ==================
# END
# ==================
# ==================
# Exercise 4.7
# ==================
library(wordcloud)
library(dplyr)
set.seed(123)
us_words <- anti_join(x = us_corp_word_freq, y = stop_words)

with(us_words, wordcloud(word, n, 
                             max.words = 400,
                             min.freq = 20,
                             scale = c(2,0.5),
                             shape = "circle",
                             random.order = F,
                             color = brewer.pal(8, "Dark2"),
                             vfont=c("serif","plain")))
# ==================
# END
# ==================
# ==================
# Exercise 4.8
# ==================
library(wordcloud2)
wordcloud2(us_words, size = 0.7, shape = "star")
# ==================
# END
# ==================
# ==================
# Exercise 4.9
# ==================
example <- matrix(c(90, 10, 110, 290), byrow=T, nrow=2)
example
e11 <- (100*200)/500
e12 <- (100*300)/500
e21 <- (200*400)/500
e22 <- (300*400)/500
ex.freq <- matrix(c(e11, e12, e21, e22), byrow = T, nrow = 2)
ex.freq
# ==================
# END
# ==================
# ==================
# Exercise 4.10
# ==================
us_corp_bigram_freq <- us_corp_bigram %>%
  count(bigram, sort = T)

us_corp_unigram <- us_tidy %>% # corp_us_words
  unnest_tokens(output = word,
                input = text,
                token = "words")

us_corp_word_freq <- us_corp_unigram %>% # corp_us_words_freq
  count(word, sort = T)


us_corp_collocations <- us_corp_bigram_freq %>% # corp_us_collocations
  rename(O11 = n) %>%
  tidyr::separate(bigram, c("w1", "w2"), sep = "\\s") %>%
  mutate(R1 = us_corp_word_freq$n[match(w1, us_corp_word_freq$word)],
         C1 = us_corp_word_freq$n[match(w2, us_corp_word_freq$word)]) %>%
  mutate(E11 = (R1*C1)/sum(O11)) %>%
  mutate(MI = log2(O11/E11),
         t = (O11 - E11)/sqrt(O11)) %>%
  arrange(desc(MI))

us_corp_collocations %>%
  arrange(desc(t))

# The main difference is w1 and w2 word distribution. 
# In t-score sorting, almost every w1 and w2 word belongs to function words, 
# which also carries least information in a sentence.
# However, under MI sorting, w1 and w2 words are content words.
# In terms of data processing value, MI result is more useful than t-score one.
# ==================
# END
# ==================
# ==================
# Exercise 4.11
# ==================
us_corp_bigram_freq %>%
  rename(O11 = n) %>%
  tidyr::separate(bigram, c("w1", "w2"), sep = "\\s") %>%
  mutate(R1 = us_corp_word_freq$n[match(w1, us_corp_word_freq$word)],
         C1 = us_corp_word_freq$n[match(w2, us_corp_word_freq$word)]) %>%
  mutate(O12 = R1 - O11,
         O21 = C1 - O11, .before = R1) %>%
  mutate(N = sum(us_corp_bigram_freq$n)) %>%
  mutate(R2 = N - R1, .before = C1) %>%
  mutate(C2 = N - C1, .before = N) %>%
  mutate(O22 = N - O11 - O12 - O21, .before = R1) %>%
  # calculate E11 E12 E21 E22
  mutate(E11 = (R1*C1)/N,
         E12 = (R1*C2)/N,
         E21 = (R2*C1)/N,
         E22 = (as.double(R2)*as.double(C2))/N
         ) %>%
  mutate(
    t = (O11 - E11)/sqrt(O11), .before = O11,
    MI = log2(O11/E11)
  ) %>%
  mutate(
    a = O11 * if_else(is.na(log(O11) - log(E11)) == T, 0, log(O11) - log(E11)),
    b = O12 * if_else(is.na(log(O12) - log(E12)) == T, 0, log(O12) - log(E12)),
    c = O21 * if_else(is.na(log(O21) - log(E21)) == T, 0, log(O21) - log(E21)),
    d = O22 * if_else(is.na(log(O22) - log(E22)) == T, 0, log(O22) - log(E22))
  ) %>%
  mutate(
    LLR = 2 * (a + b + c + d), .before = O11
  ) %>%
  select(w1, w2, MI, t, LLR, O11, O12, O21, O22, E11, E12, E21, E22) -> corp_us_collocations
# ==================
# END
# ==================
# ==================
# Exercise 4.12
# ==================
us_corp_bigram$Year_President <- paste(us_corp_bigram$Year, us_corp_bigram$President, sep = "_")
us_corp_bigram_freq
us_corp_bigram %>%
  select(Year_President, bigram) -> corp_us_bigram_sh

corp_us_bigram_sh # form a variable
corp_us_bigram_sh %>%
  group_by(Year_President, bigram) %>%
  summarize(N = n()) -> corp_us_bigram_sh_freq

# combine here.
ljoined_corp_us_freq <- left_join(corp_us_bigram_sh_freq, us_corp_bigram_freq)

ljoined_corp_us_freq %>%
  rename(O11 = n) %>%
  rename(n = N) %>%
  tidyr::separate(bigram, c("w1", "w2"), sep = "\\s", remove = F) %>%
  mutate(
    R1 = us_corp_word_freq$n[match(w1, us_corp_word_freq$word)],
    C1 = us_corp_word_freq$n[match(w2, us_corp_word_freq$word)]
  ) %>%
  mutate(N = sum(us_corp_bigram_freq$n)) %>%
  mutate(E11 = (R1*C1)/N) %>%
  mutate(
    t = (O11 - E11)/sqrt(O11),
    MI = log2(O11/E11)
  ) %>%
  select(Year_President, bigram, n, w1, w2, O11, R1, C1, E11, MI, t) %>%
  filter(O11 > 5) %>%
  top_n(MI, n = 5) %>%
  group_by(Year_President) %>%
  mutate(MI = sort(MI, decreasing = T)) -> joined.corp.us.freq

joined.corp.us.freq
# draw ggplot here
library(ggrepel)
joined.corp.us.freq %>%
  tidyr::separate(Year_President, c("Year", "President"), sep = "_", remove = F) %>%
  ggplot(aes(x = MI, y = Year, color = Year, group = 1)) + 
  geom_point() + 
  geom_text_repel(aes(label = bigram), size = 3, color = "black")
# ==================
# END
# ==================
# ==================
# Exercise 4.13
# ==================
# The regular expression is : ha(ve|s|d) (\w+? )*\w+(en|ed)$
# ==================
# END
# ==================
# ==================
# Exercise 4.14
# ==================
require(stringr)
require(tidyr)
us_corp_sents <- us_tidy %>%
  unnest_tokens(
    output = sentence,
    input = text,
    token = "sentences"
  )
us_corp_sents

us_corp_sents$SENT_ID <- seq.int(nrow(us_corp_sents))
us_corp_sents$INDEX <- paste(us_corp_sents$Year, us_corp_sents$President, sep = "-")
# us_corp_sents
us_corp_sents %>%
  unnest_tokens(
    output = perfect,
    input = sentence,
    token = function(x)
      str_extract_all(x, "ha(d|ve|s) \\w+(en|ed)")
    ) -> result_perfect
 
result_perfect
# ==================
# END
# ==================
# ==================
# Exercise 4.15
# ==================
# group_by "INDEX"
result_perfect %>%
  group_by(INDEX) %>%
  summarize(TOKEN_FREQ = n(),
            TYPE_FREQ = n_distinct(perfect)) %>%
  pivot_longer(c("TOKEN_FREQ", "TYPE_FREQ"), names_to = "STATISTIC", values_to = "NUMBER") %>%
  ggplot(aes(INDEX, NUMBER, fill = STATISTIC)) +
  geom_bar(stat = "identity",position = position_dodge()) +
  theme(axis.text.x = element_text(angle=90))
# ==================
# END
# ==================