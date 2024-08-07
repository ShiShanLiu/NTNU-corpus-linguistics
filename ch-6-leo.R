# CH6 Keyword Analysis
# ==================
# Exercise 6.1
# ==================
library(tidyverse)
library(tidytext)
library(readtext)
library(quanteda)
flist <- c("demo_data/corp_perl.txt", "demo_data/corp_python.txt")
flist
corpus <- readtext(flist) %>% 
  corpus %>%
  tidy %>%
  mutate(textid = basename(flist)) %>%
  select(textid, text)
corpus

# unnest words from text based corpus
corpus_word <- corpus %>% 
  unnest_tokens(word, text, token="words")

# add word_id
corpus_word %>%
  mutate(word_id = seq(nrow(corpus_word)), .before = word) -> corpus_word

corpus_word
# ==================
# END
# ==================
# ==================
# Exercise 6.2
# ==================
corpus_word %>%
  count(word, textid, sort = T) -> word_freq
word_freq
word_freq %>%
  pivot_wider(
    names_from = "textid",
    values_from = "n",
    values_fill = 0
  ) -> contingency_table
contingency_table
# ==================
# END
# ==================
# ==================
# Exercise 6.3 c and d counts are incorrect
# ==================
movie.review <- read_csv("demo_data/data-movie-reviews.csv")
movie.review

movie.review %>%
  unnest_tokens(
    output = word,
    input = review,
    token = "words"
  ) %>%
  select(word, sentiment) -> movie.review.unnested

movie.review.unnested %>%
  count(word, sentiment, sort = T) %>%
  pivot_wider(
    names_from = sentiment,
    values_from = n,
    values_fill = 0
  ) -> movie.review.wider
movie.review.wider

movie.review.wider %>%
  rename(
    a = negative,
    b = positive
  ) %>%
  select(word, a, b) %>%
  filter(str_detect(word, pattern = "\\W") == F) %>%
  filter(a > 10 | b > 10) %>%
  mutate(c = sum(a) - a, d = sum(b) - b) -> movie.contingency.table
movie.contingency.table # 22008 * 3 (incorrect)

# turn int to double
movie.contingency.table$a <- as.double(movie.contingency.table$a)
movie.contingency.table$b <- as.double(movie.contingency.table$b)
movie.contingency.table$c <- as.double(movie.contingency.table$c)
movie.contingency.table$d <- as.double(movie.contingency.table$d)
movie.contingency.table

# create G2 contingency table
movie.contingency.table %>%
  mutate(a.exp = ((a+b)*(a+c))/(a+b+c+d),
         b.exp = ((a+b)*(b+d))/(a+b+c+d),
         c.exp = ((c+d)*(a+c))/(a+b+c+d),
         d.exp = ((c+d)*(b+d))/(a+b+c+d),
         G2 = 2*(a*log(a/a.exp) + b*log(b/b.exp) + c*log(c/c.exp) + d*log(d/d.exp))
  ) -> movie.keyness.table

movie.keyness.table %>%
  mutate(preference = ifelse(a > a.exp, "negative", "positive")) %>%
  select(word, preference, G2, a, b, c, d, a.exp, b.exp, c.exp, d.exp) -> movie.keyness.table

movie.keyness.table %>%
  group_by(preference) %>%
  top_n(G2, n = 10) %>%
  select(word, preference, everything()) %>%
  arrange(preference, -G2) -> movie.contingency.result

movie.contingency.result
# ==================
# END
# ==================