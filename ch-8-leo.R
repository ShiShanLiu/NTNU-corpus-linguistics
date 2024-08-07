# CH8 Constructions and idioms
# ==================
# Exercise 8.1 
# ==================
library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(jiebaR)
library(readtext)
library(dplyr)

apple_seg <- worker(bylines = T,
                 user = "demo_data/dict-ch-user.txt",
                 symbol = T)

CHUNK_DELIMITER <- "[，。！？；：\n]+"

apple_df$text %>%
  # map(str_split, pattern= CHUNK_DELIMITER, simplify=TRUE) %>% ## line tokenization
  map(segment, apple_seg) %>% ## word tokenization
  map(unlist) %>% ## list to vec
  as.tokens -> apple_tokens


kwic(apple_tokens, phrase("了 起來")) -> le_kwic
kwic(apple_tokens, phrase("一 起來")) -> i_kwic
kwic(apple_tokens, phrase("不 起來")) -> bu_kwic

# "了 起來" and "不 起來" follow action verbs in a sentence,
# while "一 起來" is followed by a verb.

# ==================
# END
# ==================
# ==================
# Exercise 8.2
# ==================
setwd("C:/R language/Rscripts/")
mandarin_articles <- read_csv("C:/R language/Rscripts/demo_data/corpus-news-collection.csv",
                              locale = locale(encoding = "UTF-8"))



mandarin_segment <- worker(
  bylines = F,
  user = "demo_data/dict-ch-user.txt",
  symbol = T,
  write = "NOFILE"
)

mandarin_seg_text <- function(text, jiebar){
  segment(text, jiebar) %>%
    str_c(collapse = " ")
}

# normalize_document <- function(texts) {
#   texts %>%
#     str_replace_all("\\p{C}", " ") %>%  ## remove control chars
#     str_replace_all("\\s+", "\n")  ## replace whitespaces with linebreak
# }

mandarin_articles %>%
  filter(!str_detect(desc, "^\\s*$")) %>%
  mutate(text_id = row_number(), .before = title) %>%
  rename(text = desc) %>%
  select(text_id, title, text, image, url, source, date)-> mandarin_articles
mandarin_articles

mandarin_articles %>%
  mutate(text_tag = map_chr(text, mandarin_seg_text, mandarin_segment), .before = image) -> mandarin_articles

pattern_le <- "[^\\s]+\\s了\\b"

mandarin_articles %>%
  select(text_id, title, text_tag) %>%
  unnest_tokens(
    output = construction,
    input = text_tag,
    token = function(x)
      str_extract_all(x, pattern = pattern_le)
  ) -> mandarin_le

# create word freq list
mandarin_word_freq <- mandarin_articles %>%
  select(text_id, title, text_tag) %>%
  unnest_tokens(
    word,
    text_tag,
    token = function(x)
      str_split(x, "\\s+")
  ) %>%
  filter(nzchar(word)) %>%
  count(word, sort = T)

mandarin_word_freq %>%
  head(100)
# joint frequency
mandarin_le_freq <- mandarin_le %>%
  count(construction, sort = T) %>%
  separate(col = "construction",
           into = c("w1", "construction"),
           sep = "\\s") %>%
  mutate(w1_freq = mandarin_word_freq$n[match(w1, mandarin_word_freq$word)])
mandarin_le_freq


mandarin_le_freq %>%
  filter(str_detect(w1, pattern = "[\u4E00-\u9FFF]+")) -> mandarin_le_freq
mandarin_le_freq %>%
  head(100)

mandarin_le_freq %>%
  select(w1, w1_freq, n) %>%
  write_tsv("le.tsv")

sink("le_info.txt")
cat("Corpus Size:", sum(mandarin_word_freq$n), "\n")
cat("Construction size:", sum(mandarin_le_freq$n), "\n")
sink()

file.create("le_results.txt")

source("demo_data/coll.analysis.r")

results <- readLines("le_results.txt", encoding = "UTF-8")

results <- results[-c(1:17, (length(results)-17):length(results))]

collo_table <- read_tsv(I(results))

collo_table %>%
  mutate(coll.strength = replace(coll.strength, coll.strength == "Inf", 303.98)) -> collo_table
collo_table

collo_table %>%
  filter(relation == "attraction") %>%
  select(words, obs.freq, delta.p.constr.to.word, 
         delta.p.word.to.constr, coll.strength) %>%
  pivot_longer(
    cols = c("obs.freq",
             "delta.p.constr.to.word",
             "delta.p.word.to.constr",
             "coll.strength"),
    names_to = "metric",
    values_to = "strength"
  ) %>%
  mutate(metric = factor(
    metric,
    levels = c("obs.freq",
               "delta.p.constr.to.word",
               "delta.p.word.to.constr",
               "coll.strength")
  )) %>%
  group_by(metric) %>%
  top_n(10, strength) %>%
  ungroup() %>%
  arrange(metric, desc(strength)) -> coll_table_long

coll_table_long %>%
  mutate(words = reorder_within(words, strength, metric)) %>%
  ggplot(aes(words, strength, fill = metric)) +
  geom_col(position = "dodge" ,show.legend = F) + 
  coord_flip() +
  facet_wrap(~metric, scales = "free") +
  tidytext::scale_x_reordered() +
  labs(
    x = "Collexemes",
    y = "Strength",
    title = "Collexemes Ranked by Different Metrics"
  )
# fail to see Chinese characters in my computer
# ==================
# END
# ==================
# ==================
# Exercise 8.3
# ==================
chinese_news <- read_csv("demo_data/corpus-news-collection.csv", locale = locale(encoding = "UTF-8"))
chinese_news %>%
  mutate(text_id = row_number()) -> chinese_news

chinese_idiom <- readtext("demo_data/dict-ch-idiom.txt", encoding = "UTF-8")
chinese_idiom

word_seg <- worker(
  bylines =  T,
  user = "demo_data/dict-ch-user.txt",
  symbol = F
)

chinese_news %>%
  unnest_tokens(
    output = word,
    input = desc,
    token = function(x)
      segment(x, jiebar = word_seg)
  ) -> chinese_news_word_df

chinese_idiom %>%
  unnest_tokens(
    output = idioms,
    input = text,
    token = function(x)
      str_split(x, pattern = "\n")
  ) -> chinese_idiom_df

chinese_news_word_df %>%
  group_by(word) %>%
  summarize(freq = n(), dispersion = n_distinct(title)) %>%
  ungroup() %>%
  filter(nchar(word) == 4) %>%
  arrange(desc(freq)) -> four_idiom_freq_dis

four_idiom_freq_dis %>%
  filter(word %in% chinese_idiom_df$idioms) -> four_character_idioms
four_character_idioms

# ==================
# END
# ==================
# ==================
# Exercise 8.4
# ==================
four_idiom_freq_dis %>%
  filter(str_detect(word, pattern = "[\u4E00-\u9FFF]+")) %>%
  filter(word %in% chinese_idiom_df$idioms) %>%
  filter(str_detect(word, pattern = "(.).\\1.")) -> x_x_construction

x_x_construction %>%
  top_n(20, freq) %>%
  ggplot(aes(x = reorder(word, freq), y = freq, fill = freq)) +
  geom_bar(stat = "identity", show.legend = F) +
  coord_flip() +
  labs(x = "Four-Character Words", y = "Frequency")

# ==================
# END
# ==================
# ==================
# Exercise 8.5
# ==================
x_x_construction %>%
  mutate(schema = str_replace(word, pattern = "(.).\\1.", replacement = "\\1_\\1_"), .before = freq) %>%
  group_by(schema) %>%
  summarize(token = sum(freq), type = n()) %>%
  arrange(desc(token)) %>%
  filter(type >= 2)-> schema_df

schema_df %>%
  ggplot(aes(x = reorder(schema, type), y = type, fill = type)) +
  geom_bar(stat = "identity", show.legend = F) + 
  coord_flip() +
  labs(x = "Four-Character Words", y = "Frequency")

# ==================
# END
# ==================
# ==================
# Exercise 8.6
# ==================
x_x_construction %>%
  mutate(schema = str_replace(word, pattern = "(.).\\1.", replacement = "\\1_\\1_"), .before = freq) %>%
  group_by(schema) %>%
  summarize(token = sum(freq), type = n()) %>%
  arrange(desc(token)) %>%
  filter(type >= 5)-> schema_df_new

x_x_construction %>%
  mutate(schema = str_replace(word, pattern = "(.).\\1.", replacement = "\\1_\\1_"), .before = freq) %>%
  filter(schema %in% schema_df_new$schema) -> x_x_freq

x_x_freq %>%
  ggplot(aes(x = reorder(word, freq), y = freq, fill = schema)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = F) +
  coord_flip() +
  facet_wrap(~schema, scales = "free") +
  labs(x = "Four-Character Words", y = "Frequency")

# ==================
# END
# ==================
# ==================
# Exercise 8.7
# ==================
chinese_news_word_df %>%
  filter(str_detect(word, pattern = "[\u4E00-\u9FFF]+")) %>%
  filter(word %in% chinese_idiom_df$idioms) %>%
  filter(str_detect(word, pattern = "(.).\\1.")) %>%
  mutate(schema = str_replace(word, pattern = "(.).\\1.", replacement = "\\1_\\1_")) %>%
  select(text_id, title, source, word, schema) -> chinese_news_df

chinese_news_df %>%
  group_by(source) %>%
  summarize(token = n(), type = n_distinct(word), N = n_distinct(text_id)) %>%
  mutate(token_mean = token/N,
         type_mean = type/N) %>%
  arrange(desc(token)) -> token_type_df

token_type_df %>%
  pivot_longer(cols = c("token", "type"),
               names_to = "metric",
               values_to = "frequency") %>%
  # mutate(metric = factor(metric,
  #                        levels = c("token", "type"))) %>%
  arrange(metric, desc(frequency)) %>%
  select(source, metric, frequency) -> token_type_long

token_type_long %>%
  ggplot(aes(x = reorder(source, frequency), y = frequency, fill = metric)) +
  geom_col(position = "dodge", show.legend = F) +
  facet_wrap(~metric, scales = "free") + 
  coord_flip()

# ==================
# END
# ==================