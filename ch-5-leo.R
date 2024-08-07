# CH5 Parts-of-Speech Tagging
# ==================
# Exercise 5.1 
# ==================
library(tidyr)
library(dplyr)
require(ggplot2)
library(tidyverse)
library(tidytext)
library(quanteda)
library(spacyr)
library(rsyntax)
spacy_initialize(model = "en_core_web_sm",
                 condaenv = "ai_keras")

reticulate::py_config() # check python env
corp_us_words
corp_us_words %>%
  mutate(sent_tag = paste(corp_us_words$token, corp_us_words$pos, sep = "/")) %>%
  select(doc_id, sentence_id, sent_tag) %>%
  group_by(doc_id, sentence_id) %>%
  summarize(
    sent_tag = paste(sent_tag, collapse = "")
  ) -> corp_us_sents
corp_us_sents
# ==================
# END
# ==================
# ==================
# Exercise 5.2 still need a regression line
# ==================
syn_com <- corp_us_words %>%
  group_by(doc_id) %>%
  summarize(verb_num = sum(pos=="VERB"),
            sent_num = max(sentence_id),
            word_num = n()) %>%
  mutate(F_C = (verb_num/sent_num)*(word_num/sent_num)) %>%
  ungroup

syn_com

syn_com %>%
  ggplot(aes(x = doc_id, y = F_C, fill = doc_id)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle=90)) +
  labs(title = "Syntactic Complexty", x = "Presidents", y = "Fichtner's C") +
  guides(fill = "none")
# ==================
# END
# ==================
# ==================
# Exercise 5.3
# ==================
parsedtxt <- spacy_parse(data_corpus_inaugural, 
                         pos = T,         # universal POS
                         tag = T,         # OntoNotes 5 POS
                         lemma = T,       # Lemma
                         entity = T,      # Named Entities
                         dependency = T,  # Dependency
                         nounphrase = T)  # NP Chunking

parsedtxt %>%
  filter(dep_rel == "nsubj" | dep_rel == "ROOT") %>%
  group_by(doc_id, sentence_id) %>%
  select(doc_id, sentence_id, token_id, token, dep_rel, head_token_id)

parsedtxt %>%
  filter(dep_rel == "nsubj") %>%
  select(doc_id, sentence_id, token_id, token, head_token_id) %>%
  rename(nsubj = token)-> nsubj_parsedtxt

parsedtxt %>%
  filter(dep_rel == "ROOT") %>%
  select(doc_id, sentence_id, token_id, token, dep_rel, head_token_id) %>%
  rename(root = token)-> root_parsedtxt

left_join(nsubj_parsedtxt, root_parsedtxt, by = "doc_id") %>%
  filter(head_token_id.x == token_id.y & sentence_id.x == sentence_id.y) -> nsubj.root.parsedtxt

nsubj.root.parsedtxt %>%
  mutate(
    subject_predicate = paste(nsubj, root, sep = "_")
  ) %>%
  rename(sentence_id = sentence_id.x) %>%
  select(doc_id, sentence_id, subject_predicate) -> result
result
# ==================
# END
# ==================
# ==================
# Exercise 5.4
# ==================
corp_us_sents <- readRDS("demo_data/corp_us_sents.RDS")
corp_us_sents
# define regex patterns
pattern_pat1 <- "[^/ ]+/ADP [^/]+/NOUN"
corp_us_sents
# extract patterns from corp
corp_us_sents %>%
  unnest_tokens(
    output = pat_pp,
    input = sentence_tag,
    token = function(x)
      str_extract_all(x, pattern = pattern_pat1)
  ) %>%
  mutate(pat_clean = str_replace_all(pat_pp, "/adp|/noun", "")) -> result_pat1
result_pat1
# extract the prep and head
result_pat1 %>%
  tidyr::separate(col="pat_pp", into=c("PREP","NOUN"), sep="\\s+" ) %>%
  mutate(PREP = str_replace_all(PREP, "/[^ ]+",""),
         NOUN = str_replace_all(NOUN, "/[^ ]+","")) -> result_pat1a
result_pat1a
# ==================
# END
# ==================
# ==================
# Exercise 5.5
# ==================
result_pat1a %>%
  filter(PREP == "of") %>%
  count(doc_id, PREP, NOUN) %>%
  tidyr::pivot_wider(
    id_cols = c("doc_id"),
    names_from = "NOUN",
    values_from = "n",
    values_fill = list(n=0))

corp_us_sents

corp_us_sents %>%
  unnest_tokens(
    output = pat_pp,
    input = sentence_tag,
    token = function(x)
      str_extract_all(x, pattern = "[^ ]+/ADP ([^ ]+(?!PRON|NOUN|PROPN)) [^ ]+/(PRON|NOUN|PROPN)|[a-zA-Z]+/ADP [^ ]+/(PRON|NOUN|PROPN)")
  ) -> adp.noun.sents
adp.noun.sents
# ==================
# END
# ==================
# ==================
# Exercise 5.6 
# ==================
adp.noun.sents %>%
  mutate(
    PREP.temp = str_extract_all(pat_pp, pattern = "[^ ]+/adp"),
    NOUN.temp = str_extract_all(pat_pp, pattern = "[^ ]+/noun"),
    .before = pat_pp
  ) %>%
  mutate(
    PREP = str_replace_all(PREP.temp, "/adp", ""),
    NOUN = str_replace_all(NOUN.temp, "/noun", "")
  ) %>%
  select(doc_id, sentence_id, PREP, NOUN, pat_pp) %>%
  filter(NOUN != "character(0)")
# ==================
# END
# ==================
# ==================
# Exercise 5.7
# ==================
parsedtxt %>%
  filter(doc_id == "1793-Washington") -> parsedtxt.1793

parsedtxt.1793 %>%
  filter(dep_rel == "agent" | dep_rel == "pobj" | dep_rel == "prep") %>%
  select(doc_id, sentence_id, token_id, token, head_token_id, dep_rel) -> parsedtxt.1793.sh

parsedtxt.1793 %>%
  select(doc_id, sentence_id, token_id, head_token_id, token, dep_rel) %>%
  mutate(token_dep = paste(parsedtxt.1793$token, parsedtxt.1793$dep_rel, sep = "/")) %>%
  group_by(doc_id, sentence_id) %>%
  summarize(token_dep = paste(token_dep, collapse = "- ")) -> token.dep.1793
token.dep.1793

token.dep.1793$token_dep

str_view_all(token.dep.1793$token_dep, pattern = "[^ ]+/agent-")
# token.dep.1793 %>%
#   mutate(token_dep = str_replace_all(token_dep, "-", " ")) -> token.dep.1793
# token.dep.1793

token.dep.1793 %>%
  unnest_tokens(
    output = phrase,
    input = token_dep,
    token = function(x)
      str_extract_all(x, "[^ ]+/agent- [^ ]+/[^ ]+ [^ ]+/pobj-")
  )


# mutate(phrase = str_replace_all(phrase, "/[^ ]+", ""))

phrase.1793
parsedtxt.1793 %>%
  select(doc_id, sentence_id, token_id, head_token_id, token, dep_rel) %>%
  mutate(token_dep = paste(parsedtxt.1793$token, parsedtxt.1793$dep_rel, sep = "/")) %>%
  group_by(doc_id, sentence_id) %>%
  summarize(token_dep = paste(token_dep, collapse = " ")) %>%
  unnest_tokens(
    output = P_N,
    input = token_dep,
    token = function(x)
      str_extract_all(x, "[^ ]+/agent ([^ ]+/[^ ]+)+ [^ ]+/pobj | [^ ]+/prep ([^ ]+/[^ ]+)+ [^ ]+/pobj|[^ ]+/agent [^ ]+/pobj|[^ ]+/prep [^ ]+/pobj")
  )

temp.1793$P_N <- trimws(temp.1793$P_N, which = c("both"))
temp.1793
temp.1793 %>%
  mutate(
    P_N = str_replace_all(P_N, " [^ ]+/[^ ]+ ", " "),
    P_N = str_replace_all(P_N, "/agent", ""),
    P_N = str_replace_all(P_N, "/prep", ""),
    P_N = str_replace_all(P_N, "/pobj", ""),
    P_N = str_replace_all(P_N, " ", "_")
  ) -> pn.1973
pn.1973

spacy_finalize()
# ==================
# END
# ==================
# ==================
# Exercise 5.8
# ==================
library(rvest)
ppt.url <- "https://www.ptt.cc/bbs/Gossiping"
gossiping.session <- session(ppt.url)
gossiping.form <- gossiping.session %>%
  html_node("form") %>%
  html_form

gossiping <- session_submit(
  x = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)
gossiping

gossiping %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("index[0-9]{2,}\\.html") %>%
  str_extract("[0-9]+") %>%
  as.numeric() -> page.latest
page.latest

cur_index_page <- str_c(ppt.url, "/index", page.latest, ".html")

extract_art_links <- function(index_page, session){
    session %>%
    session_jump_to(index_page) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("[A-Z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>%
    str_c("https://www.ptt.cc",.) -> links.article
  
  return(links.article)
}
cur_art_links <- extract_art_links(cur_index_page, gossiping)
cur_art_links

extract_article_push_tables <- function(link){
  article.url <- link
  temp.html <- gossiping %>% session_jump_to(article.url) # link to the www
  # article header
  article.header <- temp.html %>%
    html_nodes("span.article-meta-value") %>% # meta info regarding the article
    html_text()
  
  # article meta
  article.author <- article.header[1] %>% str_extract("^[A-z0-9_]+") # athuor
  article.title <- article.header[3] # title
  article.datetime <- article.header[4] # time stamp
  
  # article content
  article.content <- temp.html %>%
    html_nodes( # article body
      xpath = '//div[@id="main-content"]/node()[not(self::div|self::span[@class="f2"])]'
    ) %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")
  
  # Merge article table 
  article.table <- tibble(
    datetime = article.datetime,
    title = article.title,
    author = article.author,
    content = article.content,
    url = article.url
  )
  
  # push nodes
  article.push <- temp.html %>% 
    html_nodes(xpath = "//div[@class = 'push']") # extracting pushes
  # NOTE: If CSS is used, div.push does a lazy match (extracting div.push.... also)
  
  # push tags    
  push.table.tag <- article.push %>% 
    html_nodes("span.push-tag") %>% 
    html_text(trim = TRUE) # push types (like or dislike)
  
  # push author id
  push.table.author <- article.push %>% 
    html_nodes("span.push-userid") %>% 
    html_text(trim = TRUE) # author
  
  # push content    
  push.table.content <- article.push %>% 
    html_nodes("span.push-content") %>%
    html_text(trim = TRUE)
  
  # push datetime      
  push.table.datetime <- article.push %>% 
    html_nodes("span.push-ipdatetime") %>%
    html_text(trim = TRUE) # push time stamp
  
  # merge push table
  push.table <- tibble(
    tag = push.table.tag,
    author = push.table.author,
    content = push.table.content,
    datetime = push.table.datetime,
    url = article.url
  )
  
  # return
  
  return(list(article.table = article.table, 
              push.table = push.table))
}

cur_index_page %>%
  extract_art_links(session = gossiping) %>%
  map(extract_article_push_tables) -> ptt_data

ptt_data %>%
  map(function(x) x$article.table) %>%
  bind_rows -> article.table.all

article.table.all

push.table.all <- ptt_data %>%
  map(function(x) x$push.table) %>%
  bind_rows
push.table.all

# initialize Chinese model
spacy_initialize(model = "zh_core_web_sm",
                 condaenv = "ai_keras")
reticulate::py_config() ## check ur python environment

article.content <- article.table.all$content
content.corpus <- corpus(article.table.all$content)

spacy_parse(
  content.corpus,
  pos = T,
  tag = T,
  entity = T,
  dependency = T
) -> ptt.spacy


ptt.spacy %>%
  filter(pos == "NOUN"|pos == "VERB") %>%
  filter(str_detect(token, pattern = "[\\p{Han}]")) %>%
  count(token, sort = T) -> ptt.token.freq

library(wordcloud)
set.seed(123)
with(ptt.token.freq, wordcloud(token, n,
                               max.words = 300,
                               min.freq = 3,
                               scale = c(2, 0.5),
                               color = brewer.pal(8, "Dark2"),
                               ))
spacy_finalize()
# ==================
# END
# ==================
# ==================
# Exercise 5.9
# ==================
library(stringr)
spacy_initialize(model = "en_core_web_sm",
                 condaenv = "ai_keras")

reticulate::py_config() # check python env
movie.revirews.corpus <- quanteda.textmodels::data_corpus_moviereviews
movie.review.tidy <- tidy(movie.revirews.corpus)
movie.review.tidy$doc_id <- str_c(movie.review.tidy$id1,
                                  "_",
                                  movie.review.tidy$id2,
                                  ".txt")
movie.review.tidy
movie.review.tidy %>%
  select(doc_id, sentiment) -> movie.review.tidy.sh
movie.review.tidy.sh

spacy_parse(
  movie.revirews.corpus,
  pos = T,
  lemma = T,
  tag = T,
  entity = T,
  dependency = T
) -> movie.review.spacy
movie.review.spacy


movie.review.spacy %>%
  filter(tag == "JJ" | tag == "JJR" | tag == "JJS") %>%
  select(doc_id, lemma) -> movie.review.sh
movie.review.sh

left_join(movie.review.sh, movie.review.tidy.sh) -> movie.sentiment

movie.sentiment %>%
  group_by(sentiment, lemma) %>%
  summarize(n = n()) %>%
  filter(str_detect(lemma, pattern = "^[A-z]+$")) -> movie.sentiment.sh

movie.sentiment.sh %>%
  group_by(sentiment) %>%
  top_n(n, n = 20) %>%
  arrange(sentiment, desc(n)) -> ans1
ans1

movie.review.spacy %>%
  filter(tag == "NN" | tag == "NNP" | tag == "NNPS"|
           tag == "NNS" | tag == "VB" | tag == "VBD"|
           tag == "VBG" | tag == "VBN" | tag == "VBP"|
           tag == "VBZ" | tag == "JJ" | tag == "JJR" | 
           tag == "JJS") %>%
  select(doc_id, lemma) -> movie.nvj.sh
movie.nvj.sh

left_join(movie.nvj.sh, movie.review.tidy.sh) -> movie.nvj.join
movie.nvj.join

movie.nvj.join %>%
  filter(str_detect(lemma, pattern = "^[A-z]+$")) -> movie.nvj.join.sh
movie.nvj.join.sh

# calculate pos and neg doc numbers
movie.review.tidy %>%
  select(sentiment) %>%
  filter(sentiment == "neg") -> neg.doc
neg.num <- length(neg.doc$sentiment)
neg.num

movie.review.tidy %>%
  select(sentiment) %>%
  filter(sentiment == "pos") -> pos.doc
pos.num <- length(pos.doc$sentiment)
pos.num
# ----------------------------------
movie.nvj.join.sh %>%
  group_by(lemma, sentiment) %>%
  summarize(freq = n(), dispersion = n_distinct(doc_id)) %>%
  mutate(
    tf_idf = freq * log(1000/dispersion)
  ) -> movie.tf_idf
movie.tf_idf

movie.tf_idf %>%
  filter(str_detect(lemma, pattern = "^[A-Za-z]+$")) -> movie.tf_idf
movie.tf_idf

movie.tf_idf %>%
  select(sentiment, everything()) %>%
  group_by(sentiment) %>%
  top_n(tf_idf, n = 20) %>%
  arrange(sentiment, desc(tf_idf)) -> ans2
ans2
# ==================
# END
# ==================