# CH7 Chinese Text Processing
# ==================
# Exercise 7.1 
# ==================
library(tidyverse)
library(tidytext)
library(quanteda)
library(stringr)
library(jiebaR)
library(readtext)
library(rvest)
text <- "綠黨桃園市議員王浩宇爆料，指民眾黨不分區被提名人蔡壁如、黃瀞瑩，在昨（6）日才請辭是為領年終獎金。台灣民眾黨主席、台北市長柯文哲7日受訪時則說，都是按流程走，不要把人家想得這麼壞。"

seg <- worker(
  symbol = T,
  bylines = T
)
segment(text, jiebar = seg)

# ==================
# END
# ==================
# ==================
# Exercise 7.2
# ==================
seg2 <- worker(user = "demo_data/dict-ch-user-demo.txt")
segment(text, seg2) -> segment2

seg3 <- worker(user = "demo_data/dict-ch-user-demo.txt",
               stop_word = "demo_data/stopwords-ch-demo.txt")
segment(text, seg3) -> segment3

# my way
del.stop.words = c()
for (word in 1:length(segment2)){
  if (segment2[word] %in% segment3 == F){
    del.stop.words = c(segment2[word], del.stop.words)
  }
}
unique(del.stop.words) -> uni.del.stop.words
uni.del.stop.words

# use setdiff() function !!!!!!!
setdiff(segment2, segment3)

# ==================
# END
# ==================
# ==================
# Exercise 7.3
# ==================
seg4 <- worker(type = "tag",
               user = "demo_data/dict-ch-user-demo.txt", 
               stop_word = "demo_data/stopwords-ch-demo.txt",
               symbol = F)
segment(text, seg4) -> segment4

unname(segment4) -> segment4_unamed
names(segment4) -> segment4_names

long_string = c()
for (i in 1:length(segment4_unamed)){
  result = paste(segment4_unamed[i], segment4_names[i], sep = "/")
  long_string = c(result, long_string)
}
long_string
rev(long_string) -> long_string
paste(long_string, collapse = " ") -> long_string2
long_string2

# ==================
# END
# ==================
# ==================
# Exercise 7.4
# ==================
# I think we can use new_user_word() to correct the pos
# tag in the current pos tag data base. 
# For example, one of the result is "被/p 稱為/x 「/x 英國/ns 希望/v"
# 稱為 should be "V", but is detected as "x"
# new_user_word("稱為", v) to correct the verb's pos tag.

# ==================
# END
# ==================
# ==================
# Exercise 7.5
# ==================
# I think the biggest problem is pos tagging problem.
# Almost every result before "是" is the correct verb of the bei construction.
# a revised RE is '\\b被/p\\s([^/]+/[^\\s]+\\s)*?[是]+/v'
# We can filter out the result with "是" first.
# Then, we detect the correct verb before 是.
# Later, we extract the correct verb to the VERB column.

# ==================
# END
# ==================
# ==================
# Exercise 7.6
# ==================
# replace /pos with ""
apple_line %>%
  mutate(token_line = str_replace_all(line_tag, "/[a-z]+", "")) -> apple_line

# join tokenized context to result_bei
left_join(result_bei, apple_line) -> result_bei_context

result_bei_context %>%
  rename(context = token_line) %>%
  select(doc_id, filename, line_id, context, pat_bei, VERB) -> result_bei_context
result_bei_context
# ==================
# END
# ==================
# ==================
# Exercise 7.7
# ==================
apple_df <- readtext("demo_data/applenews10000.tar.gz", encoding = "UTF-8")
apple_df <- apple_df %>%
  filter(!str_detect(text,"^\\s*$")) %>%  ## remove empty documents
  mutate(filename = doc_id, ## save original text filenames
         doc_id = row_number()) ## create doc id
apple_df

my_seg <- worker(bylines = T,
                 user = "demo_data/dict-ch-user-demo.txt",
                 symbol = T)

## for POS tagging
my_seg_pos <- worker(
  type = "tag",
  bylines = F,
  user = "demo_data/dict-ch-user-demo.txt",
  symbol = T
)

CHUNK_DELIMITER <- "[，。！？；：\n]+"

apple_line <- apple_df %>%
  ## line tokenization
  unnest_tokens(
    output = line,
    input = text,
    token = function (x)
      str_split(x, CHUNK_DELIMITER)
  ) %>%
  group_by(doc_id) %>%
  mutate(line_id = row_number()) %>%
  ungroup

stopwords_chi <- readLines("demo_data/stopwords-ch-jiebar-zht.txt", encoding = "UTF-8")
stopwords_chi

my_seg_pos <- worker(
  type = "tag",
  bylines = F,
  user = "demo_data/dict-ch-user-demo.txt",
  symbol = T
)

tag_text <- function(x, jiebar) {
  segment(x, jiebar) %>% ## tokenize
    paste(names(.), sep = "/", collapse = " ") ## reformat output
}

apple_line %>%
  mutate(line_tag = map_chr(line, tag_text, my_seg_pos)) -> apple_line

# pattern_par_cons <- "\\b[^/]+/n\\s([^/]+/[^\\s]+\\s)*?[外內中]/[n|f]"
pattern_par_cons <- "\\b[^/]+/n\\s[外內中]/[n|f]"

apple_line %>%
  select(-line) %>% # `text` is the column with original raw texts
  unnest_tokens(
    output = par_cons,
    ## pattern name
    input = line_tag,
    ## original base linguistic unit
    token = function(x)
      str_extract_all(x, pattern = pattern_par_cons)
  ) -> result_par_cons
result_par_cons

result_par_cons %>%
  separate(par_cons, c("nonsense", "NOUN", "PREP"),
           sep = " ",
           remove = F,
           fill = "left") %>%
  select(doc_id, filename, line_id, par_cons, NOUN, PREP) -> result_par_cons

result_par_cons %>%
  mutate(LM = str_replace(NOUN, pattern = "([^/]+)/n", "\\1"),
         SP = str_replace(PREP, pattern = "([^/]+)/[n|f]", "\\1")) -> result_par_cons

left_join(result_par_cons, apple_line) %>%
  rename(context = token_line) %>%
  select(doc_id, filename, line_id, context, NOUN,
         PREP, LM, SP) -> result_par_cons_output
result_par_cons_output

# ==================
# END
# ==================
# ==================
# Exercise 7.8
# ==================
result_par_cons_output %>%
  group_by(SP, LM) %>%
  summarize(n = n()) %>%
  top_n(10, n) %>%
  arrange(SP, desc(n)) -> result_par_cons_top10
result_par_cons_top10

require(ggplot2)
result_par_cons_top10 %>%
  ggplot(aes(x = reorder(LM, n), y = n, fill = SP)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~SP, scales = "free_y") +
  xlab("Landmarks") +
  ylab("Frequency") +
  coord_flip()

# ==================
# END
# ==================
# ==================
# Exercise 7.9 Mandarin wordcloud
# ==================
library(wordcloud2)
result_par_cons_output %>%
  group_by(SP, LM) %>%
  summarize(n = n()) %>%
  top_n(100, n) -> result_top100
result_top100

# 中
result_top100 %>%
  filter(SP == "中") %>%
  ungroup() %>%
  select(LM, n) -> zon_freq
zon_freq %>%
  arrange(desc(n)) -> zon_freq
# wordcloud2(size = 0.4, shape = "diamond")
wordcloud2(zon_freq, size = 1.5, shape = "diamond")
# lettercloud doesn't work!!
letterCloud(zon_freq, size = 2, word = "\u4e2d")

# 內
result_top100 %>%
  filter(SP == "內") %>%
  ungroup() %>%
  select(LM, n) %>%
  arrange(desc(n)) -> nei_freq
nei_freq
wordcloud2(nei_freq, size = 1.5, shape = "diamond")
# lettercloud doesn't work!!
letterCloud(nei_freq, size = 2, word = "\u5167")

# 外
result_top100 %>%
  filter(SP == "外") %>%
  ungroup() %>%
  select(LM, n) %>%
  arrange(desc(n)) -> wai_freq
wai_freq
wordcloud2(wai_freq, size = 1, shape = "diamond")
# lettercloud doesn't work!!
letterCloud(nei_freq, size = 2, word = "\u5916")

# ==================
# END
# ==================
# ==================
# Exercise 7.10 wait 7.9 finished
# ==================


# ==================
# END
# ==================
# ==================
# Exercise 7.11
# ==================
apple_ngram_dist %>%
  filter(str_detect(ngram, "\\b以_|_以_|_以\\b")) %>%
  arrange(desc(dispersion))

# ==================
# END
# ==================
# ==================
# Exercise 7.12
# ==================
url <- "https://www.ptt.cc/bbs/Gossiping"
session(url) -> gossip.session
gossip.session %>%
  html_node("form") %>%
  html_form -> gossip.form

session_submit(
  x = gossip.session,
  form = gossip.form,
  submit = "yes"
) -> gossiping
gossiping

# get the last page index
page.latest <- gossiping %>%
  html_nodes("a") %>% 
  html_attr("href") %>%  
  str_subset("index[0-9]{2,}\\.html") %>% 
  str_extract("[0-9]+") %>% 
  as.numeric()
page.latest

last.page <- page.latest - 10
links.article.new <- c()

for (web.page in seq(from = page.latest, to = last.page, by = -1)){
  link <- str_c(url, "/index", web.page, ".html")
  # print(link)
  links.article <- gossiping %>%
    session_jump_to(link) %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>% 
    str_c("https://www.ptt.cc",.)
  # print(links.article)
  links.article.new <- c(links.article, links.article.new)
}
links.article.new

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

links.article.new
links.article.new %>%
  map(extract_article_push_tables) -> ptt_data

# merge all into one table
article.table.all <- ptt_data %>% 
  map(function(x) x$article.table) %>%
  bind_rows
article.table.all


push.table.all <- ptt_data %>%
  map(function(x) x$push.table) %>%
  bind_rows
push.table.all

# word tokenization
word_seg <- worker(bylines = F,
                   user = "demo_data/dict-ch-user.txt",
                   symbol = T,
                   type = "tag")

word_seg2 <- worker(bylines = T,
                    user = "demo_data/dict-ch-user.txt",
                    symbol = T,
                    type = "tag")


CHUNK_DELIMITER <- "[，。！？；：\n]+"

article.table.all %>%
  filter(!str_detect(title, "Re: ")) %>%
  filter(str_detect(title, "問卦|新聞|爆卦")) -> articles.table

tag_text <- function(x, jiebar) {
  segment(x, jiebar) %>% ## tokenize
    paste(names(.), sep = "/", collapse = " ") ## reformat output
}

articles.table %>%
  mutate(word_tag = map_chr(content, tag_text, word_seg)) -> articles.table2

articles.table2 %>%
  unnest_tokens(
    output = word_pos,
    input = word_tag,
    token = function(x)
      str_split(x, pattern = " ")
  ) %>%
  filter(str_detect(word_pos, pattern = ".+/[n|v]")) -> articles.table3

articles.table3 %>%
  separate(word_pos, into = c("word", "pos"), sep = "/") %>%
  group_by(word) %>%
  summarize(freq = n(), dispersion = n_distinct(author)) -> articles_word_freq
articles_word_freq

articles_word_freq %>%
  arrange(desc(freq))

articles_word_freq %>%
  arrange(desc(dispersion)) %>%
  filter(nchar(word) >= 2 & dispersion <= 5) %>%
  arrange(desc(freq)) -> articles_word_freq_dis

wordcloud2(articles_word_freq_dis, size = 0.5, shape = "diamond")
# ==================
# END
# ==================
# ==================
# Exercise 7.13
# ==================
articles.table$content %>%
  map(segment, word_seg2) %>%
  map(unlist) %>%
  as.tokens -> articles.tokens

docvars(articles.tokens) <- articles.table[c("title","author")]

kwic(articles.tokens, pattern = "篩")

# In my data, I find that the word "篩" doesn't always 
# indicate VERB. It may be a unit in a NP, for example,
# "快篩試劑" is seperate by "篩" in kwic() result.
# As a result, I think I should add the term "快篩試劑"
# in my user-dict.
# ==================
# END
# ==================
# ==================
# Exercise 7.14
# ==================
tokenizer_ngrams <-
  function(texts,
           jiebar,
           n = 2 ,
           skip = 0,
           delimiter = "_") {
    texts %>% ## chunks-based char vector
      segment(jiebar) %>% ## word tokenization 
      as.tokens %>% ## list to tokens
      tokens_ngrams(n, skip, concatenator = delimiter) %>%  ## ngram tokenization
      as.list ## tokens to list
  }

articles.table %>%
  unnest_tokens(
    ngram,
    content,
    token = function(x)
      tokenizer_ngrams(
        texts = x,
        jiebar = word_seg2,
        n = 4,
        skip = 0,
        delimiter = "_"
      )
  ) -> articles.four_ngram

articles.four_ngram %>%
  filter(nzchar(ngram)) %>%
  filter(!str_detect(ngram, "[^\u4E00-\u9FFF_]")) -> articles.four_ngram2

articles.table %>%
  unnest_tokens(
    ngram,
    content,
    token = function(x)
      tokenizer_ngrams(
        texts = x,
        jiebar = word_seg2,
        n = 5,
        skip = 0,
        delimiter = "_"
      )
  ) -> articles.five_ngram

articles.five_ngram %>%
  filter(nzchar(ngram)) %>%
  filter(!str_detect(ngram, "[^\u4E00-\u9FFF_]")) -> articles.five_ngram2

articles.table %>%
  unnest_tokens(
    ngram,
    content,
    token = function(x)
      tokenizer_ngrams(
        texts = x,
        jiebar = word_seg2,
        n = 6,
        skip = 0,
        delimiter = "_"
      )
  ) -> articles.six_ngram

articles.six_ngram %>%
  filter(nzchar(ngram)) %>%
  filter(!str_detect(ngram, "[^\u4E00-\u9FFF_]")) -> articles.six_ngram2

four_six_ngrams <- rbind(articles.four_ngram2, articles.five_ngram2, articles.six_ngram2)

four_six_ngrams %>%
  group_by(ngram) %>%
  summarize(freq = n(), dispersion = n_distinct(title)) %>%
  filter(freq >= 5) %>%
  arrange(desc(dispersion)) -> four_six_ngram_freq_dis
four_six_ngram_freq_dis

# ==================
# END
# ==================