# CH11 Vector Space Representation
# ==================
# Exercise 12.1
# ==================
library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.textplots)
library(dplyr)
library(readtext)
library(jiebaR)
library(stringr)
library(tidytext)
corp_us <- data_corpus_inaugural
corp_us_tokens <- tokens(corp_us)
corp_us_dfm <- dfm(corp_us_tokens)

corp_us_tokens %>%
    tokens_ngrams(n = 3) %>%
    dfm() %>%
    dfm_wordstem() -> corp_us_trigram_dfm

corp_us_trigram_dfm %>%
    convert(to = "data.frame") -> corp_us_trigram_df
corp_us_trigram_df %>% head(10)
# ==================
# END
# ==================
# ==================
# Exercise 12.2
# ==================
?dfm_trim()

?dfm_weight()

?dfm_tfidf()
# ==================
# END
# ==================
# ==================
# Exercise 12.3
# ==================
corp_us_tokens <- tokens(
    corp_us,
    what = "word",
    remove_punct = TRUE,
    remove_symbol = TRUE,
    remove_numbers = TRUE
)
corp_us_tokens %>% head(10)

## window-based FCM
corp_fcm_win <- fcm(
    corp_us_tokens, ## tokens
    context = "window", ## context type
    window = 1) # window size

## Document-based FCM
corp_fcm_doc <- fcm(
    corp_us_tokens,
    context = "document")

## Create FCM from DFM
corp_fcm_dfm <- fcm(corp_us_dfm_trimmed)

## Create window size 5 FCM
corp_fcm_win_5 <- fcm(corp_us_tokens,
                      context = "window",
                      window = 5)

## Remove stopwords
corp_fcm_win_5_select <- corp_fcm_win_5 %>%
    fcm_remove(
        pattern = stopwords(),
        case_insensitive = T
    )
corp_fcm_win_5_select

## Subset fcm for plot
fcm4network <- fcm_select(corp_fcm_win_5_select,
                          pattern = names(corp_fcm_win_5_top50), 
                          selection = "keep",
                          valuetype = "fixed")
fcm4network

convert(fcm4network, to = "data.frame") -> test
colnames(test) -> test_colnames

setdiff(test_colnames, names(corp_fcm_win_5_top50))
intersect(test_colnames, names(corp_fcm_win_5_top50))

# After cross-examining the data, it seems that 
# fcm_selsct is not case sensitive because some extra
# columns are the uppercases of the variable names(corp_fcm_win_5_top50).
# If these uppercase words are in the data, they will be 
# taken into consideration for calculatiing .

# ==================
# END
# ==================
# ==================
# Exercise 12.4
# ==================
corp_us %>%
    tokens(
        # remove_punct = T,
        # remove_symbols = T,
        # remove_numbers = T
    ) -> corp_us_tokens

corp_us_tokens %>%
    tokens_ngrams(
        n = 3,
        concatenator = "_"
    ) -> corp_us_trigrams

corp_us_trigrams %>%
    dfm() -> corp_us_trigrams_dfm

corp_us_trigrams_dfm %>%
    dfm_remove(stopwords("en")) %>%
    dfm_trim(
        min_termfreq = 2,
        max_termfreq = NULL,
        termfreq_type = "count",
        min_docfreq = 2,
        max_docfreq = NULL,
        docfreq_type = "count"
    ) -> corp_us_trigram_trimmed

# filter name
convert(corp_us_trigram_trimmed, to = "data.frame") -> corp_us_trigrams_df

colnames(corp_us_trigrams_df) -> trigrams_colnames

trigrams_colnames %>%
    str_extract(pattern = "^[\\w]+_\\w+_\\w+|doc_id") %>%
    na.omit() -> trigrams_colnames_pattern

trigrams_colnames_pattern %>% head(10)

column_names <- names(corp_us_trigrams_df)[names(corp_us_trigrams_df) %in% trigrams_colnames_pattern]
length(column_names)

# select dfm columns
dfm_select(corp_us_trigram_trimmed, pattern = trigrams_colnames_pattern) -> corp_us_trigram_trimmed_names
nfeat(corp_us_trigram_trimmed_names)

corp_us_trigram_trimmed_names %>%
    dfm_tfidf() -> corp_us_trigram_tfidf

corp_us_trigram_cosine <- textstat_simil(corp_us_trigram_tfidf, method = "cosine")
corp_us_trigram_cosine %>% head(5)

corp_us_trigram_hist_cosine <- (1 - corp_us_trigram_cosine) %>%
    as.dist %>%
    hclust

plot(as.dendrogram(corp_us_trigram_hist_cosine, hang = 0.1),
     horiz = F,
     ylab = "Height",
     ylim = c(0.6, 1.0),
     main = "Trigram-based Dendrogram of US Presidential Addresses")

# ==================
# END
# ==================
# ==================
# Exercise 12.5
# ==================
corp_us %>%
    tokens(
        remove_punct = T,
        remove_numbers = T,
        remove_symbols = T
    ) -> corp_us_tokens

fcm(
    corp_us_tokens,
    context = "window",
    window = 5
) -> corp_us_tokens_fcm

corp_us_tokens_fcm %>%
    fcm_remove(
        pattern = stopwords("en")
    ) -> corp_us_fcm_win5_select

corp_fcm_win5_top50 <- topfeatures(corp_us_fcm_win5_select, 50)
corp_fcm_win5_top50

corp_us_fcm_win5_select %>%
    fcm_keep(pattern = names(corp_fcm_win5_top50)) -> fcm5network
fcm5network

corp_us_fcm_win5_select[names(corp_fcm_win5_top50),] -> fcm5cluster

textstat_simil(fcm5cluster, method = "cosine") -> fcm5cluster_cosine

(1 - fcm5cluster_cosine) %>%
    as.dist() %>%
    hclust() -> fcm5hclust

plot(as.dendrogram(fcm5hclust, hang = 0.1),
     horiz = F,
     ylab = "Height",
     ylim = c(0.5, 1.0), 
     main = "Lexical Similarity of Top 50 Featuress"
     )
# ==================
# END
# ==================
# ==================
# Exercise 12.6
# ==================
seg <- worker(bylines = T ,symbol = F)
new_words <- c("馬英九", "英九", "蔣中正", 
               "蔣經國", "李登輝", "登輝" ,
               "陳水扁", "阿扁", "蔡英文")
new_user_word(seg, new_words)

tw_president <- readtext("demo_data/TW_President.tar.gz")
tw_president

tw_president %>%
    mutate(text_id = row_number()) -> tw_president

corp_tw <- corpus(tw_president)
corp_tw

corp_tw %>%
    map(segment, seg) %>%
    map(unlist) %>%
    as.tokens -> tw_tokens

tw_tokens %>%
    dfm() %>%
    dfm_trim(
        min_termfreq = 5,
        max_termfreq = NULL,
        termfreq_type = "count",
        min_docfreq = 3,
        max_docfreq = NULL,
        docfreq_type = "count"
    ) -> tw_dfm_trimmed
tw_dfm_trimmed

nfeat(tw_dfm_trimmed)

tw_dfm_tfidf <- dfm_tfidf(tw_dfm_trimmed)

corp_tw_cosine <- textstat_simil(tw_dfm_tfidf, method = "cosine")

(1 - corp_tw_cosine) %>%
    as.dist() %>%
    hclust() -> corp_tw_hclust

plot(as.dendrogram(corp_tw_hclust, hang = 0.05),
     horiz = F,
     center = T,
     # dLeaf = 1/1000,
     ylab = "Height",
     ylim = c(0.3, 0.9),
     main = "Ngram-based Dendrogram of TW Presidential Addresses"
)

# ==================
# END
# ==================
# ==================
# Exercise 12.7
# ==================
tw_tokens %>%
    fcm(context = "window", window = 2) -> corp_tw_fcm
corp_tw_fcm

ch_stopwords <- readLines("demo_data/stopwords-ch.txt", encoding = "UTF-8")

corp_tw_fcm %>%
    fcm_remove(
        pattern = ch_stopwords
    ) -> corp_tw_fcm_clean
corp_tw_fcm_clean
top50_tw_fcm <- topfeatures(corp_tw_fcm_clean, 50)

twfcm2network <- fcm_select(
    corp_tw_fcm_clean,
    pattern = names(top50_tw_fcm),
    selection = "keep",
    valuetype = "fixed"
)
twfcm2network
# semantic network of top 50 tw features
require(scales)

textplot_network(
    twfcm2network,
    # min_freq = 5, 
    vertex_labelcolor = c("grey40"),
    vertex_labelsize = 2 * scales::rescale(rowSums(
        twfcm2network + 1), to = c(1.5, 5))
)

top100_tw_fcm <- topfeatures(corp_tw_fcm_clean, 100)

twfcm2cluster <- corp_tw_fcm_clean[names(top100_tw_fcm),]
twfcm2cluster

twfcm2cluster_cosine <- textstat_simil(twfcm2cluster, method = "cosine")
(1 - twfcm2cluster_cosine) %>%
    as.dist() %>%
    hclust() -> twfcm2_hclust

# library(dendextend)
ggdendrogram(
    twfcm2_hclust,
    theme_dendro = T,
) +
    labs(title = "Lexical Similarity of Top 100 Features")

# ==================
# END
# ==================