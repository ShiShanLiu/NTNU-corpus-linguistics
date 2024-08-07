# CH10 Structured corpus
# ==================
# Exercise 10.1 
# ==================
library(tidyverse)
library(readtext)
library(tidytext)
library(quanteda)
library(ggplot2)
NCCU <- as_tibble(readtext("demo_data/corp-NCCU-SPOKEN.tar.gz", encoding = "UTF-8"))

NCCU %>%
    mutate(text = str_replace_all(text, "\n\t", " ")) %>%
    unnest_tokens(
        turn,
        text,
        token = function(x)
            str_split(x, pattern = "\n")
    ) -> NCCU_turns

NCCU_turns %>%
    filter(doc_id == "M001.cha")

NCCU_turns_meta <- NCCU_turns %>%
    filter(str_detect(turn, "^@"))

NCCU_turns_utterance <- NCCU_turns %>%
    filter(str_detect(turn, "^\\*")) %>% ## extract all lines starting with `*`
    group_by(doc_id) %>% ## create unique index for turns
    mutate(turn_id = row_number()) %>%
    ungroup %>%
    tidyr::separate(col="turn",          ## split SPID + Utterances
                    into = c("SPID", "turn"), 
                    sep = ":\t") %>%
    mutate(turn2 = turn %>%              ## Clean up utterances
               str_replace_all("\\([(\\.)0-9]+?\\)"," <PAUSE> ") %>% ## <PAUSE>
               str_replace_all("\\&\\=[a-z]+"," <EXTRALING> ") %>% ## <EXTRALING>
               str_replace_all("[\u2308\u2309\u230a\u230b]"," ") %>% ## overlapping talk tags
               str_replace_all("@[a-z:]+"," ") %>% ## code switching tags
               str_replace_all("\\s+"," ") %>% ## additional white-spaces
               str_trim())


NCCU_words <- NCCU_turns_utterance %>%
    select(-turn) %>% ## remove raw text column
    unnest_tokens(word,  ## name for new unit
                  turn2, ## name for old unit
                  token = function(x) ## self-defined tokenizer
                      str_split(x, "\\s+"))

NCCU_words %>% 
    head(100)

NCCU_words_freq <-NCCU_words %>%
    count(word, doc_id) %>% 
    group_by(word) %>%
    summarize(freq = sum(n), dispersion = n()) %>%
    arrange(desc(freq), desc(dispersion))

# unnest_tokens 覺得 in turn2
NCCU_turns_utterance %>%
    unnest_tokens(
        output = construction,
        input = turn2,
        token = function(x)
            str_extract_all(x, pattern = "[^ ]+ 覺得")
    ) %>%
    separate(
        col = construction,
        into = c("w1", "w2"),
        sep = "\\s+"
    ) -> zue_de_cons_df
zue_de_cons_df
    
zue_de_cons_df %>%
    group_by(w1) %>%
    summarize(freq = n()) %>%
    arrange(desc(freq)) -> zue_de_freq

zue_de_freq %>%
    top_n(10, freq) %>%
    ggplot(aes(x = reorder(w1, -freq), y = freq, fill = w1)) +
    geom_bar(stat = "identity", position = "dodge",
             show.legend = F) +
    labs(x = "Top 10 words",
         y = "Frequency")
# ==================
# END
# ==================
# ==================
# Exercise 10.2
# ==================
NCCU_words_freq %>%
    filter(str_detect(word, "^[^<a-z]")) %>%  ## remove annotations/tags
    select(word, freq) %>%
    top_n(150, freq) %>%  ## plot top 150 words
    mutate(freq = sqrt(freq)) %>% ## deal with Zipfian distribution
    wordcloud2(size=0.6)

NCCU_turns_utterance$turn2 %>%
    str_split("\\s+") %>%
    as.tokens -> NCCU_tokens

docvars(NCCU_tokens)<- NCCU_turns_utterance[,c("doc_id","SPID", "turn_id")] %>%
    mutate(Text = doc_id)

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

tokenizer_ngrams_v2 <-
    function(texts,
             n = 2 ,
             skip = 0,
             delimiter = "_") {
        texts %>% ## chunks-based char vector
            str_split(pattern = "\\s+") %>% ## word tokenization
            as.tokens %>% ## list to tokens
            tokens_ngrams(n, skip, concatenator = delimiter) %>%  ## ngram tokenization
            as.list ## tokens to list
    }

NCCU_bigrams <-  NCCU_turns_utterance %>%
    select(-turn) %>%  ## remove raw texts
    unnest_tokens(bigrams,  ## name for new unit
                  turn2, ## name for old unit
                  token = function(x) ## self-defined tokenizer
                      tokenizer_ngrams_v2(
                          texts = x,
                          n = 2))%>%
    filter(bigrams!="")

NCCU_bigrams_spid_freq <- NCCU_bigrams %>% 
    count(bigrams, SPID) %>% 
    group_by(bigrams) %>%
    summarize(freq = sum(n), dispersion = n()) %>%
    arrange(desc(freq), desc(dispersion)) %>%
    filter(!str_detect(bigrams, "<"))

NCCU_bigrams_spid_freq %>%
    top_n(100, freq)

# ==================
# END
# ==================
# ==================
# Exercise 10.3
# ==================
NCCU_bigrams_freq %>%
    filter(!str_detect(bigrams, "<")) %>% ## remove bigrams with para tags
    filter(dispersion >= 5) %>% ## set bigram dispersion cut-off
    rename(O11 = freq) %>%
    tidyr::separate(col="bigrams", ## split bigrams into two columns
                    c("w1", "w2"), 
                    sep="_") %>%
    mutate(R1 = NCCU_words_freq$freq[match(w1, NCCU_words_freq$word)],
           C1 = NCCU_words_freq$freq[match(w2, NCCU_words_freq$word)]) %>% 
    ## compute expected freq of bigrams
    mutate(E11 = (R1*C1)/sum(O11)) %>% 
    ## Compute lexical assoc
    mutate(MI = log2(O11/E11),
           t = (O11 - E11)/sqrt(E11)) %>%
    mutate_if(is.double, round,2)


# ---------

# NCCU_bigrams_freq %>%
#     filter(!str_detect(bigrams, "<")) %>% ## remove bigrams with para tags
#     filter(dispersion >= 5) %>% ## set bigram dispersion cut-off
#     rename(O11 = freq) %>%
#     tidyr::separate(col="bigrams", ## split bigrams into two columns
#                     c("w1", "w2"), 
#                     sep="_") %>% 
#     ## Obtain w1 w2 freqs
#     mutate(R1 = NCCU_words_freq$freq[match(w1, NCCU_words_freq$word)],
#            C1 = NCCU_words_freq$freq[match(w2, NCCU_words_freq$word)]) %>% 
#     ## compute expected freq of bigrams
#     mutate(E11 = (R1*C1)/sum(O11)) %>% 
#     ## Compute lexical assoc
#     mutate(MI = log2(O11/E11),
#            t = (O11 - E11)/sqrt(E11)) %>%
#     mutate_if(is.double, round,2) -> NCCU_collocations


NCCU_bigrams_freq %>%
    filter(!str_detect(bigrams, "<")) %>% 
    filter(dispersion >= 5) %>%
    rename(O11 = freq) %>%
    tidyr::separate(
        bigrams, 
        c("w1", "w2"), 
        sep = "_") %>%
    mutate(R1 = NCCU_words_freq$freq[match(w1, NCCU_words_freq$word)],
           C1 = NCCU_words_freq$freq[match(w2, NCCU_words_freq$word)]) %>%
    mutate(E11 = (R1*C1)/sum(O11)) %>%
    mutate(
        t = (O11 - E11)/sqrt(E11),
        MI = log2(O11/E11)
    ) %>%
    mutate(O12 = R1 - O11,
           O21 = C1 - O11, .before = R1) %>%
    mutate(N = sum(NCCU_bigrams_freq$freq)) %>%
    mutate(R2 = N - R1, .before = C1) %>%
    mutate(C2 = N - C1, .before = N) %>%
    mutate(O22 = C2 - O12, .before = R1) %>%
    # calculate E11 E12 E21 E22
    mutate(
           E12 = (R1*C2)/N,
           E21 = (R2*C1)/N,
           E22 = (as.double(R2)*as.double(C2))/N
    ) %>%
    mutate(
        a = O11 * if_else(is.na(log(O11) - log(E11)) == T, 0, log(O11) - log(E11)),
        b = O12 * if_else(is.na(log(O12) - log(E12)) == T, 0, log(O12) - log(E12)),
        c = O21 * if_else(is.na(log(O21) - log(E21)) == T, 0, log(O21) - log(E21)),
        d = O22 * if_else(is.na(log(O22) - log(E22)) == T, 0, log(O22) - log(E22))
    ) %>%
    mutate(
        LLR = 2 * (a + b + c + d)
    ) %>%
    select(w1, w2, O11, dispersion, LLR, MI, t) -> NCCU_collocations_tb


NCCU_collocations_tb %>%
    arrange(desc(LLR)) -> NCCU_collocations_tb

# ==================
# END
# ==================
# ==================
# Exercise 10.4
# ==================
NCCU_ngrams <- NCCU_turns_utterance %>%
    select(-turn) %>%  ## remove raw texts
    unnest_tokens(ngram,  ## name for new unit
                  turn2,  ## name for old unit
                  token = function(x) ## self-defined tokenizer
                      tokenizer_ngrams_v2(
                          texts = x,
                          n = 4))%>%
    filter(ngram != "")


NCCU_ngrams %>%
    count(ngram, doc_id) %>%
    group_by(ngram) %>%
    summarize(freq = sum(n), dispersion = n()) %>%
    arrange(desc(dispersion), desc(freq)) %>%
    ungroup %>%
    filter(!str_detect(ngram,"<")) -> NCCU_ngrams_freq

NCCU_meta <- NCCU_turns_meta %>%
    filter(str_detect(turn, "^@(id)")) %>% ## extract all ID lines
    separate(col="turn", ## split SP info
             into=str_c("V",1:11, sep=""), 
             sep = "\\|") %>%
    select(doc_id, V2, V3, V4, V5, V7, V10) %>% ## choose relevant info
    mutate(DOC_SPID = str_c(doc_id, V3, sep="_")) %>% ## unique SPID
    rename(AGE = V4, ## renaming columns
           GENDER = V5, 
           GROUP = V7, 
           RELATION = V10,
           LANG = V2) %>%
    select(-V3) %>% ## remove irrelevant column
    mutate(AGE = as.integer(str_replace(AGE,";","")))

NCCU_bigrams %>% ## bigram-based DF
    filter(!str_detect(bigrams, "<")) %>% ## remove bigrams with para tags
    mutate(DOC_SPID = str_c(doc_id,  ## Create `DOC_SPID`
                            str_replace_all(SPID, "\\*", ""),
                            sep = "_")) %>%
    left_join(NCCU_meta, ## Link to meta DF
              by = c("DOC_SPID" = "DOC_SPID")) %>%
    mutate(AGE_GROUP = cut(  ## relevel AGE
        AGE,
        breaks = c(0, 20, 40, 60),
        label = c("Below_20", "AGE_20_40", "AGE_40_60")
    )) -> NCCU_bigrams_with_meta

NCCU_bigrams_by_age <-  NCCU_bigrams_with_meta %>%
    count(bigrams, AGE_GROUP, DOC_SPID) %>%
    group_by(bigrams, AGE_GROUP) %>%
    summarize(freq = sum(n),  ## frequency
              dispersion = n()) %>% ## dispersion (speakers)
    filter(dispersion >= 5) %>% ## dispersion cutoff
    ungroup

NCCU_bigrams_by_age %>%
    filter(dispersion >= 5) %>%
    group_by(AGE_GROUP) %>%
    summarize(bigram_tokens = sum(freq)) %>%
    mutate(total_bigrams = sum(bigram_tokens)) %>%
    ungroup -> age_bigram_freq
age_bigram_freq

NCCU_bigrams_by_age %>%
    filter(dispersion >= 5) %>%
    left_join(age_bigram_freq, by = c("AGE_GROUP" = "AGE_GROUP")) -> test


    #        X2 = (bigram_tokens - expected_frq)^2 / expected_frq)

test %>%
    group_by(bigrams) %>%
    mutate(expected_frq = sum(freq) * (bigram_tokens / sum(bigram_tokens)))

test %>%
    group_by(bigrams) %>%
    mutate(test = sum(freq)) -> test2




# ==================
# END
# ==================
# ==================
# Exercise 10.5
# ==================


# ==================
# END
# ==================