# CH11 XML
# ==================
# Exercise 11.1 
# ==================
library(tidyverse)
library(readtext)
library(tidytext)
library(quanteda)
library(xml2)
corp_bnc<-read_xml(x = "demo_data/corp-bnc-spoken2014-sample/S2A5-tgd.xml",
                   encoding = "UTF-8")
corp_bnc

root<- xml_root(corp_bnc)

## Name of xml root
xml_name(root)

all_utterances <- xml_find_all(root, xpath = "//u")

all_tokens_nodes_info <- function(all_utterances){
    # all_tokens_nodes_tb <- tibble()
    tokens_list <- vector(mode = "list", length = length(all_utterances))
    for (u in 1:length(all_utterances)){
        all_childrens <- xml_children(all_utterances[u])
        tokens_tb <- tokens_level_info(all_childrens)
        nodes_tb <- node_level_info(all_utterances[u])
        # tokens_nodes_tb <- cbind(tokens_tb, nodes_tb)
        tokens_list[[u]] <- tokens_tb
        tokens_list[[u]] <- inner_join(tokens_list[[u]], nodes_tb, by = character())
        # all_tokens_nodes_tb <- rbind(all_tokens_nodes_tb, tokens_nodes_tb)
    }
    result <- bind_rows(tokens_list)
    # return(all_tokens_nodes_tb)
    return(result)
}
all_tokens_nodes_info(all_utterances)


# all_tokens_nodes_tb <- tibble()
# for (u in 1: length(all_utterance)){
#     all_childrens <- xml_children(all_utterance[u])
#     tokens_tb <- tokens_level_info(all_children)
#     nodes_tb <- node_level_info(all_utterance[u])
#     tokens_nodes_tb <- cbind(tokens_tb, nodes_tb)
#     all_tokens_nodes_tb <- rbind(all_tokens_nodes_tb, tokens_nodes_tb)
# }
# all_tokens_nodes_tb


# all_tokens_nodes_info(all_utterances) -> result
# result

tokens_level_info <- function(children){
    # name
    token_names_all <- xml_name(children)
    # text
    token_texts_all <- xml_text(children)
    # pos
    token_pos_all <- xml_attr(children, attr="pos")
    # lemma
    token_lemma_all <- xml_attr(children, attr="lemma")
    # class
    token_class_all <- xml_attr(children, attr="class")
    # usas
    token_usas_all <- xml_attr(children, attr="usas")
    # dur
    dur_all <- xml_attr(children, attr = "dur")
    # desc
    desc_all <- xml_attr(children, attr = "desc")
    # coalesce dur and desc
    token_notes_all <- coalesce(dur_all, desc_all)
    
    # replace notes
    token_notes_all %>%
        str_replace("long", 'dur="long"') %>%
        str_replace("short", 'dur="short"') %>%
        str_replace("laugh", 'desc="laugh"') -> token_notes_all
    # 
    
    cur_utterance_all_df <- tibble(
        name = token_names_all,
        text = token_texts_all,
        pos = token_pos_all,
        lemma = token_lemma_all,
        class = token_class_all,
        usas = token_usas_all,
        notes = token_notes_all
    )
    return(cur_utterance_all_df)
}
all_childrens <- xml_children(all_utterances)

# 
# length(all_utterances[[1]])
# 
# token_names_all <- xml_name(all_childrens[1])
# token_names_all
# 
# token_texts_all <- xml_text(all_childrens[1])
# token_texts_all
# 
# token_pos_all <- xml_attr(all_childrens[1], attr="pos")
# token_pos_all
# 
# token_lemma_all <- xml_attr(all_childrens[1], attr="lemma")
# token_lemma_all
# 
# token_class_all <- xml_attr(all_childrens[1], attr="class")
# token_class_all
# 
# token_usas_all <- xml_attr(all_childrens[1], attr="usas")
# token_usas_all
# 
# token_notes_all <- xml_attr(all_childrens[1], attr = c("dur", "desc"))
# token_notes_all
# 
# token_notes_all %>%
#     str_replace("long", 'dur="long"') %>%
#     str_replace("short", 'dur="short"') %>%
#     str_replace("laugh", 'desc="laugh"') -> token_notes_all
# token_notes_all
# 
# cur_utterance_all_df <- tibble(
#     name = token_names_all,
#     text = token_texts_all,
#     pos = token_pos_all,
#     lemma = token_lemma_all,
#     class = token_class_all,
#     usas = token_usas_all,
#     notes = token_notes_all
# )
# cur_utterance_all_df

node_level_info <- function(utterance){
    node_level_attrs_all <- xml_attrs(utterance) %>%
        unlist
    
    n_all = c()
    who_all = c()
    trans_all = c()
    whoconfidence_all = c()
    
    for (i in 1:length(node_level_attrs_all)){
        if ((i %% 4) == 1){
            n_all = c(n_all, node_level_attrs_all[i])
        }else if ((i %% 4) == 2){
            who_all = c(who_all, node_level_attrs_all[i])
        }else if ((i %% 4) == 3){
            trans_all = c(trans_all, node_level_attrs_all[i])
        }else {
            whoconfidence_all = c(whoconfidence_all, node_level_attrs_all[i])
        }
    }
    
    names_tb <- tibble(
        n = n_all,
        who = who_all,
        trans = trans_all,
        whoconfidence = whoconfidence_all,
        xml_id = "S2A5-tgd.xml"
    )
    return(names_tb)
}

# 
# node_level_attrs_all <- xml_attrs(all_utterances[1]) %>%
#     unlist
# node_level_attrs_all
# 
# n_all = c()
# who_all = c()
# trans_all = c()
# whoconfidence_all = c()
# 
# for (i in 1:length(node_level_attrs_all)){
#     if ((i %% 4) == 1){
#         n_all = c(n_all, node_level_attrs_all[i])
#     }else if ((i %% 4) == 2){
#         who_all = c(who_all, node_level_attrs_all[i])
#     }else if ((i %% 4) == 3){
#         trans_all = c(trans_all, node_level_attrs_all[i])
#     }else {
#         whoconfidence_all = c(whoconfidence_all, node_level_attrs_all[i])
#     }
# }
# 
# n_all
# who_all
# trans_all
# whoconfidence_all
# 
# names_tb <- tibble(
#     n = n_all,
#     who = who_all,
#     trans = trans_all,
#     whoconfidence = whoconfidence_all,
#     xml_id = "S2A5-tgd.xml"
# )


# ==================
# END
# ==================
# ==================
# Exercise 11.2
# ==================
read_xml_bnc2014 <- function(xmlfile){
    corp_bnc<-read_xml(x = xmlfile, encoding = "UTF-8")
    root<- xml_root(corp_bnc)
    all_utterances <- xml_find_all(root, xpath = "//u")
    result <- all_tokens_nodes_info(all_utterances)
    return(result)
    # return(all_tokens_nodes_tb)
}
bnc_flist[1]
length(bnc_flist)
word_df <- read_xml_bnc2014(
    xmlfile = bnc_flist[11])

word_df %>%
    filter(n %in% c("1","36"))
# 3, 30, 22, 21, 
# ==================
# END
# ==================
# ==================
# Exercise 11.3
# ==================
bnc_text_meta <- read_tsv(
    file = "demo_data/corp-bnc-spoken2014-metadata/bnc2014spoken-textdata.tsv",
    col_names = FALSE,
    locale = locale(encoding = "UTF-8")
)

bnc_text_meta_names <- read_tsv(
    file = "demo_data/corp-bnc-spoken2014-metadata/metadata-fields-text.txt",
    skip = 1,
    col_names =  TRUE,
    locale = locale(encoding = "UTF-8")
)
names(bnc_text_meta) <- c("textid", bnc_text_meta_names$`XML tag`)
bnc_text_meta
sum(bnc_text_meta$rec_length)

library(lubridate)
hms(bnc_text_meta$rec_length) -> rec_length_time
rec_length_seconds <- period_to_seconds(rec_length_time)
sum(rec_length_seconds) -> rec_length_sum
seconds_to_period(rec_length_sum) -> rec_length_total
rec_length_total
paste("Total duration of the BNC2014 subset:", rec_length_total)

mean(rec_length_seconds) %>%
    seconds_to_period() %>%
    round(digits = 2)-> rec_length_mean
paste("On average, each transcript is about", rec_length_mean)
# ==================
# END
# ==================
# ==================
# Exercise 11.4
# ==================
bnc_sp_meta <- read_tsv("demo_data/corp-bnc-spoken2014-metadata/bnc2014spoken-speakerdata.tsv", 
                        col_names = FALSE,
                        locale = locale(encoding = "UTF-8"))
bnc_sp_meta

bnc_sp_meta_names <- read_tsv("demo_data/corp-bnc-spoken2014-metadata/metadata-fields-speaker.txt", 
                              skip = 1, 
                              col_names =  TRUE,
                              locale = locale(encoding = "UTF-8"))
bnc_sp_meta_names

names(bnc_sp_meta) <- c("spid", bnc_sp_meta_names$`XML tag`)
bnc_sp_meta

bnc_sp_meta %>%
    group_by(agerange, gender) %>%
    summarize(freq = n()) %>%
    filter(agerange != "Unknown")-> bnc_age_gender_freq


bnc_age_gender_freq %>%
    ggplot(aes(x = agerange, y = freq, fill = gender)) +
    geom_bar(stat = "identity", position = "dodge")

# ==================
# END
# ==================
# ==================
# Exercise 11.5
# ==================
corp_bnc_adj_gender <- corp_bnc_token_df %>%  ## token-based DF
    filter(str_detect(pos, "^(JJ[RT]?$)")) %>%  ## adjectives
    left_join(bnc_sp_meta, by = c("who"="spid")) %>% ## link to metadata
    mutate(gender = factor(gender, levels=c("F","M"))) %>% ## factor gender
    filter(!is.na(gender))

freq_adj_by_gender <- corp_bnc_adj_gender %>%
    count(gender, lemma, sort = T)
freq_adj_by_gender

freq_adj_by_gender %>%
    pivot_wider(
        names_from = gender,
        values_from = n,
        values_fill = 0
    )  -> gender_contigency_tb

gender_contigency_tb %>%
    rename(a = `F`, b = `M`) %>%
    mutate(c = sum(a) - a, d = sum(b) - b) -> gender_contigency_tb

gender_contigency_tb %>%
    mutate(
        a.exp = ((a+b)*(a+c))/(a+b+c+d),
        b.exp = ((a+b)*(b+d))/(a+b+c+d),
        c.exp = ((c+d)*(a+c))/(a+b+c+d),
        d.exp = ((c+d)*(b+d))/(a+b+c+d),
        G2 = 2*(a*log(a/a.exp) + b*log(b/b.exp) + c*log(c/c.exp) + d*log(d/d.exp))
    ) -> gender_keyness_tb

gender_keyness_tb %>%
    mutate(preference = ifelse(a > a.exp, "FEMALE", "MALE")) %>%
    select(lemma, preference, G2, everything()) %>%
    arrange(preference, desc(G2)) -> gender_keyness_tb
gender_keyness_tb

require(wordcloud2)
gender_keyness_tb %>%
    filter(preference == "FEMALE") %>%
    top_n(50,G2) %>%
    select(lemma, G2) %>%
    wordcloud2(size = 1, minRotation = -pi/2, maxRotation = -pi/2)

gender_keyness_tb %>%
    filter(preference == "MALE") %>%
    top_n(50,G2) %>%
    select(lemma, G2) %>%
    wordcloud2(size = 1, minRotation = -pi/2, maxRotation = -pi/2)

# ==================
# END
# ==================
# ==================
# Exercise 11.6
# ==================
corp_bnc_token_df <- read_csv("demo_data/data-corp-token-bnc2014.csv",
                              locale = locale(encoding = "UTF-8"))

corp_bnc_utterance_df <- corp_bnc_token_df %>%
    select(-trans, -whoconfidence) %>% ## remove irrelevant columns
    group_by(xml_id, n, who) %>% ## collapse utterance-level columns
    nest %>% ## nest each utterance's token DF into sub-DF
    ungroup

extract_wordtag_string <- function(u_df){
    utterance_df <- u_df
    cur_text <-utterance_df$text  ## all word forms
    cur_pos <- utterance_df$pos ## all pos
    tag_index <-which(is.na(cur_text))  ## check paralinguistic index
    if(length(tag_index)>0){ ## retrieve paralinguistic
        cur_pos[tag_index]<- cur_text[tag_index] <- paste0("<",utterance_df$name[tag_index],">", sep ="")
    }
    ## concatenate and output
    paste(cur_text, cur_pos, sep = "_", collapse=" ")  
} ## endfunc

## Example use
extract_wordtag_string(corp_bnc_utterance_df$data[[1]])


corp_bnc_utterance_df %>%
    mutate(utterance = map_chr(data, extract_wordtag_string)) %>%
    select(-data) -> corp_bnc_utterance_df
corp_bnc_utterance_df

corp_bnc_pat_gender <- corp_bnc_utterance_df %>%
    unnest_tokens(pattern,  ## new unit
                  utterance, ## old unit
                  token = function(x)
                      str_extract_all(x, "[^_ ]+_RG [^_ ]+_JJ"),
                  to_lower = FALSE) %>%
    left_join(bnc_sp_meta, by = c("who" = "spid"))

freq_pat_by_gender <- corp_bnc_pat_gender %>%
    mutate(pattern = str_replace_all(pattern, "_[^_ ]+","")) %>% # remove pos tags
    select(gender, pattern) %>%
    count(gender, pattern, sort=T) 
freq_pat_by_gender

corp_bnc_pat_gender %>%
    mutate(pattern = str_replace_all(pattern, "_[^_ ]+","")) %>%
    group_by(gender, pattern) %>%
    summarize(n = n(), dispersion = n_distinct(who)) %>%
    arrange(gender, desc(n)) -> corp_gender_freq
corp_gender_freq %>% head(10)

corp_gender_freq %>%
    pivot_wider(
        names_from = gender,
        values_from = c(n, dispersion),
        values_fill = 0
    ) %>%
    rename(freq_F = n_F, freq_M = n_M) -> corp_gender_freq_disp
corp_gender_freq_disp %>% head(10)

# ==================
# END
# ==================
# ==================
# Exercise 11.7
# ==================
corp_bnc_pat_gender %>%
    mutate(pattern = str_replace_all(pattern, "_[^_ ]+","")) %>%
    separate(
        col = pattern,
        into = c("ADV", "ADJ"),
        sep = "\\s+"
    ) %>%
    group_by(gender, ADJ) %>%
    summarize(freq = n(), dispersion = n_distinct(who)) %>%
    arrange(desc(freq)) %>%
    ungroup()-> bnc_gender_adj_freq

bnc_gender_adj_freq %>% head(10)

bnc_gender_adj_freq %>%
    pivot_wider(
        names_from = gender,
        values_from = c(freq, dispersion),
        values_fill = 0
    ) -> bnc_gender_adj_freq_wider

bnc_gender_adj_freq_wider %>%
    arrange(desc(freq_F)) %>%
    head(10)

sum(bnc_gender_adj_freq_wider$freq_M)

bnc_gender_adj_freq_wider %>%
    # filter(dispersion_F >= 2 | dispersion_M >= 2) %>%
    rename(a = freq_F, b = freq_M) %>%
    mutate(c = sum(a) - a, d = sum(b) - b) %>%
    mutate(
    # ch4 : if_else(is.na(log(O11) - log(E11)) == T, 0, log(O11) - log(E11)
        a.exp = ((a+b)*(a+c))/(a+b+c+d),
        b.exp = ((a+b)*(b+d))/(a+b+c+d),
        c.exp = ((c+d)*(a+c))/(a+b+c+d),
        d.exp = ((c+d)*(b+d))/(a+b+c+d)) -> bnc_gender_adj_contigency_tb

bnc_gender_adj_contigency_tb %>%
    mutate(a.test = if_else(is.nan(a * log(a/a.exp)), 0, a * log(a/a.exp)),
           b.test = if_else(is.nan(b* log(b/b.exp)), 0, b* log(b/b.exp)),
           c.test = if_else(is.nan(c* log(c/c.exp)), 0, c* log(c/c.exp)),
           d.test = if_else(is.nan(d* log(d/d.exp)), 0, d* log(d/d.exp))) -> bnc_gender_adj_contigency_tb
      
bnc_gender_adj_contigency_tb %>%
    mutate(G2 = 2*(a.test + b.test + c.test + d.test)) -> bnc_gender_adj_keyness_tb

bnc_gender_adj_keyness_tb %>% head(10)

bnc_gender_adj_keyness_tb %>%
    mutate(preference = ifelse(a > a.exp, "FEMALE", "MALE")) %>%
    select(ADJ, preference, G2, everything()) %>%
    arrange(preference, desc(G2)) -> bnc_gender_adj_keyness_tb


bnc_gender_adj_keyness_tb %>%
    group_by(preference) %>%
    top_n(10, G2)

# ==================
# END
# ==================
# ==================
# Exercise 11.8
# ==================
bnc_sample_list <- dir("demo_data/corp-bnc-spoken2014-sample/", 
                       full.names = T)
corp_bnc_sample <- map(bnc_sample_list, read_xml_bnc2014)

corp_bnc_sample_token_df <- bind_rows(corp_bnc_sample)
corp_bnc_sample_token_df %>% head(10)
write_csv(corp_bnc_sample_token_df,
          file = "demo_data/data-corp-sample-token-bnc2014.csv")

corp_bnc_sample_token_df %>%
    select(-trans, -whoconfidence) %>%
    group_by(xml_id, n, who,) %>%
    nest %>%
    ungroup -> corp_bnc_sample_utterance_df
corp_bnc_sample_utterance_df %>% head(10)

corp_bnc_sample_utterance_df %>%
    mutate(utterance = map_chr(data, extract_wordtag_string)) %>%
    select(-data) -> corp_bnc_sample_utterance_df

corp_bnc_sample_utterance_df$utterance %>% head(10)

corp_bnc_sample_utterance_df %>%
    unnest_tokens(
        pattern,
        utterance,
        token = function(x)
            str_extract_all(x, pattern = "[I_]+_PPIS1 [^_ ]+_[A-Z0-9]+ [^_ ]+_VV[A-Z0-9]|[I_]+_PPIS1 [^_ ]+_VV[A-Z0-9]|[I_]+_PPIS1 [^_ ]+_[A-Z0-9]+ [^_ ]+_[A-Z0-9]+ [^_ ]+_VV[A-Z0-9]"),
        to_lower = F
    ) %>%
    left_join(bnc_sp_meta, by = c("who" = "spid")) -> corp_bnc_sample_gender
corp_bnc_sample_gender %>% head(10)

corp_bnc_sample_gender %>%
    unnest_tokens(
        VERB,
        pattern,
        token = function(x)
            str_extract_all(x, pattern = "[^_ ]+_VV[A-Z0-9]")
    ) -> corp_bnc_sample_gender
corp_bnc_sample_gender %>% head(10)

corp_bnc_sample_gender %>%
    select(gender, VERB) %>%
    separate(
        col = VERB,
        into = c("VERB", "annotation")
    ) %>%
    select(gender, VERB) -> corp_bnc_sample_gender_verb


corp_bnc_sample_gender_verb %>%
    group_by(gender, VERB) %>%
    summarize(n = n()) %>%
    ungroup %>%
    arrange(desc(n)) %>%
    pivot_wider(
        names_from = gender,
        values_from = n,
        values_fill = 0
    ) -> corp_bnc_sample_gender_verb_wide
corp_bnc_sample_gender_verb_wide %>% head(10)

corp_bnc_sample_gender_verb_wide %>%
    top_n(20) %>%
    pivot_longer(
        cols = c("F", "M"),
        names_to = "gender",
        values_to = "freq"
    ) %>%
    ggplot(aes(x = reorder(VERB, freq), y = freq, fill = gender)) +
    geom_col(position = "dodge", show.legend = F) +
    facet_wrap(~gender, scales = "free") +
    coord_flip() +
    labs(
        x = "Verb",
        y = "Frequency",
        title = "Cooccurring Verbs with I by Genders"
    )

# ==================
# END
# ==================
# ==================
# Exercise 11.9
# ==================
corp_bnc_sample_utterance_df

corp_bnc_sample_utterance_df %>% 
    mutate(UTTERANCE = map_chr(utterance, str_replace_all, pattern = "_[A-Z0-9]+", replacement = "")) %>%
    select(-utterance) -> corp_bnc_sample_UTTERANCE_df
corp_bnc_sample_UTTERANCE_df %>% head(10)

corp_bnc_sample_UTTERANCE_df %>%
    unnest_tokens(
        output = four_grams,
        input = UTTERANCE,
        token = "ngrams",
        n = 4
    ) %>%
    mutate(four_grams = map_chr(four_grams, str_replace_all, pattern = "\\s+", replacement = "_")) -> corp_bnc_sample_UTTERANCE_four_grams

corp_bnc_sample_UTTERANCE_four_grams %>% 
    left_join(bnc_sp_meta, by = c("who" = "spid")) -> corp_bnc_sample_UTTERANCE_four_grams

corp_bnc_sample_UTTERANCE_four_grams %>%
    group_by(gender, four_grams) %>%
    summarize(freq = n(), dispersion = n_distinct(who)) %>%
    filter(!is.na(four_grams))-> corp_bnc_sample_UTTERANCE_four_grams_freq

corp_bnc_sample_UTTERANCE_four_grams_freq %>%
    filter(gender != "X") %>%
    top_n(20) %>%
    ggplot(aes(x = reorder(four_grams, freq), y = freq, fill = gender)) +
    geom_col(position = "dodge", show.legend = F) +
    facet_wrap(~gender, scales = "free") +
    coord_flip() +
    labs(
        x = "Ngram",
        y = "Frequency",
        title = "Recurring Ngrams by Genders"
    )
    
# ==================
# END
# ==================