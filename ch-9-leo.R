# CH9 Constructions and idioms
# ==================
# Exercise 9.1 
# ==================
library(tidyverse)
library(reticulate)
library(readtext)
library(quanteda)
library(tidyr)
library(tidytext)
use_condaenv("ai_keras")
ckip <- reticulate::import(module = "ckiptagger")
ws <- ckip$WS("./data")

new_words_fc <- function(file_path){
    sample_txt <- readLines(file_path)
    rep(1,length(sample_txt)) -> sample_values
    names(sample_values) <- sample_txt
    sample_list <- as.list(sample_values)
    return(sample_list)
}
new_words <- new_words_fc("demo_data/dict-sample.txt")
my_dict <- ckip$construct_dictionary(new_words)
my_dict

# ==================
# END
# ==================
# ==================
# Exercise 9.2
# ==================
# text
text <- "綠黨桃園市議員王浩宇爆料，
指民眾黨不分區被提名人蔡壁如、黃瀞瑩，
在昨（6）日才請辭是為領年終獎金。
台灣民眾黨主席、台北市長柯文哲7日受訪時則說，
都是按流程走，不要把人家想得這麼壞。"
# dict
user_words <- list("被提名人" = 1,
                  "年終獎金"=1,
                  "受訪"=1,
                  "不分區"=1)
user_dict<-ckip$construct_dictionary(user_words)
user_dict
# unnest_tokens
delimiter = "[，!?；:。\n]+"
text_corp <- corpus(text)
text_tidy <- tidy(text_corp)
text_tidy %>% 
    mutate(text_id = row_number()) -> text_tidy

text_line <- text_tidy %>%
    unnest_tokens(
        output = line,
        input = text,
        token = function (x)
            str_split(x, pattern = delimiter)
    )

text_result <- text_line %>%
    unnest_tokens(
        output = word,
        input = line,
        token = function (x)
            ws(x, recommend_dictionary = user_dict)
    )
text_result
text_result %>%
    mutate(word_id = row_number()) %>%
    select(word_id, word) -> word_result
word_result
# I think ckiptagger's weakness is that it cannot 
# process a character vector with only one text.
# Before applying ws() in unnest_tokens,
# the text has to be split by delimiters to a 
# line-based df. After that, a line-based df can be 
# sent into unnest_tokens for word segmentation.
# Compared to ckiptagger, jieba can do word segmentation
# faster than ckip and I think it's more compact in 
# using jieba. Ckiptagger has more steps to prepare for 
# creating word segmenter.

# ==================
# END
# ==================
# ==================
# Exercise 9.3
# ==================
pos <- ckip$POS("./data")
ner <- ckip$NER("./data")

texts <- c("傅達仁今將執行安樂死，卻突然爆出自己20年前遭緯來體育台封殺，他不懂自己哪裡得罪到電視台。",
           "美國參議院針對今天總統布什所提名的勞工部長趙小蘭展開認可聽證會，預料她將會很順利通過參議院支持，成為該國有史以來第一位的華裔女性內閣成員。",
           "土地公有政策?？還是土地婆有政策。.",
           "… 你確定嗎… 不要再騙了……他來亂的啦",
           "最多容納59,000個人,或5.9萬人,再多就不行了.這是環評的結論.",
           "科長說:1,坪數對人數為1:3。2,可以再增加。")
self_dict <- readtext("demo_data/dict-sample.txt")


texts_corp <- corpus(texts)
texts_tidy <- tidy(texts_corp)
texts_tidy %>%
    mutate(text_id = row_number()) -> texts_tidy

texts_result <- texts_tidy %>%
    unnest_tokens(
        output = word,
        input = text,
        token = function (x)
            ws(x, recommend_dictionary = user_dict)
    )
texts_result

texts_result %>%
    group_by(text_id) %>%
    mutate(word_id = row_number(), .before = word) %>%
    ungroup() -> texts_result
texts_result


tags <- unlist(pos(list(texts_result$word)))

texts_result$tag = tags

texts_result

texts_result %>%
    group_by(text_id) %>%
    mutate(word_length = nchar(word)) %>%
    ungroup() -> texts_result
texts_result

length(texts_result$text_id == 1)
texts_result$word_length -> texts_length
texts_length

num_bag <- c()
num_bag_sm <- c()
stop_points <- c(26, 37, 12, 18, 22, 15)
start_place <- 1

while(start_place < 7){
    for (n in 1:length(texts_length)){
        if (length(num_bag_sm) == stop_points[start_place]){
            num_bag <- c(num_bag, num_bag_sm)
            num_bag_sm <- c()
            start_place <- start_place + 1

            if (n == 1){
                number = texts_length[n]
                num_bag_sm <- c(num_bag_sm, number)
                
            }else{
                number = texts_length[n]
                num_bag_sm <- c(num_bag_sm, number)
            }
            
        }else{
            
            if (n == 1){
                number = texts_length[n]
                num_bag_sm <- c(num_bag_sm, number)
            }else{
                number = texts_length[n] + number
                num_bag_sm <- c(num_bag_sm, number)
                
            }
        }
    }
}

num_bag

texts_result["end"] <- num_bag

texts_result %>%
    mutate(start = end - word_length, .before = end) -> texts_result

texts_result %>%
    select(text_id, word_id, start, end, word, tag) -> texts_result
texts_result

# ==================
# END
# ==================
# ==================
# Exercise 9.4
# ==================
texts_result %>%
    group_by(text_id) %>%
    summarize(text = paste(word, tag, sep = "/", collapse = " ")) -> texts_df
texts_df

# ==================
# END
# ==================
# ==================
# Exercise 9.5
# ==================
ner_ckip <- function (x, self_dict){
    w1 <- ws(x, recommend_dictionary = self_dict)
    w1_pos <- pos(w1)
    w1_ner <- ner(w1, w1_pos)
    return (w1_ner)
}

ner_result <- ner_ckip(texts, user_dict)
ner_result
# --------------test------------
start_c = c()
end_c = c()
ner_type_c = c()
word_c = c()

for (list in 1:length(ner_result)){
    if (length(ner_result[[list]]) != 0){
        step1 <- as.character(ner_result[[list]])
        step1 %>%
            str_replace_all(pattern = "[{|}|']+", "") %>%
            str_split(",[^0-9]") -> step2
        step3 <- unlist(step2)
        
        for (i in 1:length(step3)){
            if ((i %% 4) == 1){
                start_c = c(start_c, step3[i])
            }else if((i %% 4) == 2){
                end_c = c(end_c, step3[i])
            }else if ((i %% 4) == 3){
                ner_type_c = c(ner_type_c, step3[i])
            }else{
                word_c = c(word_c, step3[i])
            }
        }
    }else{
        next
    }
    
}
length(start_c)
length(end_c )
length(ner_type_c)
length(word_c) 
start_c <- start_c[-length(start_c)]
start_c %>%
    str_replace_all("[(]", "") -> start_c
word_c %>%
    str_replace_all("[)]", "") -> word_c


text_tb <- tibble(
    start = start_c,
    end = end_c,
    ner_type = ner_type_c,
    string = word_c
)
text_tb

texts_result %>%
    select(text_id, start, end, tag) -> temp_df

temp_df %>%
    rename(ner_type = tag) -> temp_df

temp_df$start <- as.character(temp_df$start)
temp_df$end <- as.character(temp_df$end)
temp_df$text_id <- as.character(temp_df$text_id)

left_join(text_tb, temp_df)
# ==================
# END
# ==================
# ==================
# Exercise 9.6
# ==================
text_tb$start <- as.integer(text_tb$start)
text_tb$end <- as.integer(text_tb$end)

left_join(texts_result, text_tb) -> test
test %>%
    select(text_id, word_id, start, end, tag, ner_type) -> test

test[is.na(test)] = ""

test %>%
    rename(entity = ner_type) -> test
# ==================
# END
# ==================