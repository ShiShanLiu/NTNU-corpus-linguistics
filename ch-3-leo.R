# CH3 Creating corpus
# ==================
# Exercise 3.1 automatically scrape more than one page
# ==================
library(tidyverse)
library(rvest)
ptt.url <- "https://www.ptt.cc/bbs/Gossiping"

gossiping.session <- session(ptt.url)

gossiping.form <- gossiping.session %>%
  html_node("form") %>%
  html_form

gossiping <- session_submit(
  x = gossiping.session,
  form = gossiping.form,
  submit = "yes"
)

page.latest <- gossiping %>%
  html_nodes("a") %>% 
  html_attr("href") %>%  
  str_subset("index[0-9]{2,}\\.html") %>% 
  str_extract("[0-9]+") %>% 
  as.numeric()

# create a for loop to get more than one page indexes ----------
links.article.new <- c()

for (web.page in seq(from = 65, to = 60, by = -1)){
  link <- str_c(ptt.url, "/index391", web.page, ".html")
  # print(link)
  links.article <- gossiping %>%
    session_jump_to(link) %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>% 
    str_c("https://www.ptt.cc",.)
  print(links.article)
  links.article.new <- c(links.article, links.article.new)
}
# ---------------------------------------------------------------

links.article.new
# === getting url function starts here ===
extract_art_links <- function(index_page, session){
  links.article.new <- session %>%
    session_jump_to(index_page) %>%
    html_nodes("a") %>%
    html_attr("href") %>%
    str_subset("[A-z]\\.[0-9]+\\.[A-z]\\.[A-z0-9]+\\.html") %>%
    str_c("https://www.ptt.cc",.)
  
  return(links.article.new)
}

# === content function starts here ===
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


links.article.new %>%
  map(extract_article_push_tables) -> ptt.data.new

# Merge all article tables into one
article.table.all <- ptt.data.new %>% 
  map(function(x) x$article.table) %>%
  bind_rows
article.table.all

# Merge all push.tables into one
push.table.all <- ptt.data.new %>%
  map(function(x) x$push.table) %>%
  bind_rows

# ==================
# END
# ==================
# ==================
# Exercise 3.2
# ==================
# notice: I scraped 5 index pages in exercise 3.1
write_csv(article.table.all, path = "PTT_GOSSIPING_ARTICLE.csv")
write_csv(push.table.all, path = "PTT_GOSSIPING_PUSH.csv")

print("Corpus Size")
article.table.all$content %>%
  nchar %>%
  sum
# ==================
# END
# ==================

# ==================
# Exercise 3.3
# ==================
news.url = "https://news.pts.org.tw/"
news.session <- session(news.url)
news.links <- news.session %>%
  session_jump_to(news.url) %>%
  html_nodes("a") %>%
  html_attr("href") %>%
  str_subset("article\\/[0-9]+") %>%
  str_extract("[0-9]+") %>%
  str_c("https://news.pts.org.tw/article/",.)

news.links <- unique(news.links) # remove duplicated strings

news.article <- news.links[1]


extract_article_information <- function(link){
  new.url <- link
  temp.html <- news.session %>%
    session_jump_to(new.url)
  # article content starts here
  # article title
  article.title <- temp.html %>%
    html_nodes("h1.article-title") %>%
    html_text()
  
  # article conclusion content
  article.conclusion.content <- temp.html %>%
    html_nodes(
      xpath = "/html/body/div[6]/div/div[2]/article/div[1]/text()"
    ) %>%
    html_text(trim = TRUE) %>%
    str_c(collapse = "")
  
  # article.publishtime
  article.publishedtime <- temp.html %>%
    html_nodes("span.text-nowrap") %>%
    html_text()
  
  # article author
  article.author <- temp.html %>%
    html_nodes(
      xpath = "/html/body/div[5]/div/div/div/div[1]/span[1]"
    ) %>%
    html_text()
  
  # create the table
  test.news.table <- tibble(
    publishtime = article.publishedtime[1],
    title = article.title,
    author = article.author,
    conclusionContent = article.conclusion.content
  )
  return(test.news.table)
}

news.links %>%
  map(extract_article_information) -> news.data

news.data
# ==================
# END
# ==================