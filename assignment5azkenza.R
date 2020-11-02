install.packages("syuzhet") 

library(syuzhet)
library(rvest)
library(tidyverse)
library(dplyr)

news <- function(term) {
  
  html_dat <- read_html(paste0("https://news.google.com/search?q=",term,"&hl=en-US&gl=US&ceid=US%3Aen"))
  
  dat <- data.frame(Link = html_dat %>%
                      html_nodes('.VDXfz') %>% 
                      html_attr('href')) %>% 
    mutate(Link = gsub("./articles/","https://news.google.com/articles/",Link))
  
  news_dat <- data.frame(
    Description =  html_dat %>%
      html_nodes('.Rai5ob') %>% 
      html_text(),
    Link = dat$Link
)
  
  return(news_dat)
}

find_sentiment <- function(news,theme){
  
  news$Description <- lapply(news$Description, as.vector)
  news$Description = as.vector(as.character(news$Description))
  
  news$Syuzhet_Vector <- get_sentiment(news$Description, method="syuzhet")
  news$Affin_Vector <- get_sentiment(news$Description, method="afinn")
  news$Nrc_Vector <- get_sentiment(news$Description, method="nrc")
  news$Bing_Vector <- get_sentiment(news$Description, method="bing")
  news = head(news,20)
  news$Theme <- theme
  return(news)
  
}
data_analysis1 = data.frame(news("Fitness"))
news_f = find_sentiment(data_analysis1,"Fitness")
data_analysis2 = data.frame(news("Wearables"))
news_w = find_sentiment(data_analysis2,"Wearables")
data_analysis = bind_rows(news_f, news_w)

write_csv(data_analysis,"/Users/ayshazenabkenza/Desktop/Web_Analytics/Assignment5/sentiment_analysis.csv")
