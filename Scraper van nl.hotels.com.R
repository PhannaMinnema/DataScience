#install.packages('rvest')
library('rvest')
#str_sub functie laden
#install.packages('stringr')
library ('stringr')
#install.packages ('XML')
library ('XML')
library ('RMySQL')
library('dplyr')

setwd("D:/Data/Github/DataScience/")

urlhotel <-paste(basicurl,'/?q-check-out=2018-10-28&FPQ=3&q-check-in=2018-10-27&WOE=7&WOD=6&q-room-0-children=0&pa=1&tab=description&JHR=2&q-room-0-adults=2&YGF=2&MGT=1&ZSX=0&SYE=3', sep = '')

hotelurl <- paste(basicurl, i, sep='')
hotelscrape <- read_html(urlhotel)

basicurl <- 'https://uk.hotels.com/ho121869-tr-p1'
page <- read_html(basicurl) 
page2 <- read_html('https://uk.hotels.com/ho121869-tr-p2/')
page3 <- read_html("https://uk.hotels.com/ho121869-tr-p3/")
page4 <- read_html("https://uk.hotels.com/ho121869-tr-p4/")
page5 <- read_html("https://uk.hotels.com/ho121869-tr-p5/")

#scraper (pakt er 50)
reviewscraper <- 
  page %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()


# pakt er 50
scorescraper <- 
  page%>%
  html_nodes(".review-score .rating")%>%
  html_text()

#pakt de volgende 50
reviewscraper2 <- 
  page2 %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt de volgende 50
scorescraper2 <- 
  page2 %>%
  html_nodes(".review-score .rating")%>%
  html_text()

#pakt de volgende 50
reviewscraper3 <- 
  page3 %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt de volgende 50
scorescraper3 <- 
  page3 %>%
  html_nodes(".review-score .rating")%>%
  html_text()

#pakt de volgende 50
reviewscraper4 <- 
  page4 %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt de volgende 50
scorescraper4 <- 
  page4 %>%
  html_nodes(".review-score .rating")%>%
  html_text()

#pakt de volgende 50
reviewscraper5 <- 
  page5 %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt de volgende 50
scorescraper5 <- 
  page5 %>%
  html_nodes(".review-score .rating")%>%
  html_text()


df.webreviews <- data.frame(cbind(score= c(scorescraper,scorescraper2, scorescraper3, scorescraper4, scorescraper5), review=c(reviewscraper, reviewscraper2, reviewscraper3, reviewscraper4, reviewscraper5)))
df.webreviews$score <- as.numeric(as.character(df.webreviews$score))
df.webreviews$Consensus <- ifelse(df.webreviews$score <6, -1,
                                  ifelse(df.webreviews$score >6 && df.webreviews$score<8, 0, 1))

df.webreviews$score <- NULL
df.webreviews$Consensus <- as.factor(df.webreviews$Consensus)

rm(reviewscraper, reviewscraper2, reviewscraper3, reviewscraper4,reviewscraper5, scorescraper, scorescraper2, scorescraper3, scorescraper4, scorescraper5)

source("Cleaner.R")

df.webreviews$review <- CleanBody(df.webreviews$review)

#
db <- dbConnect(RMySQL::MySQL(), 
                dbname="rstudio", 
                host="localhost",
                port= 3306,
                user="PM",
                password="password")


# put data into MySQL with overwrite function  
SaveDataFrameToDB <- function(db, table, df, doAppend){
  dbWriteTable(db, table, df.webreviews, overwrite = !doAppend, append=doAppend)
  dbDisconnect(db)
}

SaveDataFrameToDB(db, "webreviews",df.webreviews, doAppend=TRUE)

 