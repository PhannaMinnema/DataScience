# loading libraries rvest, xml, stringr, mysql
required_packages <- c("rvest", "tidyverse", "XML", "stringr", "RMySQL", "dbConnect","tm", "RTextTools","dplyr", "e1071","reshape2", "wordcloud", "readr", 
                       "ggplot2", "tidytext","lubridate", "tidyverse", "tidyr", 
                       "SnowballC", "devtools","httr","widyr", "prediction")
x <- lapply(required_packages, library, character.only = TRUE)

# connect to MySQL  
db <- dbConnect(RMySQL::MySQL(), 
                dbname="rstudio", 
                host="localhost",
                port= 3306,
                user="PM",
                password="password")


# Reading the webpage into R
basic_url <- read_html('https://www.tripadviser.nl')
url <-'https://www.tripadvisor.nl/Hotel_Review-g150807-d154868-Reviews-Le_Blanc_Spa_Resort-Cancun_Yucatan_Peninsula.html'

#open the page
page <- read_html(url) 

#get reviews https://www.datacamp.com/community/tutorials/r-web-scraping-rvest 
#https://stackoverflow.com/questions/43232549/function-for-next-page-rvest-scrape#

get_reviews <- function(page){
  page %>% 
    # The relevant tag indicates the class
    html_nodes('.partial_entry') %>%      
    #extract only the raw text as a list  
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%               
    #Extracting the url
    #html_attr("href")
    # Convert the list into a vector 
    unlist() %>%
    return()
}

get_score <- function(page){
  regex.magix <- "\\b(?!bubble_\\d\\d)\\b\\S+"
  regex.magix2 <- "[^0-9]+"
  page %>%
    html_nodes('.reviewSelector .ui_bubble_rating')%>%
    html_text() %>%
    gsub(pattern = regex.magix, replacement = "", perl = TRUE)%>%
    gsub(pattern = regex.magix2, replacement = "")%>%
    return()
}


#scraping next page
getNextUrl <- function(page) {
  page %>% 
    html_node(".next") %>%
    html_attr("href")
  sep = ''
}

#open the next review pages
more_rev <- function(url, n) {
  for(i in 1:n){
    url <- nexturl
    page <- read_html(url) 
    post <- get_reviews(page)
    nexturl <<- getNextUrl(url)
    #score <- get_score(page)
    df <- data.frame(
               review = post,
               score = score)
    SaveDataFrameToDB(db,"webreviews", df, TRUE)
  }
}

nexturl <- url
df <- more_rev(url,12)
 
df$review <- as.vector(df$review)

#create corpus
mycorpus <- VCorpus(VectorSource(df$review))

#stopwords
my_stopwords <- c(stopwords("en"),"positive", "negative","available", "via", 
                  "the", "of", "and", "it", "in", "hotel", "staff", "location", 
                  "breakfast", "bathroom")
#clean_corpus
clean_corpus <- function(corpus){
  corpus %>%
    tm_map(content_transformer (tolower)) %>%
    tm_map(content_transformer (removePunctuation)) %>%
    tm_map(content_transformer (removeNumbers))%>%
    tm_map(content_transformer (removeWords),my_stopwords)%>%
    tm_map(content_transformer (stripWhitespace))%>%
    tm_map(content_transformer (stemDocument))%>%
    return()
}

#execute cleaning function
mycorpus <- clean_corpus(mycorpus)


# put data into MySQL with overwrite function  
SaveDataFrameToDB <- function(db, table, df, doAppend){
  dbWriteTable(db, table, df, overwrite = !doAppend, append=doAppend)
  dbDisconnect(db)
}





