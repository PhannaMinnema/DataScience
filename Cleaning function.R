#load libraries 
required_packages <- c("tm", "RTextTools","dplyr", "e1071","reshape2", "wordcloud", "readr", 
                       "ggplot2", "tidytext","lubridate", "tidyverse", "tidyr", 
                       "SnowballC", "devtools","httr")
x <- lapply(required_packages, library, character.only = TRUE)

#cleaning function
mystopwords <- as.vector(c(stopwords("en"),"positive", "negative","available", "via", "the", "of", "and", "it", "in", "hotel", "staff", "location", "breakfast", "bathroom"))
 
CleanDF <- function(words) {
  as.vector(words) %>%
    iconv(to = "ASCII") %>%
    stemDocument() %>%
    tolower() %>%
    #remove decimals longer than 3, they confuse the matrix
    gsub(pattern = "[a-zA-Z]*([0-9]{3,})[a-zA-Z0-9]* ?", replacement = "") %>%
    #remove all numbers
    #gsub(pattern = "\\d+ ", replacement = "") %>%
    #remove punctuation and digits
    gsub(pattern = "[[:punct:]]", replacement = "") %>%
    gsub(pattern = "[[:digit:]]", replacement = "") %>%
    #remove html links, unnecessary tabs, spaces, newlines
    gusb(pattern = "http\\w+",replacement="")%>%
    gsub(pattern= "[\t]{2}", replacement = "")%>%
    gsub(pattern = "^\\s+|\\s+$", replacement= "")%>%
    gsub(pattern = "[\r\n]", replacement = "") %>%
    #remove the stop words
    gsub(pattern = mystopwords, replacement = "") %>%
    #remove single letters
    gsub(pattern = "\b[a-zA-Z]\b", replacement = "") %>%
    #remove whitespace longer than one
    gsub(pattern = " {2,}", replacement = " ") %>%
    #trim whitespace at the front and the end
    as.factor %>%
    trimws() %>%
    return()
}
