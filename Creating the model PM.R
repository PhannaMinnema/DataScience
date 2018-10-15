#load libraries 
required_packages <- c("tm", "RTextTools","dplyr", "e1071","reshape2", "wordcloud", "readr", 
                       "ggplot2", "tidytext","lubridate", "tidyverse", "tidyr", 
                       "SnowballC", "devtools","httr")
x <- lapply(required_packages, library, character.only = TRUE)
 
#load RTextTools from Ronald 
install_github("ronald245/RTextTools", subdir = "RTextTools")

#open file hotel reviews downloaded from kaggle
setwd("D:/Data/Rstudio/Data Science/")
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE, nrow=20000)

#randomize the dataset
df <- df[sample(row(df)),]
#df$Positive_Review <- as.vector(df$Positive_Review)
#df$Negative_Review <- as.vector (df$Negative_Review)
#df$combi <- paste(df$Positive_Review, df$Negative_Review)

#dataframe aangemaakt gecombineerd incl ID column create as vector
pos.rev <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
neg.rev <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
df <- rbind(pos.rev, neg.rev)%>% tibble::rowid_to_column("ID")
#df_clean$review_body <- as.vector(df_clean$review_body)

# Turn df into source to create corpus
review_source <- DataframeSource(df_clean)

#create corpus
hotel_corpus <- VCorpus(review_source)

#Corpus cleaning 
my_stopwords <- c(stopwords("en"),"positive", "negative","available", "via", "the", "of", "and", "it", "in", "hotel", "staff", "location", "breakfast", "bathroom")



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

clean_corpus(hotel_corpus)

hotel_corpus[[1]]$content 


#bron: https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
#Create the Document-Term Matrix 
SaveTheMatrix <- function(df) {
  print("Creating the doc_matrix")
  doc_matrix <- create_matrix(df$review_body,
                              language = "english",
                              removeNumbers = TRUE,
                              stemWords = TRUE,
                              removeSparseTerms = .998)
  
  save(doc_matrix, file = "doc_matrix")
}

SaveTheMatrix(df)

#Creating container and Training models
TrainClassifiers <- function(dfm, doc_matrix) {
  print("Creating Container:")
  container <- create_container(doc_matrix,
                                df$Consensus,
                                trainSize = 1:20000,
                                testSize = 20001:50000,
                                virgin = FALSE)
  
  models <- train_models(container, algorithms = c("MAXENT", "SVM", "BAGGING", "RF", "TREE"))
  results <- classify_models(container, models)
  
  analytics <- create_analytics(container, models)
  summary (analytics)
  
}

TrainClassifiers(df) 
 

rm(list = c("df", "doc_matrix", "container", "models"))
