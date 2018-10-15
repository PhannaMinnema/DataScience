#load libraries 
required_packages <- c("tm", "RTextTools","dplyr", "e1071","reshape2", "wordcloud", "readr", 
                       "ggplot2", "tidytext","lubridate", "tidyverse", "tidyr", 
                       "SnowballC", "devtools","httr","widyr", "prediction")
x <- lapply(required_packages, library, character.only = TRUE)


#load RTextTools from Ronald 
#install_github("ronald245/RTextTools", subdir = "RTextTools")

#open file hotel reviews downloaded from kaggle
setwd("D:/Data/Rstudio/Data Science/")
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE, nrow=3000)
#na.strings = c("", "NA"),
#df <- df%>% na.omit()

#randomize the dataset
pos.rev <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
neg.rev <-  as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
df <-rbind(pos.rev, neg.rev)

#to dubbel check columnname for corpus
View(df)
df <- df[sample(row(df)),]
#df$Positive_Review <- as.vector(df$Positive_Review)
#df$Negative_Review <- as.vector(df$Negative_Review)
#df$combi <- paste(df$Positive_Review, df$Negative_Review)

#dataframe aangemaakt gecombineerd incl ID column create as vector  
#df <-rbind(pos.rev, neg.rev)
#df <- rbind(pos.rev, neg.rev)%>% tibble::rowid_to_column("ID")
#df_clean$review_body <- as.vector(df_clean$review_body)

#make a corpus
hotel_corpus <- VCorpus(VectorSource(df$review_body))

#inspect corpus
hotel_corpus
inspect(hotel_corpus[1:3])
#content(hotel_corpus[[1]]) # check first review

#show first item
strwrap(hotel_corpus[1])

#Corpus cleaning 
my_stopwords <- c(stopwords("en"),"positive", "negative","available", "via", 
                  "the", "of", "and", "it", "in", "hotel", "staff", "location", 
                  "breakfast", "bathroom")

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

#save cleaning function
mycorpus <- clean_corpus(hotel_corpus)

rm(pos.rev, neg.rev)
#show first 
strwrap(mycorpus[1:20])

#generate TDM
dtm<- DocumentTermMatrix(mycorpus)
#dtm <- as.matrix(dtm)

#View(dtm[[1]])
#inspect(dtm)

#generate word frequency matrix
#wfm <- wfm(mycorpus)
#wfm <- as.matrix(mycorpus)

#bron: https://journal.r-project.org/archive/2013/RJ-2013-001/RJ-2013-001.pdf
#Creating container and Training models
TrainClassifiers <- function(df,doc_matrix) {
  print("Creating Container:")
  container <- create_container(doc_matrix,
                                df$Consensus,
                                dfTrain <-1:3000,
                                dfTest <- 3001:6000,
                                virgin = FALSE)
  
  #classifiers train
  models <- train_models(container, algorithms = c("MAXENT", "SVM", "BAGGING", "RF", "TREE"))
  results <- classify_models(container, models)
  
  analytics <- create_analytics(container, models)
  summary (analytics)
  
  #save the models + container as R data
  save(models,file='Rmodels.Rd')
  save(container, file= 'container.Rd')

  }


# load model and container in trainclassifier
TrainClassifiers(df,dtm)
load("Rmodels.Rd")
load("container.Rd")


rm(list = c("df", "doc_matrix", "container", "models"))
