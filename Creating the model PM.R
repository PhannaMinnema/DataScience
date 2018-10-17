#load libraries 
library(dplyr)
library(devtools)
library(httr)
library(widyr)
library (prediction)
library(tm)
library(RTextTools)
library(e1071)
library(readr)
library(lubridate)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(stringr)
library(tidyr)
library(scales)
library(broom)
library(purrr)
library(widyr)
library(igraph)
library(ggraph)
library(SnowballC)
library(wordcloud)
library(reshape2)
theme_set(theme_minimal())



#open file hotel reviews downloaded from kaggle
setwd("D:/Data/Rstudio/Data Science/")
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE, nrow=20000)

dfdate <- df$Review_Date <- as.Date(df$Review_Date, format = "%m/%d/%Y")
dfdate %>% 
  count(Week = round_date(Review_Date, "Week")) %>%
  ggplot(aes(Week, n)) +
  geom_line() + 
  ggtitle('The Number of Reviews Per Week')


#combine pos + neg rev
pos.rev <- as.data.frame(t(rbind(review = df$Positive_Review, Consensus = as.integer(1))))
neg.rev <-  as.data.frame(t(rbind(review = df$Negative_Review, Consensus = as.integer(-1))))
df <-rbind(pos.rev, neg.rev)

#to dubbel check columnname for corpus
View(df)
df <- df[sample(row(df)),]

#make a corpus
mycorpus <- VCorpus(VectorSource(df$review))

#inspect corpus
mycorpus

#content(hotel_corpus[[1]]) # check first review
inspect(mycorpus[1:3])

#show first item
strwrap(mycorpus[1])

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
mycleancorpus <- clean_corpus(mycorpus)

#show first 
strwrap(mycorpus[1:20])

#generate TDM
dtm<- DocumentTermMatrix(mycorpus)
#dtm <- as.matrix(dtm)

#View(dtm[[1]])
#inspect(dtm)

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
  models <- train_models(container, algorithms = c("SVM", "BAGGING", "RF", "TREE"))
  results <- classify_models(container, models)
  create_scoreSummary (container,results)
  
  analytics <- create_analytics(container, results)
  summary (analytics)
  
  
  #save the models + container as R data
  #save(models,file='Rmodels.Rd')
  #save(container, file= 'container.Rd')
}

# load model and container in trainclassifier
TrainClassifiers(df,dtm)
load("Rmodels.Rd")
load("container.Rd")

df$BAGGING_LABEL = results$BAGGING_LABEL
df$FOREST_LABEL = results$FOREST_LABEL
df$TREE_LABEL = results$TREE_LABEL
df$SVM_LABEL = results$SVM_LABEL
df$Consensus= cbind(df$BAGGING_LABEL,df$FOREST_LABEL, df$TREE_LABEL, df$SVM_LABEL)

rm(list = c("doc_matrix", "container", "models"))

#Testint the algorithm accuracy
