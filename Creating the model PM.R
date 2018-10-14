#load libraries 
required_packages <- c("tm", "RTextTools","dplyr", "e1071","reshape2", "wordcloud", "readr", 
                       "ggplot2", "tidytext","lubridate", "tidyverse", "tidyr", 
                       "SnowballC", "devtools","httr")
x <- lapply(required_packages, library, character.only = TRUE)
 
#load RTextTools from Ronald 
install_github("ronald245/RTextTools", subdir = "RTextTools")

#open file hotel reviews downloaded from kaggle
setwd("D:/Data/Rstudio/Data Science/")
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE)

#randomize the dataset
df <- df[sample(row(df)),]
pos.rev <- as.data.frame(t(rbind(review_body = df$Positive_Review, Consensus = as.integer(1))))
neg.rev <- as.data.frame(t(rbind(review_body = df$Negative_Review, Consensus = as.integer(-1))))
df <- rbind(pos.rev, neg.rev)

df$Positive_Review <- as.vector(df$Positive_Review)
df$Negative_Review <- as.vector (df$Negative_Review)
df <- paste(df$Positive_Review, df$Negative_Review)


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

#Creating a container
container <- create_container(doc_matrix,
                              df$Concensus,
                              trainSize=1:20000,
                              testSize= 20001:50000,
                              virgin = FALSE)

#Training models
TrainClassifiers <- function(dfm, doc_matrix) {
  print("Creating Container:")
  container <- create_container(doc_matrix,
                                df$Consensus,
                                trainSize = 1:20000,
                                testSize = 20001:50000,
                                virgin = FALSE)
  
  trainingmodels <- train_models(container, algorithms = c("MAXENT", "SVM", "BAGGING", "RF", "TREE"))
  save(trainingmodels, file = "trainedModels.Rd")
  
  rm(list = c("df", "TDMmatrix", "container", "models"))
}
