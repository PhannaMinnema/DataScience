library(e1071)
library(tm)
library(magrittr)
library(tidyr)
library(RTextTools)
library (RMySQL)
library (caTools)
#install.packages('caTools')
memory.limit()

library(dplyr)
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
#install.packages('widyr')
library(igraph)
library(ggraph)
#install.packages('ggraph')
library(SnowballC)
library(wordcloud)
library(reshape2)
theme_set(theme_minimal())
library(bigmemory)

CleanData <- CleanData[sample(1:nrow(CleanData),nrow(CleanData)),]

train= CleanData[1:50000,]

#Model NaiveBayes
naiveBayesModel = naiveBayes(Consensus ~., data=train, laplace=0.1)

#Predictions
nbPredictions=predict(naiveBayesModel,CleanData)

#Confusion matrix to check accuracy
table(nbPredictions, CleanData$Consensus)


#Log.Model 
train= CleanData[1:8000,]
set.seed(88)
split <- sample.split(train$Consensus, SplitRatio = 0.75)


#model
Log.Model <- glm(Consensus~.,data = train, family = binomial)
summary(Log.model)

predict <- predict(Log.Model, type='response')

#confusion matrix
table(predict, CleanData$Consensus)

#Predictionstrain
lrPredictions= predict(Log.Model,
                       newdata = CleanData[!train,],
                       type = "response")

testdata = CleanData$Consensus[!train]

#Confusion matrix to check accuracy
table(lrPredictions, CleanData$Consensus)

pr <- prediction(lrPredictions, measure = "tpr", x.measure = "fpr")
plot(pfr)

#SVM
CleanDataDTM <- create_matrix(CleanData$review_body)

save(models,file='Rmodels.Rd')
TrainClassifiers <- function(df,doc_matrix){
  print("Creating Container:")
  container <- create_container(CleanDataDTM,
                                CleanData$Consensus,
                                trainSize = 1:8000,
                                testSize = 8001:9000,
                                virgin = FALSE)
  
  #classifiers train
  models <- train_models(container, algorithms = c("SVM", "MAXENT"))
  results <- classify_models(container, models)
  create_scoreSummary (container,results)
  
  analytics <- create_analytics(container, results)
  summary (analytics)
}

save(container, file= 'container.Rd')

# load model and container in trainclassifier
TrainClassifiers(CleanData,CleanDataDTM)
load("Rmodels.Rd")
load("container.Rd")


plot(results, main='SVM')
pr.iris <- prcomp(x = iris[-5], scale = FALSE, center = TRUE)
biplot(pr.iris)
