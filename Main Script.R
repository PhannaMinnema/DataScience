library(e1071)
library(tm)
library(magrittr)
library(tidyr)
library(RTextTools)
library (RMySQL)
memory.limit()

#set working directory
setwd("D:/Data/Github/DataScience/")

#Connect to db
db <- dbConnect(RMySQL::MySQL(), 
                dbname="rstudio", 
                host="localhost",
                port= 3306,
                user="PM",
                password="password")

#  MySQL with overwrite function  
SaveDataFrameToDB <- function(db, table, df, doAppend){
  dbWriteTable(db, table, df, overwrite = !doAppend, append=doAppend)
  dbDisconnect(db)
}


#get the cleaner script
source("Cleaner.R")

#Get the reviews and clean
CleanData <- CleanCSV("Hotel_Reviews.csv")
# put data into MySQL
SaveDataFrameToDB(db, "hotelreviews",CleanData, doAppend=TRUE)





