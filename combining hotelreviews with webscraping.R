#open file hotel reviews downloaded from kaggle
setwd("D:/Data/Rstudio/Data Science/")
df <- read.csv("Hotel_Reviews.csv", stringsAsFactors = FALSE, nrow=4000)

#combine pos + neg rev
pos.rev <- as.data.frame(t(rbind(review = df$Positive_Review, Consensus = as.integer(1))))
neg.rev <-  as.data.frame(t(rbind(review = df$Negative_Review, Consensus = as.integer(-1))))
df.hotelreviews <-rbind(pos.rev, neg.rev)

#combine webscraped reviews with hotelreviews into one df
df.reviews <- merge(df.hotelreviews, df.webreviews, by = 'review', all=T)

#WERKT NIET DB
# connect to MySQL  
db <- dbConnect(RMySQL::MySQL(), 
                dbname="rstudio", 
                host="localhost",
                port= 3306,
                user="PM",
                password="password")

dbListFields(db)



# put data into MySQL with overwrite function  
SaveDataFrameToDB <- function(db, table, df, doAppend){
  dbWriteTable(db, table, df, overwrite = !doAppend, append=doAppend)
  dbDisconnect(db)
}

SaveDataFrameToDB(db, "reviews",df, doAppend=TRUE)





