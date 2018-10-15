# loading libraries rvest, xml, stringr, mysql
required_packages <- c("rvest", "tidyverse", "XML", "stringr", "RMySQL", "dbConnect")
x <- lapply(required_packages, library, character.only = TRUE)

# connect to MySQL  
db <- dbConnect(RMySQL::MySQL(), 
                dbname="rstudio", 
                host="localhost",
                port= 3306,
                user="PM",
                password="password")

dbListTables(db)

# Reading the webpage into R
basic_url <- read_html('https://www.tripadviser.nl')
url <-'https://www.tripadvisor.nl/Hotel_Review-g150807-d154868-Reviews-Le_Blanc_Spa_Resort-Cancun_Yucatan_Peninsula.html'

#open the page
page <- read_html(url) 

#get reviews            https://www.datacamp.com/community/tutorials/r-web-scraping-rvest
get_reviews <- function(html){
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

get_reviews(page)
get_reviews(next_page)

#scraping next page
getNextUrl <- function(url) {
  page %>% 
    html_node(".next") %>%
    html_attr("href")
  sep = ''
}


#open the next review page
more_rev <- function(url, n) {
  purrr::map_df(1:n, ~{
    oUrl <- url
    post <- get_reviews(page)
    url <<- getNextUrl(url)
    data.frame(curpage = oUrl, 
               nexturl = url,
               posttext = post)
  })
}

df <- more_rev(url,12)

df$posttext[[1]]
















# Get the titles from the reviews & Converting the titles to text
review_titles <- append(review_titles, html_text(html_nodes(page, '.title > .noQuotes')))
review_text <- append(review_text, html_text(html_nodes(page, '.prw_reviews_text_summary_hsx > .entry > .partial_entry')))

# Create dataframe for the Reviews
reviews <- cbind(review_titles, review_text)
colnames(reviews) <- c('Titles', 'Review')

print(reviews)

if (store_reviews_in_csv) {
  # Export the reviews table to csv file
  write.table(reviews, 'csv/tripadvisor_ozo_hotel_reviews.csv') 
}

if (store_reviews_in_db) {
  print('Storing reviews in database...')
  
  reviews[,1] <- gsub('"', "'", reviews[,1])
  reviews[,2] <- gsub('"', "'", reviews[,2])
  
  # Insert Table data into the Database
  for(rowId in c(1:nrow(reviews))) {
    callQuery <- paste('CALL add_review("', reviews[rowId, 1], '", "', reviews[rowId,2], '")', sep = "")
    dbSendQuery(db, callQuery)
  }
  
  # Close connection
  dbDisconnect(db)
  
}

}