# loading libraries rvest, xml, stringr, mysql
required_packages <- c("rvest", "tidyverse", "rebus", "XML", "stringr", "RMySQL", "dbConnect")
x <- lapply(required_packages, library, character.only = TRUE)

# connect to MySQL  WERKT NIET
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
    # The relevant tag
    html_nodes('.partial_entry') %>%      
    html_text() %>% 
    # Trim additional white space
    str_trim() %>%                       
    # Convert the list into a vector
    unlist()                             
}

#open the next review page
next_page <- paste(basic_url, page %>% html_node(".next") %>% html_attr("href"), sep = '')
next_page
