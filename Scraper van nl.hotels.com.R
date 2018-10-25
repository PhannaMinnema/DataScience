#install.packages('rvest')
library('rvest')
#str_sub functie laden
#install.packages('stringr')
library ('stringr')
#install.packages ('XML')
library ('XML')

basicurl <- 'https://uk.hotels.com/ho121869'
urlhotel <-paste(basicurl,'/?q-check-out=2018-10-28&FPQ=3&q-check-in=2018-10-27&WOE=7&WOD=6&q-room-0-children=0&pa=1&tab=description&JHR=2&q-room-0-adults=2&YGF=2&MGT=1&ZSX=0&SYE=3', sep = '')

hotelurl <- paste(basicurl, sep='')
hotelscrape <- read_html(hotelurl)

#scraper (pakt er 5)
reviewscraper <- 
  hotelscrape %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt er 5 
scorescraper <- 
  hotelscrape %>%
  html_nodes(".review-score .rating")%>%
  html_text()
  

nexturl <- paste(basicurl, html_attr(html_node(hotelscrape, '.next'), 'href'), sep = '')

#reviews <- function(hotelscrape, n){
for (i in 1:20){
    review <- reviewscraper 
    score <- scorescraper 
    nextpage <- nexturl
    df <- data.frame(
      review,
      score)
}





