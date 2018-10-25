#install.packages('rvest')
library('rvest')
#str_sub functie laden
#install.packages('stringr')
library ('stringr')
#install.packages ('XML')
library ('XML')


urlhotel <-url('https://nl.hotels.com/ho149565/?q-check-out=2018-10-28&FPQ=5&q-check-in=2018-10-27&WOE=7&WOD=6&q-room-0-children=0&pa=1&tab=description&JHR=4&q-room-0-adults=2&YGF=2&MGT=1&ZSX=0&SYE=3')

hotelscrape <- read_html(urlhotel)

#scraper (pakt er 5)
reviewscraper <- hotelscrape %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()
reviewscraper

scorescraper <- hotelscrape%>%
  html_nodes("span.rating")%>%
  html_text()
scorescraper


