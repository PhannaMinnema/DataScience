#install.packages('rvest')
library('rvest')
#str_sub functie laden
#install.packages('stringr')
library ('stringr')
#install.packages ('XML')
library ('XML')


urlhotel <-paste(basicurl,'/?q-check-out=2018-10-28&FPQ=3&q-check-in=2018-10-27&WOE=7&WOD=6&q-room-0-children=0&pa=1&tab=description&JHR=2&q-room-0-adults=2&YGF=2&MGT=1&ZSX=0&SYE=3', sep = '')

hotelurl <- paste(basicurl, i, sep='')
hotelscrape <- read_html(urlhotel)


basicurl <- 'https://uk.hotels.com/ho121869-tr'
page <- read_html(basicurl) 
page2 <- read_html('https://uk.hotels.com/ho121869-tr-p2/')

#scraper (pakt er 50)
reviewscraper <- 
  page %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt er 50
scorescraper <- 
  page%>%
  html_nodes(".review-score .rating")%>%
  html_text()

#pakt de volgende 50
reviewscraper2 <- 
  page2 %>%
  html_nodes("blockquote.expandable-content") %>% #pipe-operator
  html_text()

# pakt de volgende 50
scorescraper2 <- 
  page2 %>%
  html_nodes(".review-score .rating")%>%
  html_text()

df1 <- cbind(score = scorescraper, review = reviewscraper)
df2 <- cbind (score = scorescraper2, review=reviewscraper2)

df <- data.frame(rbind(df1, df2))


