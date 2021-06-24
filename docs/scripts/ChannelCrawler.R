
library(magrittr) #for pipes
library(dplyr) #for pull function
library(rvest) #get html nodes
library(xml2) #pull html data
library(selectr) #for xpath element
library(tibble)
library(purrr) #for map functions
library(datapasta) #for recreating tibble's with ease
library(stringr)
library(rebus)
library(XML)
library(tidyr)
library(lubridate)


####################
#SCRAP CHANNEL CRAWLER
####################


USER_AGENTS <- c(
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:57.0) Gecko/20100101 Firefox/57.0",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/64.0.3282.140 Safari/537.36",
  "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:58.0) Gecko/20100101 Firefox/58.0",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:58.0) Gecko/20100101 Firefox/58.0",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:56.0) Gecko/20100101 Firefox/56.0",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64; rv:56.0) Gecko/20100101 Firefox/56.0",
  "Mozilla/5.0 (Windows NT 6.1; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.90 Safari/537.36",
  "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.78 Safari/537.36 OPR/47.0.2631.39",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_6) AppleWebKit/603.3.8 (KHTML, like Gecko) Version/10.1.2 Safari/603.3.8",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_12_3) AppleWebKit/602.4.8 (KHTML, like Gecko) Version/10.0.3 Safari/602.4.8",
  "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.12; rv:56.0) Gecko/20100101 Firefox/56.0"
)

ACCEPT_ENCODING <- c(
  'gzip, deflate',
  'gzip, deflate, compress',
  'gzip,deflate,sdch')

# functions
get_header <- function(){
  ua <- sample(USER_AGENTS, 1L)
  return(ua)
}
get_encoding <- function(){
  ae <- sample(ACCEPT_ENCODING, 1L)
  return(ae)
}


#--------------#
#              #
#   FUNKCIJE   #
#              #
#--------------#



parse_proizvodi <- function(url) {
  p <- tryCatch({
    read_html(url, user_agent = get_header()) %>% 
      html_node(xpath = '//*[@id="js-product-list"]') %>%
      html_nodes("a") %>% 
      html_attr("href") %>% 
      .[str_detect(., "/\\d+")] %>% 
      unique(.)
  }, error = function(e) NA)
  return(p)
}
parse_proizvodi_vise_pokusaja <- function(url, broj_pokusaja = 5) {
  pokusaj <- 1
  proizvodi_j <- parse_proizvodi(url)
  while (pokusaj <= broj_pokusaja & any(is.na(proizvodi_j))) {
    proizvodi_j <- parse_proizvodi(url)
    pokusaj <- pokusaj + 1
    print(paste0("Pokusaj: ", pokusaj))
  }
  return(proizvodi_j)
}


parse_proizvodi_ <- function(url) {
  p <- tryCatch({
    read_html(url, user_agent = get_header()) 
  }, error = function(e) NA)
  return(p)
}
parse_proizvodi_vise_pokusaja_ <- function(url, broj_pokusaja = 5) {
  pokusaj <- 1
  proizvodi_j <- parse_proizvodi_(url)
  while (pokusaj <= broj_pokusaja & any(is.na(proizvodi_j))) {
    proizvodi_j <- parse_proizvodi_(url)
    pokusaj <- pokusaj + 1
    print(paste0("Pokusaj: ", pokusaj))
  }
  return(proizvodi_j)
}







page  <- "https://channelcrawler.com/eng/results/48267/sort:Channel.subscribers/direction:asc"

nums <- seq(2,50)

links <- c(page,
           paste0("https://www.channelcrawler.com/eng/results/48267/page:"
                       ,nums,
                       "/sort:Channel.subscribers/direction:asc"))



parser <- function(url) {

Sys.sleep(runif(1, min = 3, max = 11))
  
name <- read_html(url) %>%
  html_nodes(., "h4") %>% 
  html_nodes(., "a") %>%
  html_attr(.,"title") 
  
link <- read_html(url) %>%
  html_nodes(., "h4") %>% 
  html_nodes(.,"a") %>%
  html_attr(.,"href") 
  
genre <- read_html(url) %>%
  html_nodes(.,  "b") %>%
  .[seq(1,length(.),2)] %>%
  html_text() 

meta <- read_html(url) %>%
  html_nodes(.,"#main-content.container") %>% 
  html_nodes(.,  "p") %>%
  html_nodes(., "small") %>% 
  .[seq(1,length(.),2)] %>%
  html_text()  %>%
  str_replace_all(., "[\t]" , "") %>% 
  gsub("\n", "*", .) %>%
  data.frame(do.call("rbind", strsplit(as.character(.), "*", fixed = TRUE))) %>%
  select(X2:X5)

subscribers <- meta$X2 %>%
  gsub(" Subscribers", "",. ) %>%
  gsub("M", "000000",.) %>%
  gsub("K","000",.) %>%
  gsub("\\.", "",.) %>%
  as.numeric() 

videos <- meta$X3 %>%
  gsub(" Videos","",.) %>%
  as.numeric()

views <- meta$X4 %>%
  gsub(" Total Views","",.) %>%
  gsub("B", "00000000",.) %>%
  gsub("M","000000",.) %>%
  gsub("K","000",.) %>%
  gsub("\\.", "",.) %>%
  as.numeric()
  
lastvideo <- meta$X5 %>%
  gsub("Latest Video: ", "",.) %>% 
  mdy("%b %d %Y") %>%
  na.omit()

description <- cbind.data.frame(name, genre, subscribers, videos, 
                                    views, lastvideo,link, # link_proizvod, # brand_,
                                    stringsAsFactors = FALSE)

return(description)

}




all <- lapply(links[1:10],parser)
all2 <- lapply(links[11:20],parser)
all3 <- lapply(links[21:30],parser)
all4 <- lapply(links[31:40],parser)
all5 <- lapply(links[41:50],parser)

allLIst <- c(all, all2, all3, all4, all5)

df <- bind_rows(allLIst, .id = "column_label")


lowYT <- bind_rows(df)


write.csv2(lowYT, file = "./data/lowYT.csv")







