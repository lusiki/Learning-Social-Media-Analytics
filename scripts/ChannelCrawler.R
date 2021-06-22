
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


####################
#SCRAP MOJA_LJEKARNA
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



#, brand
#, o
parsanjeProizvoda <- function(url) {
  
  
  naziv <-  html_node(url, css = "div.channel:nth-child(1) > h4:nth-child(1) > a:nth-child(1)") %>%
    html_text() %>%
    ifelse(length(.) == 0, NA, .)
  
  cijena <- html_node(url, ".flex-container") %>%
    html_text() %>%
    str_extract(.,"\\d.*K") %>%
    ifelse(length(.) == 0, NA, .)
  
  opis_kratki <- html_node(url, "#product-description-short > p:nth-child(1)") %>%
    html_text() %>%
    ifelse(length(.) == 0, NA, .)
  
  kategorije <- html_node(url, "#product-categories") %>%
    html_text() %>%
    ifelse(length(.) == 0, NA, .) %>%
    gsub(".*[[:blank:]]", "",.) %>%
    gsub("\n"," ",.) %>%
    trimws() %>%
    str_squish() %>%
    gsub(" ",";",.)
  
  opis_dugi <- html_node(url, css = "div.product-description:nth-child(1)") %>%
    html_text()%>%
    ifelse(length(.) == 0, NA, .)
  
  
  detalji_proizvod <- html_node(url, css = "section.product-features:nth-child(4) > dl:nth-child(2)") %>%
    html_text() %>%
    ifelse(length(.) == 0, NA, .)
  
  link_img <- html_node(url, css = '.js-qv-product-cover' ) %>%
    html_attr('src') %>%
    .[1] %>%
    ifelse(length(.) == 0, NA, .)
  
  #  link_proizvod <- o 
  
  #  brand_ <- brand %>%
  #    str_extract(., "\\-(.*)") %>%
  #    gsub("-", " ",.) %>%
  #    trimws() %>%
  #    toupper() %>%
  #    str_extract(., "(^.*)?\\?") %>%
  #    gsub("\\?", "",.)
  
  
  podatciProizvod <- cbind.data.frame(naziv, opis_kratki, cijena, opis_dugi, kategorije, 
                                      detalji_proizvod, link_img,# link_proizvod, # brand_,
                                      stringsAsFactors = FALSE)
  
  return(podatciProizvod)
  
}










#--------------#
#              #
#    SCRAP     #
#              #
#--------------#






proizvodaci <- read_html("https://www.mojaljekarna.hr/proizvodjaci", encoding = "UTF-8") %>% 
  html_node(xpath = "//*[@id='main']/div") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  .[str_detect(., "/\\d+")]



brandoviLoop <- list()
proizvodi <- c()
for (i in 1:100) { #seq_along(proizvodaci)
  
  # dodati 10 stranica na linkove kako bi obuhvatio sve
  proizvodaci_i <- proizvodaci[i] %>% 
    paste0(., "?page=", 1:6)
  
  # loop kroz sve, prekini kada stranica nema vise proizvoda
  for (j in seq_along(proizvodaci_i)) {
    proizvodi_j <- parse_proizvodi_vise_pokusaja(proizvodaci_i[j])
    
    if (length(proizvodi_j) == 0) {
      break()
    } else{
      proizvodi <- c(proizvodi, proizvodi_j)
    }
    Sys.sleep(0.3)
  } 
  
  
  lista <- list()
  
  for(k in seq_along(proizvodi)) {
    
    proizvod <- parse_proizvodi_vise_pokusaja_(proizvodi[k])
    
    #    brand <- proizvodaci_i[j]
    #    o <- proizvodi[k]
    lista[[k]] <- parsanjeProizvoda(proizvod)#,o) #, brand)
  }
  
  brandoviLoop[[i]] <- do.call(rbind, lista)
}



proizvodiSave100 <- do.call(rbind, brandoviLoop)


save(proizvodiSave, file = "my_ljekarna.xlsx")

myl <- readRDS("E:/Luka/FREELANCE/APOTHE_KOS/my_ljekarna.rds")

saveRDS(proizvodiSave, file = "my_ljekarna.rds")




url  <- "https://channelcrawler.com/eng/results/48267/sort:Channel.subscribers/direction:desc"
page <- read_html(url)


naziv <- page %>%
  html_nodes(., "h4") %>% 
  html_nodes(., "a") %>%
  html_attr(.,"title")
  
links <- page %>%
  html_nodes(., "h4") %>% 
  html_nodes(.,"a") %>%
  html_attr(.,"href")
  
zanr <- page %>%
  html_nodes(.,  "b") %>%
  .[seq(1,length(.),2)] %>%
  html_text()

meta <- page %>%
  html_nodes(.,"#main-content.container") %>% 
  html_nodes(.,  "p") %>%
  html_nodes(., "small") %>% 
  .[seq(1,length(.),2)] %>%
  html_text()

subscr <- meta %>% str_extract(meta,"[^bers]")

x <- as.data.frame(meta)
x %>% separate(x, c("A","B"), sep = ',')

x %>% gsub("\\t","",.)

str_replace_all(meta, "[\t]" , "")




