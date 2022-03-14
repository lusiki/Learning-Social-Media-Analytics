---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 3: Data Science Prerequisites"
author: "Luka Sikic, PhD"
date: "Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)" #"14 ožujak 2022"
output:
  html_document:
    code_folding: show
    theme: flatly
    highlight: haddock
    toc: yes
    toc_depth: 4
    toc_float: yes
    keep_md: yes
  pdf_document:
    toc: yes
    toc_depth: '4'
---









# FETCHING DATA


### Two main approaches:

1. **webscraping (guerrila)**


- takes some skills
<br>
- everything can be scraped (tailor made solutions)
<br>
- takes more time and effort
<br>


2. **API (gentelman`s way)**

- often not avaliable 
<br>
- usually not for free
<br>
- easier to implement in production


### **webscraping**
  
- lets scrape [this](https://www.vecernji.hr/vijesti/umro-bivsi-austrijski-potkancelar-i-prijatelja-hrvatske-dr-erhard-busek-1570780) article and [this](https://www.vecernji.hr/vijesti/u-kijevu-pogodena-stambena-zgrada-objavljena-snimka-raketiranja-nebodera-u-mariupolju-1570614) from [Večernji list](https://www.vecernji.hr/)



```r
# 1. copy urls
url1 <- "https://www.vecernji.hr/vijesti/umro-bivsi-austrijski-potkancelar-i-prijatelja-hrvatske-dr-erhard-busek-1570780"

url2 <- "https://www.vecernji.hr/vijesti/u-kijevu-pogodena-stambena-zgrada-objavljena-snimka-raketiranja-nebodera-u-mariupolju-1570614"
# 2. request page 
page1 <- html_session(url1)
```

- we need to write a function to *grab* parts of the article
- this part takes some routine skill and revolves around `rvest` package
  

```r
# 3. write a function to take parts of the article
parseArticle <- function(webpage) {
  
  title <- html_nodes(webpage, xpath = '//h1[@class="article__title"]') %>%
    html_text() %>% 
    trimws() %>% 
    ifelse(length(.) == 0, NA, .)
  
  date <- html_nodes(webpage, xpath = '//*[@class="article__header_date"]') %>%
    html_text() %>% 
    str_replace_all(pattern = "\\\r\\\n| u", replacement = "") %>% 
    trimws() %>% 
    ifelse(length(.) == 0, NA, .)
  
  noComment <- html_nodes(webpage, xpath = '//*[@class="article__comments_number"]') %>%
    html_text() %>% 
    trimws() %>% 
    str_extract("\\d+") %>% 
    as.numeric(.) %>% 
    ifelse(length(.) == 0, NA, .)
  
  views <- html_nodes(webpage, xpath = '//*[@class="article__header_views"]') %>%
    html_text() %>% 
    trimws() %>% 
    str_extract("\\d+") %>% 
    as.numeric(.) %>% 
    ifelse(length(.) == 0, NA, .)
  
  articleLabel <- html_nodes(webpage, xpath = '//*[@class="article__label"]') %>%
    html_text() %>% 
    trimws() %>% 
    ifelse(length(.) == 0, NA, .)
  
  author <- html_nodes(webpage, xpath = '//*[@class="article__author--link"]') %>%
    html_text() %>% 
    trimws()
  if (length(author) == 0) {author <- NA}
  
  articletext <- html_nodes(webpage, xpath = '//*[@class="article__body--main_content"]/p') %>%
    html_text() %>% 
    str_flatten(., "\n") %>% 
    ifelse(length(.) == 0, NA, .)
  
  keywords <- html_nodes(webpage, xpath = '//*[@class="article__tag_name"]') %>%
    html_text() %>% 
    trimws() %>% 
    str_flatten(., ";") %>% 
    ifelse(length(.) == 0, NA, .)
  
  
  articles <- cbind.data.frame(title, date, noComment, views, articleLabel, 
                              articleLabel, author, articletext, keywords, stringsAsFactors = FALSE)
  return(articles)
}
```

- finally, lets apply the function and check the results
  

```r
# 4. apply the function
data <- parseArticle(page1)
# 5. check the data
str(data)
```

```
## 'data.frame':	1 obs. of  9 variables:
##  $ title       : chr "Umro bivši austrijski potkancelar i prijatelja Hrvatske dr. Erhard Busek"
##  $ date        : chr "14. ožujka 2022. 16:48"
##  $ noComment   : num 0
##  $ views       : num 685
##  $ articleLabel: logi NA
##  $ articleLabel: logi NA
##  $ author      : chr "Snježana Herek"
##  $ articletext : chr "Bivši austrijski potkancelar i ministar znanosti i obrazovanja, dogradonačelnik Beča i čelnik Narodne stranke ("| __truncated__
##  $ keywords    : chr "Austrija;Erhard Busek"
```

```r
data$title
```

```
## [1] "Umro bivši austrijski potkancelar i prijatelja Hrvatske dr. Erhard Busek"
```

```r
data$views
```

```
## [1] 685
```

```r
nchar(data$articletext)
```

```
## [1] 5148
```

- we would want to automati this approach and apply it to multiple articles 
- let`s first find a couple of more interesting articles: [I](https://www.vecernji.hr/vijesti/glavni-tajnik-un-a-nuklearni-sukob-ponovno-izgleda-moguc-1570823),
[II](https://www.vecernji.hr/vijesti/sto-je-clanak-5-nato-a-aktiviran-je-samo-jednom-a-ne-moze-se-primijeniti-na-ukrajinu-1570810), [III](https://www.vecernji.hr/vijesti/kotromanovic-ako-je-bila-rijec-o-naoruzanom-dronu-napad-je-to-na-clanicu-nato-a-1570731)



```r
# assign urls of the articles
url3 <- "https://www.vecernji.hr/vijesti/glavni-tajnik-un-a-nuklearni-sukob-ponovno-izgleda-moguc-1570823"
url4 <- "https://www.vecernji.hr/vijesti/sto-je-clanak-5-nato-a-aktiviran-je-samo-jednom-a-ne-moze-se-primijeniti-na-ukrajinu-1570810"
url5 <- "https://www.vecernji.hr/vijesti/kotromanovic-ako-je-bila-rijec-o-naoruzanom-dronu-napad-je-to-na-clanicu-nato-a-1570731"
# bind all articles together
urls <- c(url1,url2,url3,url4,url5)
# check
urls
```

```
## [1] "https://www.vecernji.hr/vijesti/umro-bivsi-austrijski-potkancelar-i-prijatelja-hrvatske-dr-erhard-busek-1570780"              
## [2] "https://www.vecernji.hr/vijesti/u-kijevu-pogodena-stambena-zgrada-objavljena-snimka-raketiranja-nebodera-u-mariupolju-1570614"
## [3] "https://www.vecernji.hr/vijesti/glavni-tajnik-un-a-nuklearni-sukob-ponovno-izgleda-moguc-1570823"                             
## [4] "https://www.vecernji.hr/vijesti/sto-je-clanak-5-nato-a-aktiviran-je-samo-jednom-a-ne-moze-se-primijeniti-na-ukrajinu-1570810" 
## [5] "https://www.vecernji.hr/vijesti/kotromanovic-ako-je-bila-rijec-o-naoruzanom-dronu-napad-je-to-na-clanicu-nato-a-1570731"
```




```r
# read in urls
pages <- lapply(urls,html_session)
# grab all article parts
multipleArticles <- lapply(pages, parseArticle)
#make data.frame
dataArticles <- do.call(rbind, multipleArticles)
# check the data
dim(dataArticles)
```

```
## [1] 7 9
```

```r
glimpse(dataArticles)
```

```
## Rows: 7
## Columns: 9
## $ title        <chr> "Umro bivši austrijski potkancelar i prijatelja Hrvatske ~
## $ date         <chr> "14. ožujka 2022. 16:48", "14. ožujka 2022. 20:31", "14. ~
## $ noComment    <dbl> 0, 98, 98, 0, 0, 131, 131
## $ views        <dbl> 685, 62967, 62967, 235, 243, 53953, 53953
## $ articleLabel <lgl> NA, NA, NA, NA, NA, NA, NA
## $ articleLabel <lgl> NA, NA, NA, NA, NA, NA, NA
## $ author       <chr> "Snježana Herek", "Vecernji.hr", "Hina", "Hina", "Vecernj~
## $ articletext  <chr> "Bivši austrijski potkancelar i ministar znanosti i obraz~
## $ keywords     <chr> "Austrija;Erhard Busek", "invazija;napad;Rusija;rat;Ukraj~
```

```r
dataArticles$title
```

```
## [1] "Umro bivši austrijski potkancelar i prijatelja Hrvatske dr. Erhard Busek"                                        
## [2] "Rusi raznijeli streljivo ispred nuklearne elektrane Zaporižja? Moskva najavila opasnu namjeru"                   
## [3] "Rusi raznijeli streljivo ispred nuklearne elektrane Zaporižja? Moskva najavila opasnu namjeru"                   
## [4] "Glavni tajnik UN-a: Nuklearni sukob ponovno izgleda moguć"                                                       
## [5] "Što je članak 5. NATO-a? Aktiviran je samo jednom, a ne može se primijeniti na Ukrajinu"                         
## [6] "Kotromanović: Ako je dron bio naoružan, onda NATO mora aktivirati članak 5 bez obzira je li ruski ili ukrajinski"
## [7] "Kotromanović: Ako je dron bio naoružan, onda NATO mora aktivirati članak 5 bez obzira je li ruski ili ukrajinski"
```

```r
dataArticles$views
```

```
## [1]   685 62967 62967   235   243 53953 53953
```

```r
nchar(dataArticles$articletext)
```

```
## [1]  5148 22405 22405  1558  3970  5675  5675
```





## STORING DATA

## MANIPULATING DATA

## ANALYTICS

## REPORTING

# CONNCLUDING POINTS



