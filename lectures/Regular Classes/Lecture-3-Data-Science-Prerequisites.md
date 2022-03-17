---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 3: Data Science Prerequisites"
author: "Luka Sikic, PhD"
date: "Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)" #"17 ožujak 2022"
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
##  $ views       : num 959
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
## [1] 959
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

- lets automate this procedure for multiple articles now and check the data


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
## $ date         <chr> "14. ožujka 2022. 16:48", "14. ožujka 2022. 23:03", "14. ~
## $ noComment    <dbl> 0, 107, 107, 8, 19, 132, 132
## $ views        <dbl> 959, 90294, 90294, 3039, 7624, 74506, 74506
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
## [2] "Teške borbe u Donbasu, američki dužnosnik tvrdi: Rusko napredovanje gotovo potpuno zaustavljeno"                 
## [3] "Teške borbe u Donbasu, američki dužnosnik tvrdi: Rusko napredovanje gotovo potpuno zaustavljeno"                 
## [4] "Glavni tajnik UN-a: Nuklearni sukob ponovno izgleda moguć"                                                       
## [5] "Što je članak 5. NATO-a? Aktiviran je samo jednom, a ne može se primijeniti na Ukrajinu"                         
## [6] "Kotromanović: Ako je dron bio naoružan, onda NATO mora aktivirati članak 5 bez obzira je li ruski ili ukrajinski"
## [7] "Kotromanović: Ako je dron bio naoružan, onda NATO mora aktivirati članak 5 bez obzira je li ruski ili ukrajinski"
```

```r
dataArticles$views
```

```
## [1]   959 90294 90294  3039  7624 74506 74506
```

```r
nchar(dataArticles$articletext)
```

```
## [1]  5148 27867 27867  1558  3970  5675  5675
```

### **API**

- lets quickly inspect the [API documentation](https://documenter.getpostman.com/view/7210955/S1EMUebv?version=latest) 
- then we need to compile the full API request and retrieve the data


```r
# this is a private infor
source(here::here("Creds/api.R"))
# identify from your Mediatoolkit App
groups <- "182718"
keywords <- "6521533"
# select time period
from_time <- as.character(as.numeric(as.POSIXlt("2022-03-13", format="%Y-%m-%d")))
to_time <- as.character(as.numeric(as.POSIXlt("2022-03-14", format="%Y-%m-%d")))
# number of articles to retrieve
count <- 3000
# connect all parts into request string
requestString <- paste0("https://api.mediatoolkit.com/organizations/126686/groups/",groups,
              "/keywords/",keywords,
              "/mentions?access_token=",token,
              "&from_time=",from_time,
              "&to_time=",to_time,
              "&count=",count,
              "&sort=time&type=all&offset=0&ids_only=false")
# check the request string
requestString
```

```
## [1] "https://api.mediatoolkit.com/organizations/126686/groups/182718/keywords/6521533/mentions?access_token=ddms5s0l3gejlz2z42ydt0bnwmf6ssqd62bdxteu7t8sumv5ii&from_time=1647126000&to_time=1647212400&count=3000&sort=time&type=all&offset=0&ids_only=false"
```

```r
# make GET request to Mediatoolkit server API
API_request <- httr::GET(requestString)
# check the API request object
API_request
```

```
## Response [https://api.mediatoolkit.com/organizations/126686/groups/182718/keywords/6521533/mentions?access_token=ddms5s0l3gejlz2z42ydt0bnwmf6ssqd62bdxteu7t8sumv5ii&from_time=1647126000&to_time=1647212400&count=3000&sort=time&type=all&offset=0&ids_only=false]
##   Date: 2022-03-17 07:14
##   Status: 200
##   Content-Type: application/json;charset=utf-8
##   Size: 3.8 MB
```

```r
# parse the request into JSON object
jS_text <- httr::content(API_request, as = "text", type = "aplication/json", encoding = "UTF-8")
# make a list from JSON object
dataList <- jsonlite::fromJSON(jS_text, flatten = TRUE)
# make a data.frame from list
data <- data.frame(dataList$data)
```

- now we have the retrieved data in a data.frame object
- let`s check what is inside


```r
# size of the data
dim(data)
```

```
## [1] 3000   51
```

```r
# variables and variable types
glimpse(data)
```

```
## Rows: 3,000
## Columns: 51
## $ response.comment_count              <int> 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 4~
## $ response.keywords                   <list> "i", "i", <"i", "I">, "i", "i", "~
## $ response.pinterest_count            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ response.reach                      <int> 697, 15, 1576, 1167, 411, 1734, 64~
## $ response.insert_time                <int> 1647212400, 1647212400, 1647212400~
## $ response.description                <chr> "Mediji u Srbiji pišu kako je novi~
## $ response.engagement_rate            <dbl> 4.5919283, 0.0000000, 1.2692537, 0~
## $ response.type                       <chr> "web", "web", "web", "web", "web",~
## $ response.title                      <chr> "PREDSJEDNIČKI IZBORI Škoro se zbo~
## $ response.original_photos            <list> "https://direktno.hr/upload/publi~
## $ response.photos                     <list> "https://mediatoolkit.com/img/0x5~
## $ response.mention                    <chr> "Ja sam čovek s biroa za nezaposle~
## $ response.original_photo             <chr> "https://direktno.hr/upload/publis~
## $ response.score                      <dbl> 1647212400, 1647212400, 1647212400~
## $ response.all_keyword_feed_locations <list> [<data.frame[1 x 2]>], [<data.fra~
## $ response.mozrank                    <dbl> 5.864264, 0.000000, 1.400000, 6.20~
## $ response.from                       <chr> "direktno.hr", "cazma.hr", "najbol~
## $ response.id                         <dbl> 9143912283, 9145111616, 9145128040~
## $ response.auto_sentiment             <chr> "negative", "positive", "positive"~
## $ response.database_insert_time       <int> 1647212792, 1647251035, 1647251371~
## $ response.keyword_name               <chr> "opće", "opće", "opće", "opće", "o~
## $ response.image                      <chr> "https://mediatoolkit.com/img/50x5~
## $ response.like_count                 <int> 24, 0, 6, 0, 6, 0, 34, 0, 0, 0, 0,~
## $ response.languages                  <list> "hr", "hr", "hr", "hr", "hr", "hr~
## $ response.group_name                 <chr> "Luka", "Luka", "Luka", "Luka", "L~
## $ response.pr_value                   <int> 10, 0, 8, 58, 2, 17, 3, 142, 58, 1~
## $ response.photo                      <chr> "https://mediatoolkit.com/img/0x50~
## $ response.influence_score            <int> 3, 1, 1, 7, 1, 2, 1, 7, 7, 3, 7, 2~
## $ response.url                        <chr> "https://direktno.hr/eu-i-svijet/p~
## $ response.virality                   <dbl> 0.65606254, 0.00000000, 0.70409712~
## $ response.share_count                <int> 6, 0, 12, 0, 2, 13, 3, 0, 0, 0, 0,~
## $ response.linkedin_count             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ response.source_reach               <int> 4300, 200, 10, 80000, 10, 800, 90,~
## $ response.domain                     <chr> "direktno.hr", "cazma.hr", "najbol~
## $ response.tag_feed_locations         <list> [], [], [], [], [], [], [], [], [~
## $ response.interaction                <int> 32, 0, 20, 0, 8, 13, 37, 0, 0, 0, ~
## $ response.locations                  <list> "HR", "HR", "HR", "HR", "HR", "HR~
## $ response.author                     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.youtube_channel_id         <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.full_mention               <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.view_count                 <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.twid                       <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_comment_id          <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_parent_link_id      <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.subreddit                  <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_type                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_score               <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_fullname            <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.is_placeholder             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_comment_count       <int> NA, NA, NA, NA, NA, NA, NA, NA, NA~
## $ response.reddit_link_id             <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA~
```

```r
# check if there are articles from Večernji list
data %>% 
  group_by(response.type) %>%
  count %>%
  arrange(desc(n)) %>%
  head()
```

```
## # A tibble: 6 x 2
## # Groups:   response.type [6]
##   response.type     n
##   <chr>         <int>
## 1 web            1314
## 2 twitter         818
## 3 facebook        265
## 4 reddit          247
## 5 youtube         193
## 6 comment         115
```

```r
data %>%
  filter(response.from == "vecernji.hr") %>%
  select(title = response.title,
         time = response.insert_time,
         author = response.author,
         url = response.url,
         comment = response.comment_count,
         text = response.mention) %>%
  arrange(desc(comment)) %>%
  head
```

```
##                                                                                                                 title
## 1                        Vučić o padu drona u Zagrebu: 'To se Srbiji ne bi dogodilo, mi bi to oborili za pet minuta!'
## 2                               Visoki izvor iz MORH-a: Bomba u letjelici težine do 120 kg, eksplodirala ispod zemlje
## 3  Putinovi specijalci upadaju u rat, kreće totalni napad na Kijev. Naš general: Moguće je da Rusi ovo neće izdržati!
## 4 Ljudi u strahu od rata dižu štednju i kupuju stanove. Može li se ponoviti katastrofa iz 2008. i kako spasiti novac?
## 5          Vojni stručnjak: Da je eksplodiralo 120 kg eksploziva, sumnjam da bismo imali bilo kakve ostatke bilo čega
## 6        Nastavljaju se žestoke borbe, najteža situacija je u Mariupolju gdje se ljudi tuku za komad kruha i kap vode
##         time author
## 1 1647199162   <NA>
## 2 1647196367   <NA>
## 3 1647209120   <NA>
## 4 1647201866   <NA>
## 5 1647202093   <NA>
## 6 1647206736   <NA>
##                                                                                                                                                        url
## 1                          https://www.vecernji.hr/vijesti/vucic-o-padu-drona-u-zagrebu-to-se-srbiji-ne-bi-dogodilo-mi-bi-to-oborili-za-pet-minuta-1570574
## 2                               https://www.vecernji.hr/vijesti/slijedi-istraga-avio-bomba-koja-je-pronadena-u-letjelici-teska-je-do-120-kilograma-1570559
## 3   https://www.vecernji.hr/vijesti/putinovi-specijalci-upadaju-u-rat-krece-totalni-napad-na-kijev-nas-general-moguce-je-da-rusi-ovo-nece-izdrzati-1570576
## 4 https://www.vecernji.hr/vijesti/ljudi-u-strahu-od-rata-dizu-stednju-i-kupuju-stanove-moze-li-se-ponoviti-katastrofa-iz-2008-i-kako-spasiti-novac-1570555
## 5         https://www.vecernji.hr/vijesti/vojni-strucnjak-da-je-eksplodiralo-120-kg-eksploziva-sumnjam-da-bismo-imali-bilo-kakve-ostatke-bilo-cega-1570584
## 6      https://www.vecernji.hr/vijesti/nastavljaju-se-zestoke-borbe-najteza-situacija-je-u-mariupolju-gdje-se-ljudi-tuku-za-komad-kruha-i-kap-vode-1570572
##   comment
## 1     820
## 2     421
## 3     230
## 4     181
## 5     122
## 6      37
##                                                                                                                                                                                                                                                        text
## 1    Vučić o padu drona u Zagrebu: 'To se Srbiji ne bi dogodilo, mi bi to oborili za pet minuta!' Na predizbornom skupu dotaknuo se i tema poput Ukrajine, NATO-a, pada drona u centru Zagreba, ali i spremnosti i opremljenosti vojske. Srpski predsjednik
## 2   Visoki izvor iz MORH-a: Bomba u letjelici težine do 120 kg, eksplodirala ispod zemlje Nakon što je izvučena postavljena je uz sami krater, pregledali su je policijski i vojni službenici. Jutros je izvučena olupina letjelice koja je pala u četvrtak
## 3  Rusi će pojačavati pritisak putem raketa srednjeg dometa, a avijacijom uništavati ceste Ruske su se snage obrušile su se tijekom vikenda na gradove i naselja na zapadu Ukrajine, gdje se do sada nisu vodile tako žestoke borbe. Projektili su pogodili
## 4 Ljudi u strahu od rata dižu štednju i kupuju stanove. Može li se ponoviti katastrofa iz 2008. i kako spasiti novac? Na tržištu nekretnina nije uočen strah od rata i krize. Štoviše, cijene kvadrata rastu, u dvije godine u Zagrebu su poskupjeli za oko
## 5  Vojni stručnjak Robert Barić gostovao je u Dnevniku HTV-a i komentirao pad letjelice na Zagreb. Naime, iz visokih izvora u MORH-u doznaje se da je bomba pronađena u letjelici koja je pala na Zagreb bila težine do 120 kilograma. Srećom, letjelica je
## 6   Nastavljaju se žestoke borbe, najteža situacija je u Mariupolju gdje se ljudi tuku za komad kruha i kap vode Baza Javoriv, 10 kilometara od poljske granice, obično se koristi za obuku i vježbe ukrajinske vojske i njezinih NATO partnera, uglavnom u
```

```r
data %>% 
  slice(1) %>% 
  select(title = response.title)
```

```
##                                                                  title
## 1 PREDSJEDNIČKI IZBORI Škoro se zbog nedostatka novca neće kandidirati
```

```r
data %>% 
  slice(1) %>%
  select(text = response.mention)
```

```
##                                                                                                                                                                                                                                               text
## 1 Ja sam čovek s biroa za nezaposlene gdje ću i ostati i posijle ovih izbora. Na žalost, nisam uspio da sakupim dovoljno novca da bih imao kakvu takvu skromnu kampanju, a istovrsremeno nisam želio da pristanem da budem dio nekih kombinacija i
```

```r
data %>% 
  slice(1) %>%
  select(url = response.url)
```

```
##                                                                                                            url
## 1 https://direktno.hr/eu-i-svijet/predsjednicki-izbori-skoro-se-zbog-nedostatka-novca-nece-kandidirati-263796/
```



## STORING DATA

## MANIPULATING DATA

#### TIDY WAY

- in R there are three major ways to manipulate data: [base](https://iqss.github.io/dss-workshops/R/Rintro/base-r-cheat-sheet.pdf), [tidyverse](https://www.tidyverse.org/), [data.table](https://www.machinelearningplus.com/data-manipulation/datatable-in-r-complete-guide/)
- you can also combine them together
- we are going to explore and use tidywerse and data.table syntax in this course
- let`s check some important syntax
- check this famous [paper](https://vita.had.co.nz/papers/tidy-data.pdf) (Hadley Wickham, 2014 JSS) to motivate the tidyverse way and check [tidyverse](https://www.tidyverse.org/) ecosystem
- basic dplyr (tidyverse) syntax includes the following:

1. `filter`: filter (i.e. *subset*) rows by value.


```r
# get the number of web articles/activity: FILTER
data %>% 
  filter(response.type == "web") %>% # FILTER!
  summarise(NumberOfArticles = n())
```

```
##   NumberOfArticles
## 1             1314
```

2. `arrange`: order (i.e. *reorder*) rows by value.


```r
# arrange by share
data %>% 
  filter(response.type == "web") %>%
  group_by(response.from) %>% 
  summarise(Share = mean(response.share_count),
            Reach = mean(response.reach),
            Virality = mean(response.virality),
            LikeCount = mean(response.like_count),
            Comment = mean(response.comment_count)) %>%
  arrange(desc(Share)) # ARRANGE!
```

```
## # A tibble: 273 x 6
##    response.from             Share  Reach Virality LikeCount Comment
##    <chr>                     <dbl>  <dbl>    <dbl>     <dbl>   <dbl>
##  1 kgz.hr                   1750   129784  242.         869      88 
##  2 platak.hr                 563    10601    1           11       0 
##  3 logicno.com               117    15919    2.10       191      19 
##  4 geopolitika.news           99     7283    1.86       304     163 
##  5 ampeu.hr                   77     9297    0.800       40       0 
##  6 hkig.hr                    61     5141    6.00        21       1 
##  7 lisinski.hr                42     6895    2.40        44       5 
##  8 zagorje-international.hr   41     4515    1.11        26       6 
##  9 sloboda.hr                 39    69720    2.78       497     544 
## 10 priznajem.hr               34.2  24550    1.64       422.    153.
## # ... with 263 more rows
```

```r
# arrange by reach  
data %>% 
  filter(response.type == "web") %>%
  group_by(response.from) %>% 
  summarise(Reach = mean(response.reach),
            Share = mean(response.share_count),
            Virality = mean(response.virality),
            LikeCount = mean(response.like_count),
            Comment = mean(response.comment_count)) %>%
  arrange(desc(Reach)) # ARRANGE!
```

```
## # A tibble: 273 x 6
##    response.from   Reach  Share Virality LikeCount Comment
##    <chr>           <dbl>  <dbl>    <dbl>     <dbl>   <dbl>
##  1 kgz.hr        129784  1750   242.          869     88  
##  2 sloboda.hr     69720    39     2.78        497    544  
##  3 dw.com         41956    17     0.0438        2      1  
##  4 bongacams.com  40517     0     0             0      0  
##  5 priznajem.hr   24550    34.2   1.64        422.   153. 
##  6 index.hr       17231.   14.9   2.98        324.   139. 
##  7 logicno.com    15919   117     2.10        191     19  
##  8 jutarnji.hr    15493.   14.8   1.24        152.    53.5
##  9 24sata.hr      15168.   10.9   1.37        209.    79.4
## 10 zara.com       14313     0     0             0      0  
## # ... with 263 more rows
```

```r
# arrange by comment 
data %>% 
  filter(response.type == "web") %>%
  group_by(response.from) %>% 
  summarise(Comment = mean(response.comment_count)) %>%
  arrange(desc(Comment)) # ARRANGE!
```

```
## # A tibble: 273 x 2
##    response.from           Comment
##    <chr>                     <dbl>
##  1 sloboda.hr                544  
##  2 geopolitika.news          163  
##  3 priznajem.hr              153. 
##  4 index.hr                  139. 
##  5 morski.hr                 132. 
##  6 teleskop.hr               108  
##  7 novidani.com              105  
##  8 zagreb.info               103. 
##  9 kgz.hr                     88  
## 10 braniteljski-portal.com    83.3
## # ... with 263 more rows
```

3. `select`: Choose (i.e. *subset*) columns by name. 



```r
# get to know your data I
data %>% 
  select(response.from, response.title, response.url) %>% # SELECT!
  filter(response.from == "geopolitika.news") 
```

```
##      response.from
## 1 geopolitika.news
##                                                         response.title
## 1 MMF više ne smatra nemogućim da Rusija prestane plaćati svoje dugove
##                                                                                                 response.url
## 1 https://www.geopolitika.news/vijesti/mmf-vise-ne-smatra-nemogucim-da-rusija-prestane-placati-svoje-dugove/
```

```r
# get to know your data II
data %>% 
  select(response.from, response.title, response.url) %>% # SELECT!
  filter(response.from == "priznajem.hr") 
```

```
##   response.from
## 1  priznajem.hr
## 2  priznajem.hr
## 3  priznajem.hr
## 4  priznajem.hr
## 5  priznajem.hr
## 6  priznajem.hr
## 7  priznajem.hr
## 8  priznajem.hr
##                                                                                                   response.title
## 1                          Vučić: Neću nikoga plašiti, ali situacija je sve teža. Pogledajte sad ruske medije...
## 2                             Kalinić: Od Banožićevih bisera je ostalo samo još da kaže da je Zemlja ravna ploča
## 3                                  Vučić: Da se nama dogodilo ono u Zagrebu, mi bismo dron oborili za pet minuta
## 4                  Rusi kroz ‘srpsku rupu‘ ulaze u Europu ‘sazad‘: Razgrabljene karte na letovima Moskva-Beograd
## 5              Specijalac koji je ubio Osamu bin Ladena o Putinu: 'Ne možete poraziti luđaka pokazujući slabost'
## 6 Ukrajina objavila nove podatke o ruskim vojnim gubicima, evo što su im Ukrajinci uništili od početka invazije!
## 7            Anonymousi objavili videoporuku specijalno namijenjenu građanima Rusije: "Uklonite Putina s vlasti"
## 8               POSTOJI ŠANSA ZA MIR: Veliki preokret, postignut veliki napredak u pregovorima Ukrajine i Rusije
##                                                                                                                                        response.url
## 1                                                                                                                    https://priznajem.hr/?p=179023
## 2                            https://priznajem.hr/novosti/kalinic-od-banozicevih-bisera-je-ostalo-samo-jos-da-kaze-da-je-zemlja-ravna-ploca/179020/
## 3                                  https://priznajem.hr/novosti/vucic-da-se-nama-dogodilo-ono-u-zagrebu-mi-bismo-dron-oborili-za-pet-minuta/179018/
## 4                     https://priznajem.hr/novosti/rusi-kroz-srpsku-rupu-ulaze-u-europu-sazad-razgrabljene-karte-na-letovima-moskva-beograd/179015/
## 5               https://priznajem.hr/novosti/specijalac-koji-je-ubio-osamu-bin-ladena-o-putinu-ne-mozete-poraziti-ludaka-pokazujuci-slabost/179012/
## 6 https://priznajem.hr/novosti/ukrajina-objavila-nove-podatke-o-ruskim-vojnim-gubicima-evo-sto-su-im-ukrajinci-unistili-od-pocetka-invazije/179009/
## 7             https://priznajem.hr/novosti/anonymousi-objavili-videoporuku-specijalno-namijenjenu-gradanima-rusije-uklonite-putina-s-vlasti/179006/
## 8                                                                                                                    https://priznajem.hr/?p=179004
```

```r
# get to know your data III
data %>% 
  select(response.from, response.title, response.url) %>% # SELECT!
  filter(response.from == "sloboda.hr")   
```

```
##   response.from
## 1    sloboda.hr
##                                                                                                           response.title
## 1 LOŠ(O) ANALITIČAR: "Ako bi izbio rat, a neće, 60% ukrajinske vojske prelazi Rusima i sve se rješava u 48 sati" (VIDEO)
##                                                                                                                           response.url
## 1 https://www.sloboda.hr/loso-analiticar-ako-bi-izbio-rat-a-nece-60-ukrajinske-vojske-prelazi-rusima-i-sve-se-rjesava-u-48-sati-video/
```

4. `mutate`: Create new columns.



```r
# select biggest portals
data %>% 
  filter(response.type == "web") %>%
  group_by(response.from) %>% 
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  select(response.from) %>%
  pull -> largePortals
# check biggest portals
largePortals
```

```
##  [1] "novine.hr"            "index.hr"             "dnevnik.hr"          
##  [4] "360hr.news"           "vecernji.hr"          "ljekarnaonline.hr"   
##  [7] "slobodnadalmacija.hr" "24sata.hr"            "jutarnji.hr"         
## [10] "glas-slavonije.hr"
```

```r
# create negation operator
`%!in%` <- Negate(`%in%`)
# Create new column and check some descriptives
data %>% 
  filter(response.type == "web") %>%
  mutate(PortalSize = case_when(response.from %in% largePortals ~ "Large", # MUTATE!
                                response.from %!in% largePortals ~ "Small")) %>% 
  group_by(PortalSize) %>%
  count
```

```
## # A tibble: 2 x 2
## # Groups:   PortalSize [2]
##   PortalSize     n
##   <chr>      <int>
## 1 Large        375
## 2 Small        939
```




5. `summarise`: Make a descriptive summary.

- we already saw this function in action :-)
- let`s see another one



```r
data %>% 
  filter(response.type == "web") %>%
  summarise(Average = n())
```

```
##   Average
## 1    1314
```

```r
data %>%
  filter(response.type == "web") %>% 
  select()
```

```
## data frame with 0 columns and 1314 rows
```

#### DATA.TABLE WAY

-advantages of data.table include:

1. Concise syntax


```r
# read in library
library(data.table)
# check class
class(data)
```

```
## [1] "data.frame"
```

```r
# set the data.table object
dataDT = as.data.table(data) 
# check class again
class(dataDT)
```

```
## [1] "data.table" "data.frame"
```

```r
# do some descriptive statistics
dataDT[response.type == "web",
       .(minShare = min(response.share_count),
         maxShare = max(response.share_count),
         avgShare = mean(response.share_count),
         stdShare = sd(response.share_count))][]
```

```
##    minShare maxShare avgShare stdShare
## 1:        0     1750 7.160578 52.26276
```

```r
# how many letters in a title
dataDT[response.type == "web",
       .(Avg = mean(nchar(response.title)),
         STD = sd(nchar(response.title)),
         min = min(nchar(response.title)),
         max = max(nchar(response.title)))][]
```

```
##         Avg      STD min max
## 1: 70.76408 33.58205   4 160
```

```r
# how many letters in a text
dataDT[response.type == "web",
       .(Avg = mean(nchar(response.mention)),
         STD = sd(nchar(response.mention)),
         min = min(nchar(response.mention)),
         max = max(nchar(response.mention)))][]
```

```
##         Avg     STD min max
## 1: 241.7017 20.1523  54 250
```

2. Very fast


```r
library(tictoc)

# how many letters in a text by DT
tic()
dataDT[response.type == "web",
       .(Avg = mean(nchar(response.mention)),
         min = min(nchar(response.mention)),
         max = max(nchar(response.mention)))]
```

```
##         Avg min max
## 1: 241.7017  54 250
```

```r
toc()
```

```
## 0 sec elapsed
```

```r
tic()
# how many letters in a text by tidy
data %>% 
  group_by(response.type) %>%
  summarise(Avg = mean(nchar(response.mention)),
         min = min(nchar(response.mention)),
         max = max(nchar(response.mention)))
```

```
## # A tibble: 7 x 4
##   response.type   Avg   min   max
##   <chr>         <dbl> <int> <int>
## 1 comment        168.    34   250
## 2 facebook        49     49    49
## 3 instagram       50     50    50
## 4 reddit         108.     5   250
## 5 twitter         NA     NA    NA
## 6 web            242.    54   250
## 7 youtube        189.     9   250
```

```r
toc()
```

```
## 0.03 sec elapsed
```

```r
# READ IN FULL SAMPLE
path <- "D:/LUKA/Freelance/Mediatoolkit/FULLDATA"
raw <- list.files(path = path , pattern="xlsx")
raw_path <- paste0(path, "/", raw)
all_raw <- map_df(raw_path, read_excel)
# make data.table object
allDT <- as.data.table(all_raw)


# lets check average activity size across 
allDT[,
       .(Avg = mean(nchar(TITLE)),
         STD = sd(nchar(TITLE)),
         min = min(nchar(TITLE)),
         max = max(nchar(TITLE))),
       by = SOURCE_TYPE]
```

```
##    SOURCE_TYPE       Avg         STD min  max
## 1:       forum        NA          NA  NA   NA
## 2:         web        NA          NA  NA   NA
## 3:     twitter 178.27823 114.1908158   4 6030
## 4:      reddit 110.32908  79.2360687   1  350
## 5:     youtube  51.40096  23.5850392   1  100
## 6:     comment  22.04194   0.7407619  22   53
## 7:    facebook 127.80191  39.3007556   4  160
## 8:   instagram 176.66035  72.6184769   5  350
```

```r
allDT[SOURCE_TYPE == "twitter",.(Avg = mean(nchar(TITLE)),
         STD = sd(nchar(TITLE)),
         min = min(nchar(TITLE)),
         max = max(nchar(TITLE)))]
```

```
##         Avg      STD min  max
## 1: 178.2782 114.1908   4 6030
```

3. Memory efficiency

Measuring memory efficiency is relatively complicated [thing](https://stackoverflow.com/a/61376971). For details [check](https://jangorecki.gitlab.io/r-talks/2019-06-18_Poznan_why-data.table/why-data.table.pdf) (after 12th slide) check data.table functionality.

4. Lots of possibilities, stability and 5. Low dependancy

These elements are mutually related. Dependancy is related [to](http://www.tinyverse.org/):



```r
tools::package_dependencies("dplyr", recursive = TRUE)[[1]]
```




```r
tools::package_dependencies("data.table", recursive = TRUE)[[1]]
```
```
## [1] "methods"
```

- data.table has typical syntax:

.center[
.large2[DT[<span style='color: #66C2A5;'>i</span>, <span style='color: #FC8D62;'>j</span>, <span style='color: #8DA0CB;'>by</span>]]
]

![:col_row <span style='color: #66C2A5;'>What rows?</span>, <span style='color: #FC8D62;'>What to do?</span>, <span style='color: #8DA0CB;'>Group by...</span>]



.center[dplyr "equivalents":]
![:col_list <span style='color: #66C2A5;'>filter(); slice(); arrange()</span>, <span style='color: #FC8D62;'>select(); mutate()</span>, <span style='color: #8DA0CB;'>group_by()</span>]


- tidyverse works step by step and data.table does it in one step
- one operation is one flid thought
- chaining in DT is also possible
- let`check some of these possibilities


```r
# check 10 articles d+from vecernji.hr
allDT[SOURCE_TYPE == "web" & FROM == "vecernji.hr",
      .(TITLE,URL,COMMENT_COUNT)] 
```

```
##                                                                                                                            TITLE
##     1:         Pričali smo s Beograđanima: 'Nema šanse da ga je netko napao, mi volimo Splićane. Rijeka je na tom dijelu duboka'
##     2:                                                                              Boga korak do rekordnog transfera u Atalantu
##     3:                                                     Željka Kamenov: Neke nove navike iz doba korone vrijedilo bi zadržati
##     4:                                                                   Monaco bez Kovača izborio osminu finala, kraj za Rennes
##     5:                        Pirotehničar: Ove sam godine u Zagrebu za Novu čuo bombe i rafale automastkog oružja. Znam i zašto
##    ---                                                                                                                          
## 32402:                                              Svi radimo ovih 20 grešaka u kuhanju i tako uništavamo hranu - Ordinacija.hr
## 32403: Ovo je najčešći želučani problem zbog kojeg idemo liječniku, a evo i kako lijekovi utječu na vašu probavu - Ordinacija.hr
## 32404:                       Ne-Hodgkinov limfom - koji su simptomi i što sve morate znati o ovoj teškoj bolesti - Ordinacija.hr
## 32405:                                                 VIDEO Šok za PSG! U posljednjih deset godina nije tako rano ispao iz Kupa
## 32406:                           Čekamo novi "Oz" i pitamo se što je sljedeće - gay drvosječa? Možda Dorothy koja želi biti Don?
##                                                                                                                                                                    URL
##     1:            https://www.vecernji.hr/vijesti/pricali-smo-s-beogradanima-nema-sanse-da-ga-je-netko-napao-mi-volimo-splicane-rijeka-je-na-tom-dijelu-duboka-1552186
##     2:                                                                              https://www.vecernji.hr/sport/boga-korak-do-rekordnog-transfera-u-atalantu-1552174
##     3:                                                    https://www.vecernji.hr/vijesti/zeljka-kamenov-neke-nove-navike-iz-doba-korone-vrijedilo-bi-zadrzati-1551978
##     4:                                                                    https://www.vecernji.hr/sport/monaco-bez-kovaca-izborio-osminu-finala-kraj-za-rennes-1552184
##     5:                         https://www.vecernji.hr/zagreb/pirotehnicar-ove-sam-godine-u-zagrebu-za-novu-cuo-bombe-i-rafale-automastkog-oruzja-znam-i-zasto-1552183
##    ---                                                                                                                                                                
## 32402:                                          https://ordinacija.vecernji.hr/zdravi-tanjur/jedi-zdravo/svi-radimo-ovih-20-gresaka-u-kuhanju-i-tako-unistavamo-hranu/
## 32403: https://ordinacija.vecernji.hr/zdravlje/ohr-savjetnik/ovo-je-najcesci-zelucani-problem-zbog-kojeg-idemo-lijecniku-a-evo-i-kako-lijekovi-utjecu-na-vasu-probavu/
## 32404:                        https://ordinacija.vecernji.hr/zdravlje/ohr-savjetnik/ne-hodgkinov-limfom-koji-su-simptomi-i-sto-sve-morate-znati-o-ovoj-teskoj-bolesti/
## 32405:                                                  https://www.vecernji.hr/sport/video-sok-za-psg-u-posljednjih-deset-godina-nije-tako-rano-ispao-iz-kupa-1559697
## 32406:                               https://www.vecernji.hr/kultura/cekamo-novi-oz-i-pitamo-se-sto-je-sljedece-gay-drvosjeca-mozda-dorothy-koja-zeli-biti-don-1559483
##        COMMENT_COUNT
##     1:           573
##     2:             0
##     3:             0
##     4:             0
##     5:            59
##    ---              
## 32402:             0
## 32403:             0
## 32404:             0
## 32405:             0
## 32406:             0
```

- let`s see how changing columns looks look like


```r
# check the date column
str(allDT$DATE)
```

```
##  chr [1:3476130] "2022-01-02" "2022-01-02" "2022-01-02" "2022-01-02" ...
```

```r
# change the date column into date format
allDT[, DateColumn := as.Date(DATE,"%Y-%m-%d" )] # modify by reference
# checNoViewk the new date column 
str(allDT$DateColumn)
```

```
##  Date[1:3476130], format: "2022-01-02" "2022-01-02" "2022-01-02" "2022-01-02" "2022-01-02" ...
```

```r
# show the results
allDT[1:5,.(DateColumn,TITLE,SOURCE_TYPE)][]
```

```
##    DateColumn
## 1: 2022-01-02
## 2: 2022-01-02
## 3: 2022-01-02
## 4: 2022-01-02
## 5: 2022-01-02
##                                                                                                         TITLE
## 1:                                                                                                       <NA>
## 2:                                                                                                       <NA>
## 3:                                                                                                       <NA>
## 4:                                                               EU fit for 55 plan - smanjivanje emisije CO2
## 5: LMS 993-Kit Teler: SMARAGDNA OGRLICA (5) (#19086776) - Aukcije - www.stripovi.com - Prozor u svijet stripa
##    SOURCE_TYPE
## 1:       forum
## 2:       forum
## 3:       forum
## 4:       forum
## 5:         web
```

- combinations of tidy and and DT syntax is also posssible


```r
allDT[SOURCE_TYPE == "facebook",.(AUTHOR,COMMENT_COUNT)] %>%
  filter(COMMENT_COUNT  > 0) %>%
  distinct(.) %>%
  arrange(desc(COMMENT_COUNT)) %>%
  head(15)
```

```
##                    AUTHOR COMMENT_COUNT
##  1:          Teta Violeta         33178
##  2:                Net.hr         10905
##  3:       Violeta We Care          9023
##  4:       Andrea Andrassy          8948
##  5:      Marijana Batinić          8724
##  6:                 Karla          8437
##  7:   Violeta Double Care          8216
##  8:       Andrea Andrassy          8153
##  9:        Violeta Srbija          7787
## 10:                Mustra          7191
## 11:       Andrea Andrassy          7136
## 12:       Andrea Andrassy          6972
## 13:             muzika.hr          6622
## 14: Katarina Mamić Design          6294
## 15:            Elegant.hr          6243
```

- grouping in data table



```r
allDT[,.(AwerageNoArticles = .N), by = SOURCE_TYPE][order(-AwerageNoArticles)]
```

```
##    SOURCE_TYPE AwerageNoArticles
## 1:         web           1443570
## 2:       forum            816445
## 3:    facebook            343927
## 4:     twitter            330532
## 5:      reddit            213029
## 6:     youtube            165681
## 7:     comment             92857
## 8:   instagram             70089
```


## ANALYTICS

- we will cover that in three following lectures
- methods used are statistical analysis, machine learning and textutal analysis

## REPORTING

- the [example](https://raw.githack.com/lusiki/WebObradaPodataka/main/XtraPredavanja/AparatiZaKavu/Prez.html) of (almost) production ready .Rmd report

# CONNCLUDING POINTS

- 



