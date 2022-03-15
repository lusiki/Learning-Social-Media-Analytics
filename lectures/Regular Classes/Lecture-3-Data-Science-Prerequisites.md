---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 3: Data Science Prerequisites"
author: "Luka Sikic, PhD"
date: "Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)" #"15 ožujak 2022"
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
##  $ views       : num 907
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
## [1] 907
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
## $ noComment    <dbl> 0, 106, 106, 7, 19, 133, 133
## $ views        <dbl> 907, 85384, 85384, 2755, 7019, 72696, 72696
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
## [1]   907 85384 85384  2755  7019 72696 72696
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
##   Date: 2022-03-15 15:24
##   Status: 200
##   Content-Type: application/json;charset=utf-8
##   Size: 3.78 MB
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
## $ response.comment_count              <int> 2, 0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ response.keywords                   <list> "i", "i", <"i", "I">, "i", "i", "~
## $ response.pinterest_count            <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ response.reach                      <int> 697, 15, 1576, 1167, 257, 1734, 14~
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
## $ response.like_count                 <int> 24, 0, 6, 0, 0, 0, 1, 0, 0, 0, 0, ~
## $ response.languages                  <list> "hr", "hr", "hr", "hr", "hr", "hr~
## $ response.group_name                 <chr> "Luka", "Luka", "Luka", "Luka", "L~
## $ response.pr_value                   <int> 10, 0, 8, 58, 1, 17, 1, 142, 58, 1~
## $ response.photo                      <chr> "https://mediatoolkit.com/img/0x50~
## $ response.influence_score            <int> 3, 1, 1, 7, 1, 2, 1, 7, 7, 3, 7, 2~
## $ response.url                        <chr> "https://direktno.hr/eu-i-svijet/p~
## $ response.virality                   <dbl> 0.65606254, 0.00000000, 0.70409712~
## $ response.share_count                <int> 6, 0, 12, 0, 2, 13, 2, 0, 0, 0, 0,~
## $ response.linkedin_count             <int> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0~
## $ response.source_reach               <int> 4300, 200, 10, 80000, 10, 800, 90,~
## $ response.domain                     <chr> "direktno.hr", "cazma.hr", "najbol~
## $ response.tag_feed_locations         <list> [], [], [], [], [], [], [], [], [~
## $ response.interaction                <int> 32, 0, 20, 0, 2, 13, 3, 0, 0, 0, 0~
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
  group_by(response.from) %>%
  count %>%
  arrange(desc(n)) %>%
  head()
```

```
## # A tibble: 6 x 2
## # Groups:   response.from [6]
##   response.from     n
##   <chr>         <int>
## 1 <NA>           1140
## 2 croatia         170
## 3 vecernji.hr     151
## 4 novine.hr        53
## 5 index.hr         45
## 6 dnevnik.hr       41
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
## 2     409
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

## ANALYTICS

## REPORTING

# CONNCLUDING POINTS



