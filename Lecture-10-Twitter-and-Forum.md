---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 10: Twitter"
author: "Luka Sikic, PhD <br> Faculty of Croatian Studies | [LSMA](https://lusiki.github.io/Learning-Social-Media-Analytics/)"
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








```r
# read in data
path <- "D:/LUKA/Freelance/Mediatoolkit/FULLtxtDATA"
raw <- list.files(path = path , pattern="xlsx")
raw_path <- paste0(path, "/", raw)
all_raw <- map_df(raw_path, read_excel)
```


```r
# some basic data wrangle
all <- as.data.table(all_raw)
all <- all[,DATE := as.Date(DATE,"%Y-%m-%d")][,DATETIME := anytime(paste(DATE,TIME))]
posts <- all[!duplicated(all),]
rm(all,all_raw)
# select media
forum <- posts[SOURCE_TYPE == "forum",]
forum <- as.data.table(forum)



forum[,.N,TITLE][order(-N)]

forum[TITLE == "Potraga za razlogom ili teorije zavjere vol. 9",][]
```


```r
tw <- read.csv2("C:/Users/Lukas/OneDrive/Desktop/tw.csv")
```




# Basic descriptives of overall activity


```r
# PER INFLUENCER
tw <- tw %>%
  mutate(PROFILE = gsub("^.*\\.com/([^/]+).*", "\\1", URL))
tw <- as.data.table(tw)

# most active profiles
unique(tw[,.N,PROFILE][order(-N)])
```

```
##                PROFILE    N
##     1:      hrtvijesti 1246
##     2:    portaldnevno 1139
##     3: Dalmacija_Danas 1057
##     4:    brokerbarcos  663
##     5:     zagreb_info  647
##    ---                     
## 37478:   lydia_penguin    1
## 37479: HywelPlaidCymru    1
## 37480:   Mucenistudent    1
## 37481:           p4dme    1
## 37482:       Demostat1    1
```

```r
# most popular
tw %>% 
  group_by(PROFILE) %>%
  summarise(FOLLOW = mean(FOLLOWERS_COUNT)) %>%
  arrange(desc(FOLLOW)) %>% 
  head(40) 
```

```
## # A tibble: 40 x 2
##    PROFILE           FOLLOW
##    <chr>              <dbl>
##  1 khloekardashian 29973623
##  2 pabllovittar     3072344
##  3 KidCudi          2817839
##  4 Nasty_CSA        2010115
##  5 erigganewmoney   1822182
##  6 phamswing        1559883
##  7 PoemPorns        1323818
##  8 MarioMandzukic9  1037953
##  9 hulu             1027891
## 10 estopaoficial    1011572
## # ... with 30 more rows
```

```r
# most influential
tw %>% 
  group_by(PROFILE) %>%
  summarise(REACH = sum(REACH)) %>%
  arrange(desc(REACH))  %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE           REACH
##    <chr>             <int>
##  1 DNEVNIKhr       7391289
##  2 24sata_HR       5851797
##  3 vecernji_list   4009695
##  4 VladaRH         2751524
##  5 hrtvijesti      2344931
##  6 dreamjeons      1687520
##  7 khloekardashian 1511558
##  8 triviamor        994551
##  9 taeteland        965578
## 10 sincerelyart     891840
## # ... with 30 more rows
```

```r
# most influential II
tw %>% 
  group_by(PROFILE) %>%
  summarise(INTERACTIONS = sum(INTERACTIONS)) %>%
  arrange(desc(INTERACTIONS)) %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         INTERACTIONS
##    <chr>                  <int>
##  1 dreamjeons            126368
##  2 sincerelyart           75775
##  3 taeteland              72793
##  4 triviamor              71201
##  5 hh_jmxx                63520
##  6 maybeeevirgo           49328
##  7 rainemescudi           37138
##  8 pabllovittar           36018
##  9 Skrille75951826        33486
## 10 convomf                30323
## # ... with 30 more rows
```

```r
# most appreciated
tw %>% 
  group_by(PROFILE) %>%
  summarise(FAVORITE = sum(FAVORITE_COUNT)) %>%
  arrange(desc(FAVORITE)) %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         FAVORITE
##    <chr>              <int>
##  1 dreamjeons        106071
##  2 sincerelyart       69363
##  3 taeteland          62958
##  4 triviamor          57078
##  5 hh_jmxx            54063
##  6 maybeeevirgo       37475
##  7 rainemescudi       35518
##  8 pabllovittar       33209
##  9 Skrille75951826    28701
## 10 convomf            27974
## # ... with 30 more rows
```

```r
# most appreciated
tw %>% 
  group_by(PROFILE) %>%
  summarise(RETWEET = sum(RETWEET_COUNT)) %>%
  arrange(desc(RETWEET)) %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         RETWEET
##    <chr>             <int>
##  1 dreamjeons        20297
##  2 triviamor         14123
##  3 maybeeevirgo      11853
##  4 taeteland          9835
##  5 hh_jmxx            9457
##  6 sincerelyart       6412
##  7 Skrille75951826    4785
##  8 HRJVISUAL          4121
##  9 stefondiggs        3457
## 10 pabllovittar       2809
## # ... with 30 more rows
```


```r
# PER TWEET

# most popular
tw %>% 
  select(PROFILE, FULL_TEXT, FOLLOWERS_COUNT,URL) %>%
  arrange(desc(FOLLOWERS_COUNT)) %>% 
  head(40)
```

```
##             PROFILE
##  1: khloekardashian
##  2:    pabllovittar
##  3:         KidCudi
##  4:       Nasty_CSA
##  5:  erigganewmoney
##  6:       phamswing
##  7:       PoemPorns
##  8: MarioMandzukic9
##  9:            hulu
## 10:   estopaoficial
## 11: AKOSiLOLANiDORA
## 12:         DeJLoaf
## 13:         convomf
## 14:       NoahUrrea
## 15:    shxx131bi131
## 16:       taeteland
## 17:         convomf
## 18:    emmalangevin
## 19:         convomf
## 20:         convomf
## 21:         convomf
## 22:         convomf
## 23:         convomf
## 24:        88rising
## 25:     SeniormanOA
## 26:      memesiwish
## 27:     dariofrance
## 28:     stefondiggs
## 29:     stefondiggs
## 30:     ayunda_risu
## 31:      shu_yamino
## 32:    archiveforJK
## 33:        InuKishu
## 34:       ren_i_bot
## 35:        convomfs
## 36:  luca_kaneshiro
## 37:    Martyguptill
## 38:        convomfs
## 39:      FOCALISTIC
## 40:     miiniyoongs
##             PROFILE
##                                                                                                                                                                                                                           FULL_TEXT
##  1:                                                                                                                                                                                                 I love my mommy #TheKardashians
##  2:                                                                                                                                                    . @MarinaDiamandis I LOVE U <U+0001F91F><U+0001F3FD> https://t.co/sIUoENHxBH
##  3:                                                                                                                              <U+0001F4A8><U+0001F4A8><U+0001F4A8> @jackblack i love u dude haha - Scott https://t.co/u98IKfjbQC
##  4:                                                                                                                                                           OUT NOW! #CantImagine https://t.co/xMxHqIa5Ws https://t.co/5xGDUKKVvU
##  5:                                                                                                                         Abi make I snap our village juju picture take do NFTs? <U+0001F60F><U+0001F60F><U+0001F60F><U+0001F60F>
##  6:                                                                                                                                                                                                            i love japanese food
##  7:                                                                                                                                                                                            i am worthy. https://t.co/36OGsZAl55
##  8:                   Nova uloga, ali isti osjećaj zadovoljstva i ponosa što sam dio hrvatske reprezentacije <U+0001F1ED><U+0001F1F7><U+2764><U+FE0F><U+200D><U+0001F525>#obitelj #family #mm17<U+0001F32A> https://t.co/4kWcAN85D5
##  9:                                                                                                                                                                                                      @pizzadinosaur I just know
## 10:                                                                                                                                                                                    I love Zarza Capilla https://t.co/yEizAOQa9A
## 11:                                                                                                                                                                                                I BELIEVE AIKA ROBREDO SUPREMACY
## 12:                                                                                                                                                                                                         I love you <U+0001F90D>
## 13:                                                                                                                                                                                                 <U+0001F499> hi R, i love you..
## 14:                                                                                                                                                                                              I love you https://t.co/pzvV61vRbB
## 15:                                                                                                                                                                                     @jilju_131 I know you can do it<U+0001F609>
## 16:                                                                                                                                                                              baby i love u sm ¦<U+FE0E> https://t.co/4dACNpWOrk
## 17:                                                                                                                                                                                         kak j, i have crush on you <U+0001F499>
## 18:                                                                                                                                                                                                                   i love my dog
## 19:                                                                                                                                                                                                     <U+0001F499> i love you, A.
## 20:                                                                                                                                                                                                       <U+0001F499> i love you J
## 21:                                                                                                                                                                       <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
## 22:                                                                                                                                            <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
## 23:                                                                                                                                                                                    kak @/tkwpcnfak i hv crush on u <U+0001F499>
## 24:                                                                                                                                                                               RT @nakedbibi i look cute https://t.co/bjrJw0Vcwg
## 25:                                                                                                                                                       I go make am <U+0001F64F><U+0001F3FD><U+0001F602> https://t.co/1nFoL4ZqMn
## 26:                                                                                                                                                                                       am i a joke to u? https://t.co/OHoESm3Rgb
## 27: RT @NObuljen Uoči ceremonije dodjele nagrada na @la_Biennale s <U+0001F1EE><U+0001F1F9> ministrom kulture @dariofrance potpisala sam četverogodišnji program suradnje u području kulture i obrazovanja. https://t.co/q11QG32hxW
## 28:                                                                                                                                                                                                                 God I love you…
## 29:                                                                                                                                                                                                                 God I love you…
## 30:                                                                                                                                                       @kobokanaeru I like durian ice cream <U+0001F368><U+0001F368><U+0001F368>
## 31:                                                                                                                                                                                                        @luca_kaneshiro i pogged
## 32:                                                                                                                                     <U+0001F430> i ABSOLUTELY love banana milk… YEAHHH~ \n\nHES SO CUTE https://t.co/bdrsmHbqZw
## 33:                                                                                                                                                                                      #KISHU I¦<U+FE0F>U https://t.co/YxY7UbonZh
## 34:                                                                                                                                                                                                                     I love you.
## 35:                                                                                                                                                                                                    am i a problem? <U+0001F4AD>
## 36:                                                                                                                                                                                     @NIJISANJI_World I LOOK PRETTY <U+0001F62E>
## 37:                                    @blu_dymnd3 @chadpreetTC @CryptoBitTako @Connnjy @HassleSauce @evanluza @gabz_ph @villain_supa @bratan1992 @CryptocrooksNFT @dj_nini @yuya_dev @niemann_jana @sirikademi @ZnowiiX @Am_I_Nutz
## 38:                                                                                                                                                                                        i dare u pap suhu di dom lo <U+0001F90D>
## 39:                                                                                                                             RT @sjozii I Know i cnt dance but aslong go nale @FOCALISTIC  ra lwela dawg https://t.co/XvkULcwAu6
## 40:                                                                                                    [220331 Jungkook Instagram Story]\n\n<U+0001F49C> i like you vs i love you\n<U+0001F430> i love you~ https://t.co/tqRDbEyL5I
##                                                                                                                                                                                                                           FULL_TEXT
##     FOLLOWERS_COUNT
##  1:        29973623
##  2:         3072344
##  3:         2817839
##  4:         2010115
##  5:         1822182
##  6:         1559883
##  7:         1323818
##  8:         1037953
##  9:         1027891
## 10:         1011572
## 11:          839435
## 12:          838230
## 13:          821595
## 14:          820641
## 15:          816967
## 16:          816541
## 17:          814402
## 18:          786768
## 19:          759869
## 20:          754953
## 21:          754898
## 22:          752605
## 23:          731867
## 24:          625342
## 25:          498437
## 26:          476348
## 27:          472263
## 28:          457913
## 29:          457913
## 30:          451549
## 31:          437003
## 32:          436034
## 33:          429578
## 34:          425272
## 35:          419549
## 36:          409765
## 37:          406867
## 38:          384108
## 39:          383439
## 40:          377268
##     FOLLOWERS_COUNT
##                                                                URL
##  1: https://twitter.com/khloekardashian/status/1516999050465136640
##  2:    https://twitter.com/pabllovittar/status/1507137433317556225
##  3:         https://twitter.com/KidCudi/status/1510820591481552897
##  4:       https://twitter.com/Nasty_CSA/status/1507624674905178120
##  5:  https://twitter.com/erigganewmoney/status/1514392645140942849
##  6:       https://twitter.com/phamswing/status/1506599988997939203
##  7:       https://twitter.com/PoemPorns/status/1510663322810548227
##  8: https://twitter.com/MarioMandzukic9/status/1507315207433076738
##  9:            https://twitter.com/hulu/status/1518355788741316608
## 10:   https://twitter.com/estopaoficial/status/1515306715746557954
## 11: https://twitter.com/AKOSiLOLANiDORA/status/1513768705385107460
## 12:         https://twitter.com/DeJLoaf/status/1514802362858172416
## 13:         https://twitter.com/convomf/status/1515595937879830531
## 14:       https://twitter.com/NoahUrrea/status/1510327745510723585
## 15:    https://twitter.com/shxx131bi131/status/1514848188347600899
## 16:       https://twitter.com/taeteland/status/1515707621487128595
## 17:         https://twitter.com/convomf/status/1514536446387884035
## 18:    https://twitter.com/emmalangevin/status/1511919871243669510
## 19:         https://twitter.com/convomf/status/1505844167775518721
## 20:         https://twitter.com/convomf/status/1506674654370738180
## 21:         https://twitter.com/convomf/status/1506662084830179330
## 22:         https://twitter.com/convomf/status/1506464608478851073
## 23:         https://twitter.com/convomf/status/1503918126529843200
## 24:        https://twitter.com/88rising/status/1520660183696830464
## 25:     https://twitter.com/SeniormanOA/status/1507341849765335063
## 26:      https://twitter.com/memesiwish/status/1514925668043161602
## 27:     https://twitter.com/dariofrance/status/1520127562906836998
## 28:     https://twitter.com/stefondiggs/status/1516204214531198976
## 29:     https://twitter.com/stefondiggs/status/1516204214531198976
## 30:     https://twitter.com/ayunda_risu/status/1507264312389545987
## 31:      https://twitter.com/shu_yamino/status/1515619320436514820
## 32:    https://twitter.com/archiveforJK/status/1510265656687616009
## 33:        https://twitter.com/InuKishu/status/1514956650758746112
## 34:       https://twitter.com/ren_i_bot/status/1517204091851616256
## 35:        https://twitter.com/convomfs/status/1519718149075398656
## 36:  https://twitter.com/luca_kaneshiro/status/1512686070890385408
## 37:    https://twitter.com/Martyguptill/status/1516021934798032902
## 38:        https://twitter.com/convomfs/status/1510124872453672960
## 39:      https://twitter.com/FOCALISTIC/status/1507611048798527491
## 40:     https://twitter.com/miiniyoongs/status/1509711652752662532
##                                                                URL
```

```r
# most influential
tw %>% 
  select(PROFILE, FULL_TEXT, REACH,URL) %>%
  arrange(desc(REACH))  %>% 
  head(40)
```

```
##             PROFILE
##  1:      dreamjeons
##  2: khloekardashian
##  3:       triviamor
##  4:       taeteland
##  5:    sincerelyart
##  6:         hh_jmxx
##  7:    maybeeevirgo
##  8:    pabllovittar
##  9:    rainemescudi
## 10:         CG_wwdd
## 11: Skrille75951826
## 12:       HRJVISUAL
## 13:  luca_kaneshiro
## 14:         convomf
## 15:     stefondiggs
## 16:     stefondiggs
## 17:      shu_yamino
## 18:       bookpoets
## 19:     omomomo_ttv
## 20:         KidCudi
## 21:  erigganewmoney
## 22: Skrille75951826
## 23:         convomf
## 24:      ReimuEndou
## 25:   DonkeyKongApe
## 26:       jpegmafia
## 27:    emmalangevin
## 28:       NoahUrrea
## 29:       Nasty_CSA
## 30:    BAIZEJINTIAN
## 31:     Adore_Domoo
## 32:  CherrySnakeCat
## 33:     lisasuprmcy
## 34: madeoflavenderr
## 35:       waifuumia
## 36:    MusicPop_art
## 37:     FinanaRyugu
## 38:    chaotictw1nk
## 39:      memesiwish
## 40:      imjaebooms
##             PROFILE
##                                                                                                                                                    FULL_TEXT
##  1: <U+0001F464>"are you jeon jungkook?"\n<U+0001F430>"and, if i am?"\n\n<U+0001F464>"i lov…lov… i like you”\n<U+0001F430>“why can’t you say 'i love you' !”
##  2:                                                                                                                          I love my mommy #TheKardashians
##  3:                                                                                                i love youu i love you i love you https://t.co/LnpsKXP6kW
##  4:                                                                                                       baby i love u sm ¦<U+FE0E> https://t.co/4dACNpWOrk
##  5:                                                                                      i make THE CUTEST rugs no joke <U+0001F629> https://t.co/XRWvzyq0DD
##  6:                                                                                                idc i stalk people i got no shame<U+0001F62D><U+0001F62D>
##  7:                                                                                                                                “u look tired” oh bc i am
##  8:                                                                             . @MarinaDiamandis I LOVE U <U+0001F91F><U+0001F3FD> https://t.co/sIUoENHxBH
##  9:                                                                                                                      I WIN I WIN https://t.co/Uy7ubqKZs9
## 10:                                                                                                         I love you\n#dreamfanart https://t.co/pT1GGVXUMD
## 11:                                                                                                           I-NO #GuiltyGearStrive https://t.co/SGZ3brcm4R
## 12:                                                  renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
## 13:                                                                                                              @NIJISANJI_World I LOOK PRETTY <U+0001F62E>
## 14:                                                                                                <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
## 15:                                                                                                                                          God I love you…
## 16:                                                                                                                                          God I love you…
## 17:                                                                                                                                 @luca_kaneshiro i pogged
## 18:                                                                                                                        i woke up https://t.co/xs3zRngpjJ
## 19:                                                                                                                        I am weak https://t.co/sYG4CBcdLn
## 20:                                                       <U+0001F4A8><U+0001F4A8><U+0001F4A8> @jackblack i love u dude haha - Scott https://t.co/u98IKfjbQC
## 21:                                                  Abi make I snap our village juju picture take do NFTs? <U+0001F60F><U+0001F60F><U+0001F60F><U+0001F60F>
## 22:                                                                                                           I-NO #GuiltyGearStrive https://t.co/siYo2uDZus
## 23:                                                                     <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
## 24:                                                                                                                  Godamn I love Niji EN\nBye Im emotional
## 25:                                                                                                                                               i love you
## 26:                                                                                                                                      i love black people
## 27:                                                                                                                                            i love my dog
## 28:                                                                                                                       I love you https://t.co/pzvV61vRbB
## 29:                                                                                    OUT NOW! #CantImagine https://t.co/xMxHqIa5Ws https://t.co/5xGDUKKVvU
## 30:                                                               #Akurylic #ArtoftheRogue\n"I love you"<U+2764><U+FE0F><U+0001F49C> https://t.co/RmuqVV4YQW
## 31:                                                                                                                    I love sleeping a$$ naked<U+0001F975>
## 32:                                                                                                                   How do I Look? https://t.co/yM66ZAPKKD
## 33:                                                                                                           basta i love u both :< https://t.co/u0KPlzwhSH
## 34:                                                                                                                  i love you lizz https://t.co/sbXNd9QejH
## 35:                                                                                                   do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
## 36:                                                                 I like purple people <U+0001F974><U+0001F49C><U+2728>\n\n#purple https://t.co/qna3WT9bMX
## 37:                                                                                                                                               man i suck
## 38:                                                                                                                                       god i love gay sex
## 39:                                                                                                                am i a joke to u? https://t.co/OHoESm3Rgb
## 40:                                                                                                                 GOT7 I love you. https://t.co/fxF2pFp1OZ
##                                                                                                                                                    FULL_TEXT
##       REACH                                                            URL
##  1: 1619067      https://twitter.com/dreamjeons/status/1504752941693124608
##  2: 1511558 https://twitter.com/khloekardashian/status/1516999050465136640
##  3:  994515       https://twitter.com/triviamor/status/1512688247956156417
##  4:  965578       https://twitter.com/taeteland/status/1515707621487128595
##  5:  891840    https://twitter.com/sincerelyart/status/1507053925102280719
##  6:  824858         https://twitter.com/hh_jmxx/status/1517495381327859717
##  7:  744293    https://twitter.com/maybeeevirgo/status/1504597668366008320
##  8:  570271    https://twitter.com/pabllovittar/status/1507137433317556225
##  9:  403794    https://twitter.com/rainemescudi/status/1515714247468236801
## 10:  274918         https://twitter.com/CG_wwdd/status/1504728327843573760
## 11:  262219 https://twitter.com/Skrille75951826/status/1508438202679689217
## 12:  256021       https://twitter.com/HRJVISUAL/status/1510188863175634944
## 13:  211495  https://twitter.com/luca_kaneshiro/status/1512686070890385408
## 14:  210848         https://twitter.com/convomf/status/1506662084830179330
## 15:  199120     https://twitter.com/stefondiggs/status/1516204214531198976
## 16:  196508     https://twitter.com/stefondiggs/status/1516204214531198976
## 17:  195015      https://twitter.com/shu_yamino/status/1515619320436514820
## 18:  178025       https://twitter.com/bookpoets/status/1520333216489476097
## 19:  176651     https://twitter.com/omomomo_ttv/status/1506288545908109320
## 20:  172804         https://twitter.com/KidCudi/status/1510820591481552897
## 21:  168871  https://twitter.com/erigganewmoney/status/1514392645140942849
## 22:  168493 https://twitter.com/Skrille75951826/status/1506606592661532674
## 23:  166730         https://twitter.com/convomf/status/1506464608478851073
## 24:  157207      https://twitter.com/ReimuEndou/status/1509367618666319873
## 25:  155747   https://twitter.com/DonkeyKongApe/status/1509529228299055111
## 26:  148201       https://twitter.com/jpegmafia/status/1505796494607085575
## 27:  146372    https://twitter.com/emmalangevin/status/1511919871243669510
## 28:  138705       https://twitter.com/NoahUrrea/status/1510327745510723585
## 29:  135471       https://twitter.com/Nasty_CSA/status/1507624674905178120
## 30:  130534    https://twitter.com/BAIZEJINTIAN/status/1518973411308892160
## 31:  129670     https://twitter.com/Adore_Domoo/status/1514087116883566593
## 32:  126642  https://twitter.com/CherrySnakeCat/status/1513968270902185985
## 33:  125758     https://twitter.com/lisasuprmcy/status/1509070998074667008
## 34:  125233 https://twitter.com/madeoflavenderr/status/1506377255550861312
## 35:  124779       https://twitter.com/waifuumia/status/1512555707677265922
## 36:  119813    https://twitter.com/MusicPop_art/status/1509142678788878342
## 37:  118574     https://twitter.com/FinanaRyugu/status/1506467423423279104
## 38:  117086    https://twitter.com/chaotictw1nk/status/1517782558934712321
## 39:  113315      https://twitter.com/memesiwish/status/1514925668043161602
## 40:  112197      https://twitter.com/imjaebooms/status/1517732565465313280
##       REACH                                                            URL
```

```r
# most influential II
tw %>% 
  select(PROFILE, FULL_TEXT, INTERACTIONS,URL) %>%
  arrange(desc(INTERACTIONS))  %>% 
  head(40)
```

```
##             PROFILE
##  1:      dreamjeons
##  2:    sincerelyart
##  3:       taeteland
##  4:       triviamor
##  5:         hh_jmxx
##  6:    maybeeevirgo
##  7:    rainemescudi
##  8:    pabllovittar
##  9:         CG_wwdd
## 10: Skrille75951826
## 11:       HRJVISUAL
## 12:  luca_kaneshiro
## 13:         convomf
## 14:      shu_yamino
## 15:     stefondiggs
## 16:     stefondiggs
## 17:     omomomo_ttv
## 18:      ReimuEndou
## 19: Skrille75951826
## 20:   DonkeyKongApe
## 21:       jpegmafia
## 22:       bookpoets
## 23:         convomf
## 24:    BAIZEJINTIAN
## 25:       waifuumia
## 26:    emmalangevin
## 27:     FinanaRyugu
## 28:  CherrySnakeCat
## 29:    chaotictw1nk
## 30: madeoflavenderr
## 31:    MusicPop_art
## 32:     lisasuprmcy
## 33:       NoahUrrea
## 34:     Adore_Domoo
## 35:         woIfevr
## 36:   MpiloKhumalo_
## 37:    crying2frank
## 38:       kaohom503
## 39:      memesiwish
## 40:   CantDieBroke2
##             PROFILE
##                                                                                                                                                    FULL_TEXT
##  1: <U+0001F464>"are you jeon jungkook?"\n<U+0001F430>"and, if i am?"\n\n<U+0001F464>"i lov…lov… i like you”\n<U+0001F430>“why can’t you say 'i love you' !”
##  2:                                                                                      i make THE CUTEST rugs no joke <U+0001F629> https://t.co/XRWvzyq0DD
##  3:                                                                                                       baby i love u sm ¦<U+FE0E> https://t.co/4dACNpWOrk
##  4:                                                                                                i love youu i love you i love you https://t.co/LnpsKXP6kW
##  5:                                                                                                idc i stalk people i got no shame<U+0001F62D><U+0001F62D>
##  6:                                                                                                                                “u look tired” oh bc i am
##  7:                                                                                                                      I WIN I WIN https://t.co/Uy7ubqKZs9
##  8:                                                                             . @MarinaDiamandis I LOVE U <U+0001F91F><U+0001F3FD> https://t.co/sIUoENHxBH
##  9:                                                                                                         I love you\n#dreamfanart https://t.co/pT1GGVXUMD
## 10:                                                                                                           I-NO #GuiltyGearStrive https://t.co/SGZ3brcm4R
## 11:                                                  renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
## 12:                                                                                                              @NIJISANJI_World I LOOK PRETTY <U+0001F62E>
## 13:                                                                                                <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
## 14:                                                                                                                                 @luca_kaneshiro i pogged
## 15:                                                                                                                                          God I love you…
## 16:                                                                                                                                          God I love you…
## 17:                                                                                                                        I am weak https://t.co/sYG4CBcdLn
## 18:                                                                                                                  Godamn I love Niji EN\nBye Im emotional
## 19:                                                                                                           I-NO #GuiltyGearStrive https://t.co/siYo2uDZus
## 20:                                                                                                                                               i love you
## 21:                                                                                                                                      i love black people
## 22:                                                                                                                        i woke up https://t.co/xs3zRngpjJ
## 23:                                                                     <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
## 24:                                                               #Akurylic #ArtoftheRogue\n"I love you"<U+2764><U+FE0F><U+0001F49C> https://t.co/RmuqVV4YQW
## 25:                                                                                                   do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
## 26:                                                                                                                                            i love my dog
## 27:                                                                                                                                               man i suck
## 28:                                                                                                                   How do I Look? https://t.co/yM66ZAPKKD
## 29:                                                                                                                                       god i love gay sex
## 30:                                                                                                                  i love you lizz https://t.co/sbXNd9QejH
## 31:                                                                 I like purple people <U+0001F974><U+0001F49C><U+2728>\n\n#purple https://t.co/qna3WT9bMX
## 32:                                                                                                           basta i love u both :< https://t.co/u0KPlzwhSH
## 33:                                                                                                                       I love you https://t.co/pzvV61vRbB
## 34:                                                                                                                    I love sleeping a$$ naked<U+0001F975>
## 35:                                                                                                                mia goth i love u https://t.co/d4K4IdBS3d
## 36:                                                                                                  I got a job<U+0001F97A><U+2764><U+0001F64F><U+0001F3FE>
## 37:                                                                                                                                       i love frank ocean
## 38:                                                   #YaminoArt I love Ib game <U+0001F979><U+0001F49B><U+2764><U+FE0F><U+0001F49C> https://t.co/M88gkccmQQ
## 39:                                                                                                                am i a joke to u? https://t.co/OHoESm3Rgb
## 40:                                                    @yikobaz @kirawontmiss THINK I FOUND HIM <U+0001F62D><U+0001F923><U+0001F525> https://t.co/OwHudN33dv
##                                                                                                                                                    FULL_TEXT
##     INTERACTIONS                                                            URL
##  1:       122064      https://twitter.com/dreamjeons/status/1504752941693124608
##  2:        75775    https://twitter.com/sincerelyart/status/1507053925102280719
##  3:        72793       https://twitter.com/taeteland/status/1515707621487128595
##  4:        71201       https://twitter.com/triviamor/status/1512688247956156417
##  5:        63520         https://twitter.com/hh_jmxx/status/1517495381327859717
##  6:        49328    https://twitter.com/maybeeevirgo/status/1504597668366008320
##  7:        37138    https://twitter.com/rainemescudi/status/1515714247468236801
##  8:        36018    https://twitter.com/pabllovittar/status/1507137433317556225
##  9:        24281         https://twitter.com/CG_wwdd/status/1504728327843573760
## 10:        20278 https://twitter.com/Skrille75951826/status/1508438202679689217
## 11:        17177       https://twitter.com/HRJVISUAL/status/1510188863175634944
## 12:        17037  https://twitter.com/luca_kaneshiro/status/1512686070890385408
## 13:        15821         https://twitter.com/convomf/status/1506662084830179330
## 14:        15772      https://twitter.com/shu_yamino/status/1515619320436514820
## 15:        14130     https://twitter.com/stefondiggs/status/1516204214531198976
## 16:        13932     https://twitter.com/stefondiggs/status/1516204214531198976
## 17:        13454     https://twitter.com/omomomo_ttv/status/1506288545908109320
## 18:        13221      https://twitter.com/ReimuEndou/status/1509367618666319873
## 19:        13208 https://twitter.com/Skrille75951826/status/1506606592661532674
## 20:        12119   https://twitter.com/DonkeyKongApe/status/1509529228299055111
## 21:        11516       https://twitter.com/jpegmafia/status/1505796494607085575
## 22:        11386       https://twitter.com/bookpoets/status/1520333216489476097
## 23:        11324         https://twitter.com/convomf/status/1506464608478851073
## 24:        10932    https://twitter.com/BAIZEJINTIAN/status/1518973411308892160
## 25:        10750       https://twitter.com/waifuumia/status/1512555707677265922
## 26:        10379    https://twitter.com/emmalangevin/status/1511919871243669510
## 27:        10289     https://twitter.com/FinanaRyugu/status/1506467423423279104
## 28:        10060  https://twitter.com/CherrySnakeCat/status/1513968270902185985
## 29:         9573    https://twitter.com/chaotictw1nk/status/1517782558934712321
## 30:         9496 https://twitter.com/madeoflavenderr/status/1506377255550861312
## 31:         9197    https://twitter.com/MusicPop_art/status/1509142678788878342
## 32:         9133     https://twitter.com/lisasuprmcy/status/1509070998074667008
## 33:         8869       https://twitter.com/NoahUrrea/status/1510327745510723585
## 34:         8586     https://twitter.com/Adore_Domoo/status/1514087116883566593
## 35:         8498         https://twitter.com/woIfevr/status/1515669028034867200
## 36:         8380   https://twitter.com/MpiloKhumalo_/status/1519647346270867456
## 37:         7578    https://twitter.com/crying2frank/status/1510810844526829568
## 38:         7461       https://twitter.com/kaohom503/status/1513866464356298757
## 39:         7390      https://twitter.com/memesiwish/status/1514925668043161602
## 40:         7318   https://twitter.com/CantDieBroke2/status/1505696878251421696
##     INTERACTIONS                                                            URL
```

```r
# most appreciated
tw %>% 
  select(PROFILE, FULL_TEXT, FAVORITE_COUNT,URL) %>%
  arrange(desc(FAVORITE_COUNT)) %>% 
  head(40)
```

```
##             PROFILE
##  1:      dreamjeons
##  2:    sincerelyart
##  3:       taeteland
##  4:       triviamor
##  5:         hh_jmxx
##  6:    maybeeevirgo
##  7:    rainemescudi
##  8:    pabllovittar
##  9:         CG_wwdd
## 10: Skrille75951826
## 11:  luca_kaneshiro
## 12:         convomf
## 13:      shu_yamino
## 14:       HRJVISUAL
## 15:      ReimuEndou
## 16:     stefondiggs
## 17:     stefondiggs
## 18: Skrille75951826
## 19:     omomomo_ttv
## 20:   DonkeyKongApe
## 21:         convomf
## 22:       jpegmafia
## 23:       waifuumia
## 24:    emmalangevin
## 25:     FinanaRyugu
## 26:    BAIZEJINTIAN
## 27:       bookpoets
## 28:  CherrySnakeCat
## 29:    chaotictw1nk
## 30:       NoahUrrea
## 31:   MpiloKhumalo_
## 32: madeoflavenderr
## 33:    MusicPop_art
## 34:         woIfevr
## 35:     lisasuprmcy
## 36:   CantDieBroke2
## 37:      memesiwish
## 38:       kaohom503
## 39:     Adore_Domoo
## 40:      fetishxsel
##             PROFILE
##                                                                                                                                                    FULL_TEXT
##  1: <U+0001F464>"are you jeon jungkook?"\n<U+0001F430>"and, if i am?"\n\n<U+0001F464>"i lov…lov… i like you”\n<U+0001F430>“why can’t you say 'i love you' !”
##  2:                                                                                      i make THE CUTEST rugs no joke <U+0001F629> https://t.co/XRWvzyq0DD
##  3:                                                                                                       baby i love u sm ¦<U+FE0E> https://t.co/4dACNpWOrk
##  4:                                                                                                i love youu i love you i love you https://t.co/LnpsKXP6kW
##  5:                                                                                                idc i stalk people i got no shame<U+0001F62D><U+0001F62D>
##  6:                                                                                                                                “u look tired” oh bc i am
##  7:                                                                                                                      I WIN I WIN https://t.co/Uy7ubqKZs9
##  8:                                                                             . @MarinaDiamandis I LOVE U <U+0001F91F><U+0001F3FD> https://t.co/sIUoENHxBH
##  9:                                                                                                         I love you\n#dreamfanart https://t.co/pT1GGVXUMD
## 10:                                                                                                           I-NO #GuiltyGearStrive https://t.co/SGZ3brcm4R
## 11:                                                                                                              @NIJISANJI_World I LOOK PRETTY <U+0001F62E>
## 12:                                                                                                <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
## 13:                                                                                                                                 @luca_kaneshiro i pogged
## 14:                                                  renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
## 15:                                                                                                                  Godamn I love Niji EN\nBye Im emotional
## 16:                                                                                                                                          God I love you…
## 17:                                                                                                                                          God I love you…
## 18:                                                                                                           I-NO #GuiltyGearStrive https://t.co/siYo2uDZus
## 19:                                                                                                                        I am weak https://t.co/sYG4CBcdLn
## 20:                                                                                                                                               i love you
## 21:                                                                     <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
## 22:                                                                                                                                      i love black people
## 23:                                                                                                   do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
## 24:                                                                                                                                            i love my dog
## 25:                                                                                                                                               man i suck
## 26:                                                               #Akurylic #ArtoftheRogue\n"I love you"<U+2764><U+FE0F><U+0001F49C> https://t.co/RmuqVV4YQW
## 27:                                                                                                                        i woke up https://t.co/xs3zRngpjJ
## 28:                                                                                                                   How do I Look? https://t.co/yM66ZAPKKD
## 29:                                                                                                                                       god i love gay sex
## 30:                                                                                                                       I love you https://t.co/pzvV61vRbB
## 31:                                                                                                  I got a job<U+0001F97A><U+2764><U+0001F64F><U+0001F3FE>
## 32:                                                                                                                  i love you lizz https://t.co/sbXNd9QejH
## 33:                                                                 I like purple people <U+0001F974><U+0001F49C><U+2728>\n\n#purple https://t.co/qna3WT9bMX
## 34:                                                                                                                mia goth i love u https://t.co/d4K4IdBS3d
## 35:                                                                                                           basta i love u both :< https://t.co/u0KPlzwhSH
## 36:                                                    @yikobaz @kirawontmiss THINK I FOUND HIM <U+0001F62D><U+0001F923><U+0001F525> https://t.co/OwHudN33dv
## 37:                                                                                                                am i a joke to u? https://t.co/OHoESm3Rgb
## 38:                                                   #YaminoArt I love Ib game <U+0001F979><U+0001F49B><U+2764><U+FE0F><U+0001F49C> https://t.co/M88gkccmQQ
## 39:                                                                                                                    I love sleeping a$$ naked<U+0001F975>
## 40:                                                                                                      I love Selena Gomez's smile https://t.co/O9vvcyA7MJ
##                                                                                                                                                    FULL_TEXT
##     FAVORITE_COUNT
##  1:         102590
##  2:          69363
##  3:          62958
##  4:          57078
##  5:          54063
##  6:          37475
##  7:          35518
##  8:          33209
##  9:          22685
## 10:          17311
## 11:          16017
## 12:          15087
## 13:          15019
## 14:          13056
## 15:          12441
## 16:          12386
## 17:          12219
## 18:          11390
## 19:          11350
## 20:          10623
## 21:          10544
## 22:          10462
## 23:          10318
## 24:          10219
## 25:          10098
## 26:           9884
## 27:           9039
## 28:           8785
## 29:           8554
## 30:           8423
## 31:           8147
## 32:           8062
## 33:           7807
## 34:           7560
## 35:           7442
## 36:           6848
## 37:           6658
## 38:           6498
## 39:           6401
## 40:           6196
##     FAVORITE_COUNT
##                                                                URL
##  1:      https://twitter.com/dreamjeons/status/1504752941693124608
##  2:    https://twitter.com/sincerelyart/status/1507053925102280719
##  3:       https://twitter.com/taeteland/status/1515707621487128595
##  4:       https://twitter.com/triviamor/status/1512688247956156417
##  5:         https://twitter.com/hh_jmxx/status/1517495381327859717
##  6:    https://twitter.com/maybeeevirgo/status/1504597668366008320
##  7:    https://twitter.com/rainemescudi/status/1515714247468236801
##  8:    https://twitter.com/pabllovittar/status/1507137433317556225
##  9:         https://twitter.com/CG_wwdd/status/1504728327843573760
## 10: https://twitter.com/Skrille75951826/status/1508438202679689217
## 11:  https://twitter.com/luca_kaneshiro/status/1512686070890385408
## 12:         https://twitter.com/convomf/status/1506662084830179330
## 13:      https://twitter.com/shu_yamino/status/1515619320436514820
## 14:       https://twitter.com/HRJVISUAL/status/1510188863175634944
## 15:      https://twitter.com/ReimuEndou/status/1509367618666319873
## 16:     https://twitter.com/stefondiggs/status/1516204214531198976
## 17:     https://twitter.com/stefondiggs/status/1516204214531198976
## 18: https://twitter.com/Skrille75951826/status/1506606592661532674
## 19:     https://twitter.com/omomomo_ttv/status/1506288545908109320
## 20:   https://twitter.com/DonkeyKongApe/status/1509529228299055111
## 21:         https://twitter.com/convomf/status/1506464608478851073
## 22:       https://twitter.com/jpegmafia/status/1505796494607085575
## 23:       https://twitter.com/waifuumia/status/1512555707677265922
## 24:    https://twitter.com/emmalangevin/status/1511919871243669510
## 25:     https://twitter.com/FinanaRyugu/status/1506467423423279104
## 26:    https://twitter.com/BAIZEJINTIAN/status/1518973411308892160
## 27:       https://twitter.com/bookpoets/status/1520333216489476097
## 28:  https://twitter.com/CherrySnakeCat/status/1513968270902185985
## 29:    https://twitter.com/chaotictw1nk/status/1517782558934712321
## 30:       https://twitter.com/NoahUrrea/status/1510327745510723585
## 31:   https://twitter.com/MpiloKhumalo_/status/1519647346270867456
## 32: https://twitter.com/madeoflavenderr/status/1506377255550861312
## 33:    https://twitter.com/MusicPop_art/status/1509142678788878342
## 34:         https://twitter.com/woIfevr/status/1515669028034867200
## 35:     https://twitter.com/lisasuprmcy/status/1509070998074667008
## 36:   https://twitter.com/CantDieBroke2/status/1505696878251421696
## 37:      https://twitter.com/memesiwish/status/1514925668043161602
## 38:       https://twitter.com/kaohom503/status/1513866464356298757
## 39:     https://twitter.com/Adore_Domoo/status/1514087116883566593
## 40:      https://twitter.com/fetishxsel/status/1508557824783179783
##                                                                URL
```

```r
# most appreciated
tw %>% 
  select(PROFILE, FULL_TEXT, RETWEET_COUNT,URL) %>%
  arrange(desc(RETWEET_COUNT))  %>% 
  head(40)
```

```
##             PROFILE
##  1:      dreamjeons
##  2:       triviamor
##  3:    maybeeevirgo
##  4:       taeteland
##  5:         hh_jmxx
##  6:    sincerelyart
##  7:       HRJVISUAL
##  8: Skrille75951826
##  9:    pabllovittar
## 10:        _pinyala
## 11:       bookpoets
## 12:     Adore_Domoo
## 13:     omomomo_ttv
## 14:      imjaebooms
## 15:       huiijunii
## 16:        Akii_Chx
## 17:        jeonjkah
## 18: Skrille75951826
## 19:         hobiyeh
## 20:         jho_pev
## 21:     stefondiggs
## 22:     stefondiggs
## 23:    spanishcvndy
## 24:     lisasuprmcy
## 25:      tannluvbot
## 26:      AgustDS121
## 27:     kootaeverse
## 28:    rainemescudi
## 29:      vantaevivi
## 30: btsforeverworld
## 31:         CG_wwdd
## 32:        namulver
## 33:    crying2frank
## 34:   DonkeyKongApe
## 35:   serendijiminx
## 36:     Anxiousgram
## 37: madeoflavenderr
## 38:        defdaily
## 39:    MusicPop_art
## 40:        bwikweey
##             PROFILE
##                                                                                                                                                               FULL_TEXT
##  1:            <U+0001F464>"are you jeon jungkook?"\n<U+0001F430>"and, if i am?"\n\n<U+0001F464>"i lov…lov… i like you”\n<U+0001F430>“why can’t you say 'i love you' !”
##  2:                                                                                                           i love youu i love you i love you https://t.co/LnpsKXP6kW
##  3:                                                                                                                                           “u look tired” oh bc i am
##  4:                                                                                                                  baby i love u sm ¦<U+FE0E> https://t.co/4dACNpWOrk
##  5:                                                                                                           idc i stalk people i got no shame<U+0001F62D><U+0001F62D>
##  6:                                                                                                 i make THE CUTEST rugs no joke <U+0001F629> https://t.co/XRWvzyq0DD
##  7:                                                             renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
##  8:                                                                                                                      I-NO #GuiltyGearStrive https://t.co/SGZ3brcm4R
##  9:                                                                                        . @MarinaDiamandis I LOVE U <U+0001F91F><U+0001F3FD> https://t.co/sIUoENHxBH
## 10:                                               RT @HRJVISUAL renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
## 11:                                                                                                                                   i woke up https://t.co/xs3zRngpjJ
## 12:                                                                                                                               I love sleeping a$$ naked<U+0001F975>
## 13:                                                                                                                                   I am weak https://t.co/sYG4CBcdLn
## 14:                                                                                                                            GOT7 I love you. https://t.co/fxF2pFp1OZ
## 15:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 16:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 17:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 18:                                                                                                                      I-NO #GuiltyGearStrive https://t.co/siYo2uDZus
## 19:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 20:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 21:                                                                                                                                                     God I love you…
## 22:                                                                                                                                                     God I love you…
## 23:                                                                                                                                                        i love sushi
## 24:                                                                                                                      basta i love u both :< https://t.co/u0KPlzwhSH
## 25:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 26:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 27:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 28:                                                                                                                                 I WIN I WIN https://t.co/Uy7ubqKZs9
## 29:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 30:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 31:                                                                                                                    I love you\n#dreamfanart https://t.co/pT1GGVXUMD
## 32:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 33:                                                                                                                                                  i love frank ocean
## 34:                                                                                                                                                          i love you
## 35:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 36:                                                                                                                                       you're my favorite i love you
## 37:                                                                                                                             i love you lizz https://t.co/sbXNd9QejH
## 38: [INSTAGRAM] 220414 https://t.co/qAAjcViOTP\n\n“I kept my promise”\n\nhttps://t.co/avWrwn11wG\n\n#JAYB #<U+C81C><U+C774><U+BE44> @jaybnow_hr https://t.co/6sTFM4Wh46
## 39:                                                                            I like purple people <U+0001F974><U+0001F49C><U+2728>\n\n#purple https://t.co/qna3WT9bMX
## 40:                                  RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##                                                                                                                                                               FULL_TEXT
##     RETWEET_COUNT
##  1:         19474
##  2:         14123
##  3:         11853
##  4:          9835
##  5:          9457
##  6:          6412
##  7:          4121
##  8:          2967
##  9:          2809
## 10:          2358
## 11:          2347
## 12:          2185
## 13:          2104
## 14:          2006
## 15:          1926
## 16:          1882
## 17:          1881
## 18:          1818
## 19:          1773
## 20:          1753
## 21:          1744
## 22:          1713
## 23:          1704
## 24:          1691
## 25:          1666
## 26:          1666
## 27:          1666
## 28:          1620
## 29:          1617
## 30:          1617
## 31:          1596
## 32:          1546
## 33:          1531
## 34:          1496
## 35:          1464
## 36:          1444
## 37:          1434
## 38:          1397
## 39:          1390
## 40:          1387
##     RETWEET_COUNT
##                                                                URL
##  1:      https://twitter.com/dreamjeons/status/1504752941693124608
##  2:       https://twitter.com/triviamor/status/1512688247956156417
##  3:    https://twitter.com/maybeeevirgo/status/1504597668366008320
##  4:       https://twitter.com/taeteland/status/1515707621487128595
##  5:         https://twitter.com/hh_jmxx/status/1517495381327859717
##  6:    https://twitter.com/sincerelyart/status/1507053925102280719
##  7:       https://twitter.com/HRJVISUAL/status/1510188863175634944
##  8: https://twitter.com/Skrille75951826/status/1508438202679689217
##  9:    https://twitter.com/pabllovittar/status/1507137433317556225
## 10:        https://twitter.com/_pinyala/status/1510227180361744385
## 11:       https://twitter.com/bookpoets/status/1520333216489476097
## 12:     https://twitter.com/Adore_Domoo/status/1514087116883566593
## 13:     https://twitter.com/omomomo_ttv/status/1506288545908109320
## 14:      https://twitter.com/imjaebooms/status/1517732565465313280
## 15:       https://twitter.com/huiijunii/status/1510356270531981323
## 16:        https://twitter.com/Akii_Chx/status/1510331198865768448
## 17:        https://twitter.com/jeonjkah/status/1510334687260065794
## 18: https://twitter.com/Skrille75951826/status/1506606592661532674
## 19:         https://twitter.com/hobiyeh/status/1510312897255202822
## 20:         https://twitter.com/jho_pev/status/1510309567925833734
## 21:     https://twitter.com/stefondiggs/status/1516204214531198976
## 22:     https://twitter.com/stefondiggs/status/1516204214531198976
## 23:    https://twitter.com/spanishcvndy/status/1505301511462535168
## 24:     https://twitter.com/lisasuprmcy/status/1509070998074667008
## 25:      https://twitter.com/tannluvbot/status/1510299887916756998
## 26:      https://twitter.com/AgustDS121/status/1510297623114108930
## 27:     https://twitter.com/kootaeverse/status/1510296074061156354
## 28:    https://twitter.com/rainemescudi/status/1515714247468236801
## 29:      https://twitter.com/vantaevivi/status/1510292715820695555
## 30: https://twitter.com/btsforeverworld/status/1510290910151467013
## 31:         https://twitter.com/CG_wwdd/status/1504728327843573760
## 32:        https://twitter.com/namulver/status/1510288985276493826
## 33:    https://twitter.com/crying2frank/status/1510810844526829568
## 34:   https://twitter.com/DonkeyKongApe/status/1509529228299055111
## 35:   https://twitter.com/serendijiminx/status/1510283970973421572
## 36:     https://twitter.com/Anxiousgram/status/1511266283261771778
## 37: https://twitter.com/madeoflavenderr/status/1506377255550861312
## 38:        https://twitter.com/defdaily/status/1514283150654029834
## 39:    https://twitter.com/MusicPop_art/status/1509142678788878342
## 40:        https://twitter.com/bwikweey/status/1510277352651390977
##                                                                URL
```


# check twitter activity on CRO supply side 


```r
# select relevant CRO profiles
unique(tw[,.N,FROM][order(-N)]) %>%
  filter(N > 5) %>% 
  pull(FROM) -> CRO_TW

tw[FROM %in% CRO_TW,] -> CTW

CTW %>%
  mutate(PROFILE = gsub("^.*\\.com/([^/]+).*", "\\1", URL)) -> CTW

# most popular
CTW %>% 
  group_by(PROFILE) %>%
  summarise(FOLLOW = mean(FOLLOWERS_COUNT)) %>%
  arrange(desc(FOLLOW))  %>% 
  head(100)
```

```
## # A tibble: 100 x 2
##    PROFILE          FOLLOW
##    <chr>             <dbl>
##  1 convomf         770027 
##  2 convomfs        387564.
##  3 BTS_twt_TAEHYNG 333488 
##  4 baIenciagajk    244914 
##  5 DNEVNIKhr       243775.
##  6 HNS_CFF         240832.
##  7 24sata_HR       199798.
##  8 VladaRH         191339.
##  9 vecernji_list   175780.
## 10 waifuumia       170063 
## # ... with 90 more rows
```

```r
# most influential
CTW %>% 
  group_by(PROFILE) %>%
  summarise(REACH = sum(REACH)) %>%
  arrange(desc(REACH))  %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         REACH
##    <chr>           <int>
##  1 DNEVNIKhr     7391289
##  2 24sata_HR     5851797
##  3 vecernji_list 4009695
##  4 VladaRH       2751524
##  5 hrtvijesti    2344931
##  6 HNS_CFF        642727
##  7 convomf        621324
##  8 N1infoZG       426762
##  9 novilisthr     424521
## 10 poligrafnews   400520
## # ... with 30 more rows
```

```r
# most influential II
CTW %>% 
  group_by(PROFILE) %>%
  summarise(INTERACTIONS = sum(INTERACTIONS)) %>%
  arrange(desc(INTERACTIONS))  %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         INTERACTIONS
##    <chr>                  <int>
##  1 convomf                30323
##  2 HRJVISUAL              17177
##  3 waifuumia              10750
##  4 madeoflavenderr         9496
##  5 branchica               9275
##  6 AndrejPlenkovic         9154
##  7 lisasuprmcy             9133
##  8 crying2frank            7578
##  9 jeonghanszone           7233
## 10 MejMin                  6643
## # ... with 30 more rows
```

```r
# most appreciated
CTW %>% 
  group_by(PROFILE) %>%
  summarise(FAVORITE = sum(FAVORITE_COUNT)) %>%
  arrange(desc(FAVORITE)) %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         FAVORITE
##    <chr>              <int>
##  1 convomf            27974
##  2 HRJVISUAL          13056
##  3 waifuumia          10318
##  4 branchica           8671
##  5 AndrejPlenkovic     8122
##  6 madeoflavenderr     8062
##  7 lisasuprmcy         7442
##  8 konjos              6348
##  9 MejMin              6330
## 10 crying2frank        6047
## # ... with 30 more rows
```

```r
# most appreciated
CTW %>% 
  group_by(PROFILE) %>%
  summarise(RETWEET = sum(RETWEET_COUNT)) %>%
  arrange(desc(RETWEET))  %>% 
  head(40)
```

```
## # A tibble: 40 x 2
##    PROFILE         RETWEET
##    <chr>             <int>
##  1 HRJVISUAL          4121
##  2 SneanaKrstovic1    2552
##  3 negde_tamo         2545
##  4 markovic_dana      2430
##  5 convomf            2349
##  6 lisasuprmcy        1691
##  7 crying2frank       1531
##  8 jeonghanszone      1517
##  9 madeoflavenderr    1434
## 10 Nevenaa84          1426
## # ... with 30 more rows
```



```r
# PER TWEET

# most popular
CTW %>% 
  select(PROFILE, FULL_TEXT, FOLLOWERS_COUNT,URL) %>%
  arrange(desc(FOLLOWERS_COUNT))  %>% 
  head(40)
```

```
##             PROFILE
##  1:         convomf
##  2:         convomf
##  3:         convomf
##  4:         convomf
##  5:         convomf
##  6:         convomf
##  7:         convomf
##  8:        convomfs
##  9:        convomfs
## 10:        convomfs
## 11:        convomfs
## 12: BTS_twt_TAEHYNG
## 13:       DNEVNIKhr
## 14:       DNEVNIKhr
## 15:       DNEVNIKhr
## 16:       DNEVNIKhr
## 17:       DNEVNIKhr
## 18:       DNEVNIKhr
## 19:       DNEVNIKhr
## 20:       DNEVNIKhr
## 21:       DNEVNIKhr
## 22:       DNEVNIKhr
## 23:       DNEVNIKhr
## 24:       DNEVNIKhr
## 25:       DNEVNIKhr
## 26:       DNEVNIKhr
## 27:       DNEVNIKhr
## 28:       DNEVNIKhr
## 29:       DNEVNIKhr
## 30:       DNEVNIKhr
## 31:       DNEVNIKhr
## 32:       DNEVNIKhr
## 33:       DNEVNIKhr
## 34:       DNEVNIKhr
## 35:       DNEVNIKhr
## 36:       DNEVNIKhr
## 37:       DNEVNIKhr
## 38:       DNEVNIKhr
## 39:       DNEVNIKhr
## 40:       DNEVNIKhr
##             PROFILE
##                                                                                                                                                                                                                                                                                                                                               FULL_TEXT
##  1:                                                                                                                                                                                                                                                                                                                     <U+0001F499> hi R, i love you..
##  2:                                                                                                                                                                                                                                                                                                             kak j, i have crush on you <U+0001F499>
##  3:                                                                                                                                                                                                                                                                                                                         <U+0001F499> i love you, A.
##  4:                                                                                                                                                                                                                                                                                                                           <U+0001F499> i love you J
##  5:                                                                                                                                                                                                                                                                                           <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
##  6:                                                                                                                                                                                                                                                                <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
##  7:                                                                                                                                                                                                                                                                                                        kak @/tkwpcnfak i hv crush on u <U+0001F499>
##  8:                                                                                                                                                                                                                                                                                                                        am i a problem? <U+0001F4AD>
##  9:                                                                                                                                                                                                                                                                                                            i dare u pap suhu di dom lo <U+0001F90D>
## 10:                                                                                                                                                                                                                                                                                                          <U+0001F90D> i like u, kasih feedback pls?
## 11:                                                                                                                                                                                                                                                                                 <U+0001F90D> kalo kamu brondong i love you <U+0001FAF6><U+0001F3FB>
## 12:                                                                                                                                                                                                                                                                                                                                     I love you ARMY
## 13:                                                                                                                                                                                                                                                                                         Saznajte sve odgovore. #dnevnikhr \nhttps://t.co/5zs0bSUNTh
## 14: RT @zeljkagulan Najveca epidemija rotavirusa u posljednjih 30 godina kaze infektolog Tesovic za #DnevnikNoveTv @DNEVNIKhr \n\nhttps://t.co/kG7lZOz8hU https://dnevnik.hr/vijesti/hrvatska/goran-tesovic-o-ospicama-hepatitisu-a-i-rotavirusu-to-je-najveca-epidemija---722308.html?itm_source=HomeTopRow&itm_medium=Dnevnik&itm_campaign=Naslovnica
## 15:                                                                                                                                                                                                                                                                     Gledajte utakmice i dalje na @novahr #dnevnikhr @golhr\nhttps://t.co/vgjnXW8512
## 16:                                                                                                                                                                                                                                                                      [VIDEO] "Prenosi se putem nečistih ruku." #dnevnikhr \nhttps://t.co/nANCmdqeAU
## 17:                                                                                                                                                                                                            Svi govore o haljini Mije Dimšić koja nam se javila iz Torina, a bili smo i na svadbi Marka Livaje i Iris Rajčić. #DnevnikNoveTV @novahr
## 18:                                                                                                                                                              On je srednjoškolac, otkrio je tri nove vrste kukaca i među najmlađim je znanstvenicima u Hrvatskoj. sedamnaestogodišnji Sebastian Ćato naša je BOLJA HRVATSKA! #DnevnikNoveTV @novahr
## 19:                                                                                                                                                                                                                                                         Zbog rotavirusa u bolnici završio i ministar branitelja Tomo Medved. #DnevnikNoveTV @novahr
## 20:                                                                                                               Skinuli smo maske, no krenule nove zdravstvene prijetnje: među školarcima se pojavio hepatitis A, zbog necijepljenja moguća i epidemija ospica. Valentina Baus razgovarat će s infektologom Goranom Tešovićem. #DnevnikNoveTV @novahr
## 21:                                                                                                                                                   Hrvatska nogometna reprezentacija i idućih šest godina na Novoj TV - nastavljamo s prijenosima svih utakmica kvalifikacija, Lige nacija te prijateljskih dvoboja Vatrenih! #DnevnikNoveTV @novahr
## 22:                                                           Od ponoći ponovno poskupljuje gorivo - u Bruxellesu hitan sastanak ministara energetike oko isporuke plina - prijeti li Europi recesija? Mislav Bago razgovarat će s ministrom financija Zdravkom Marićem. \n\nPratite #DnevnikNoveTV od 19 i 15 sati. #dnevnikhr https://t.co/25xDeQOuXp
## 23:                                                                       RT @Mbolsec V. Filipović za @novahr: od iduće godine nacionalni ispiti za sve osmaše,  ZA SAD bez ocjena. Izgledno da će se '23. ispit pisati i iz stranog jezika, razmišlja se i o geografiji  i povijesti. \n@DNEVNIKhr \n\n https://t.co/cK3dERKYfT https://bit.ly/3KCiFMP
## 24:                                                                                                                                                                                                                                                                                              I to se dogodilo. #dnevnikhr \nhttps://t.co/O5WE9ygZ10
## 25:                                                                                                                                                                                                                                                                         Treba li ovakve kazne uvesti i kod nas? #dnevnikhr\nhttps://t.co/4i1TjfZwY2
## 26:                                                                                                                                                                                                                                                                            Zatvaraju ljude metalnim barijerama. #dnevnikhr\nhttps://t.co/xaey9Ryunt
## 27:                                                                                                                                                                                                                                                                                          Čeka se odluka. #dnevnikhr @golhr\nhttps://t.co/DcLj1vndql
## 28:                                                                                                                                                                                                                                                                                    Povodom Razmanskog bajrama. #dnevnikhr \nhttps://t.co/kFK7rYHeSm
## 29:                                                                                                                                                                                                                                                                                  U ovome je trik. #dnevnikhr @zadovoljnahr\nhttps://t.co/PdNhS9VmNX
## 30:                                                                                                                                                          Milanović na obilježavanju godišnjice Bljeska: "Hrvatska vojska je bila u inicijativi i slavno je završila rat." #dnevnikhr\n\nViše na --> https://t.co/THD2ZJzJyk https://t.co/404MuyBtip
## 31:                                                                                                                                                                                                                                                                               Velika obljetnica, velike akcije. #dnevnikhr\nhttps://t.co/8WqdATeBC5
## 32:                                                                                                                                                                                                                                                                               [VIDEO] Muke zbog prekovremenih. #dnevnikhr \nhttps://t.co/rzPWjFsQp9
## 33:                                                                                                                                                                                                   U subotu se Vučić s Dodikom u društvu hrani vojnim arsenalom i priča o vojnoj neutralnosti. Glavni adut stigao mu iz Kine. #DnevnikNoveTV @novahr
## 34:                                                                                                                                                                                                Umjesto plaćanja pošteno zarađenog za prekovremene, sustav kojem je na čelu Beroš nastavlja sudovanje s doktorima i sestrama. #DnevnikNoveTV @novahr
## 35:                                                                                                                                                                                                                        Rusi pojačali napade na Ukrajinu. Pratite Dnevnik Nove TV od 19 i 15 sati na @novahr. \n\n#dnevnikhr https://t.co/i4aCBBpsui
## 36:                                                                                                                                                                                                                                                                                          Više sreće drugi put. #dnevnikhr \nhttps://t.co/YNMr6ibbvd
## 37:                                                                                                                                                                                                                                                               I mali balkon može biti prekrasan. #dnevnikhr @zadovoljnahr \nhttps://t.co/D5hwRGQd3H
## 38:                                                                                                                                                                                                                                                                              Napao ju je i verbalno i fizički. #dnevnikhr \nhttps://t.co/cBMLOP2lCZ
## 39:                                                                                                                                                                                                                                         Boris Beker, šesterostruki osvajač Grand Slama osuđen na dvije i pol godine zatvora. #DnevnikNoveTV @novahr
## 40:                                                                                                                                                                                             Litra ulja 18 kuna. Litra dizela 13 kuna. Inflacija rapidno raste. Pratite #DnevnikNoveTV od 19 i 15 sati @novahr. \n#dnevnikhr https://t.co/IG275SH8e7
##                                                                                                                                                                                                                                                                                                                                               FULL_TEXT
##     FOLLOWERS_COUNT
##  1:          821595
##  2:          814402
##  3:          759869
##  4:          754953
##  5:          754898
##  6:          752605
##  7:          731867
##  8:          419549
##  9:          384108
## 10:          376714
## 11:          369883
## 12:          333488
## 13:          246461
## 14:          246450
## 15:          246450
## 16:          246450
## 17:          246444
## 18:          246444
## 19:          246444
## 20:          246444
## 21:          246441
## 22:          246436
## 23:          246434
## 24:          246428
## 25:          246422
## 26:          246422
## 27:          246421
## 28:          246369
## 29:          246338
## 30:          246278
## 31:          246269
## 32:          246255
## 33:          246239
## 34:          246234
## 35:          246234
## 36:          246228
## 37:          246215
## 38:          246114
## 39:          246107
## 40:          246087
##     FOLLOWERS_COUNT
##                                                                URL
##  1:         https://twitter.com/convomf/status/1515595937879830531
##  2:         https://twitter.com/convomf/status/1514536446387884035
##  3:         https://twitter.com/convomf/status/1505844167775518721
##  4:         https://twitter.com/convomf/status/1506674654370738180
##  5:         https://twitter.com/convomf/status/1506662084830179330
##  6:         https://twitter.com/convomf/status/1506464608478851073
##  7:         https://twitter.com/convomf/status/1503918126529843200
##  8:        https://twitter.com/convomfs/status/1519718149075398656
##  9:        https://twitter.com/convomfs/status/1510124872453672960
## 10:        https://twitter.com/convomfs/status/1508120079564939264
## 11:        https://twitter.com/convomfs/status/1506568238062530561
## 12: https://twitter.com/BTS_twt_TAEHYNG/status/1519351095226937344
## 13:       https://twitter.com/DNEVNIKhr/status/1521229609236414466
## 14:       https://twitter.com/DNEVNIKhr/status/1521206853878927363
## 15:       https://twitter.com/DNEVNIKhr/status/1521203039171842051
## 16:       https://twitter.com/DNEVNIKhr/status/1521202895756083201
## 17:       https://twitter.com/DNEVNIKhr/status/1521186507104550913
## 18:       https://twitter.com/DNEVNIKhr/status/1521185248972791809
## 19:       https://twitter.com/DNEVNIKhr/status/1521181474342375424
## 20:       https://twitter.com/DNEVNIKhr/status/1521180217691234309
## 21:       https://twitter.com/DNEVNIKhr/status/1521182983855747073
## 22:       https://twitter.com/DNEVNIKhr/status/1521170577041477632
## 23:       https://twitter.com/DNEVNIKhr/status/1521163758416105474
## 24:       https://twitter.com/DNEVNIKhr/status/1521131062558179328
## 25:       https://twitter.com/DNEVNIKhr/status/1521113462973284354
## 26:       https://twitter.com/DNEVNIKhr/status/1521109938189357056
## 27:       https://twitter.com/DNEVNIKhr/status/1521105271891841026
## 28:       https://twitter.com/DNEVNIKhr/status/1520859040657588227
## 29:       https://twitter.com/DNEVNIKhr/status/1520806503309000705
## 30:       https://twitter.com/DNEVNIKhr/status/1520675825082933248
## 31:       https://twitter.com/DNEVNIKhr/status/1520651481720012800
## 32:       https://twitter.com/DNEVNIKhr/status/1520510999710871554
## 33:       https://twitter.com/DNEVNIKhr/status/1520461389906817026
## 34:       https://twitter.com/DNEVNIKhr/status/1520455455981637633
## 35:       https://twitter.com/DNEVNIKhr/status/1520446362512408576
## 36:       https://twitter.com/DNEVNIKhr/status/1520424234580905986
## 37:       https://twitter.com/DNEVNIKhr/status/1520403403482058754
## 38:       https://twitter.com/DNEVNIKhr/status/1520110416382156803
## 39:       https://twitter.com/DNEVNIKhr/status/1520098592194707456
## 40:       https://twitter.com/DNEVNIKhr/status/1520083001677434881
##                                                                URL
```

```r
# most influential
CTW %>% 
  select(PROFILE, FULL_TEXT, REACH,URL) %>%
  arrange(desc(REACH))  %>% 
  head(40)
```

```
##             PROFILE
##  1:       HRJVISUAL
##  2:         convomf
##  3:         convomf
##  4:     lisasuprmcy
##  5: madeoflavenderr
##  6:       waifuumia
##  7:    crying2frank
##  8: BTS_twt_TAEHYNG
##  9:   jeonghanszone
## 10:          MejMin
## 11:         convomf
## 12:   archivesofjay
## 13:        G0THRUBY
## 14:         convomf
## 15:    baIenciagajk
## 16:         convomf
## 17:  SamsungCroatia
## 18:      JKSIMPRINT
## 19:         convomf
## 20:     AndjaMarics
## 21: MayaSar77291463
## 22:         convomf
## 23:       branchica
## 24:          BEG97N
## 25:      googiemilk
## 26:        kvluvs_7
## 27:   UspehPetrovic
## 28:   jeonghanszone
## 29: nemanja_stankov
## 30:     ex9thoughts
## 31: SneanaKrstovic1
## 32:      negde_tamo
## 33: SneanaKrstovic1
## 34:      negde_tamo
## 35:         PJSFEED
## 36:   MajaVojinovi1
## 37:       vojinovi2
## 38:         raebits
## 39:      _Apostle13
## 40:  BogdanTanjevic
##             PROFILE
##                                                                                                                                                                                                                                                                                                                         FULL_TEXT
##  1:                                                                                                                                                                                                                       renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
##  2:                                                                                                                                                                                                                                                                     <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
##  3:                                                                                                                                                                                                                                          <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
##  4:                                                                                                                                                                                                                                                                                basta i love u both :< https://t.co/u0KPlzwhSH
##  5:                                                                                                                                                                                                                                                                                       i love you lizz https://t.co/sbXNd9QejH
##  6:                                                                                                                                                                                                                                                                        do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
##  7:                                                                                                                                                                                                                                                                                                            i love frank ocean
##  8:                                                                                                                                                                                                                                                                                                               I love you ARMY
##  9:                                                                                                                                                                                                                                                              i love you jeonghan ¦<U+FE0E> @pledis_17 https://t.co/hfilqActNZ
## 10:                                                                Hoću li u pakao jer čitav dan umirem od smijeha na priču o gospođi koja je uspjela nakon milionite vještačke u Pragu da zatrudni i rodi sinčića pa nedavno na nekom rođendanu rekla A VIDI KAKAV JE NEMIRAN, ENO GA OPET JE PAO A JOŠ GA NI OTPLATILI NISMO <3
## 11:                                                                                                                                                                                                                                                                                                   <U+0001F499> i love you, A.
## 12:                                                                                                                                                                                                                                                                 Shhh… i love u ¦<U+FE0E> #enhypen_jay https://t.co/EGjaOuddPw
## 13:                                                                                                                                                                                                                                                                              "i love jennie kim but-" https://t.co/lO3kma1moY
## 14:                                                                                                                                                                                                                                                                                               <U+0001F499> hi R, i love you..
## 15:                                                                                                                                                                                                                                                                                         i am OBSESSED https://t.co/AjDhmprdJ7
## 16:                                                                                                                                                                                                                                                                                       kak j, i have crush on you <U+0001F499>
## 17:                                                                                                                                                                                                                        Imamo odgovor: #Jin! #MakeNightsEpic uz novi #GalaxyS22 i Nightography kameru. https://t.co/B76ccFM2Lv
## 18:                                                                                                                                                                                                                                                                      i wanna bop jungkook’s cute nose https://t.co/CM20bO8zIJ
## 19:                                                                                                                                                                                                                                                                                                     <U+0001F499> i love you J
## 20:                                          inače doživjela sam pravo renesansu u bajnoj Opatiji (Thalassoterapija) i prije 4 dana učinila prve samostalne korake nakon 7 godina Bivanja u okovima svog tijela smrznuta i zaleđena od emocionalnih šokova. sada hodam kao beba ali znate bebe uskoro ću… https://t.co/gatgYSOoWz
## 21:                                                                                                                                                                                            RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 22:                                                                                                                                                                                                                                                                                  kak @/tkwpcnfak i hv crush on u <U+0001F499>
## 23:                                                                                                                                                                                     Budem ljuta na ljude zbog odnosa prema meni, a onda shvatim da sam sve, ali sve dozvolila. Prećutala, odobrila, preskočila. I tako redom.
## 24:                                                                                                                                                                                            RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 25:                                                                                                                                                                                            RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 26:                                                                                                                                                                                            RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 27:                                                                                                                                                                                                  Svo zlo i psihopatija Aleksandra Vučića se vidi u rečenici: Tamo su ljudi još siromašniji i tamo sam još više glasova dobio!
## 28:                                                                                                                                                                                                                                                           i love you jeonghan <U+0001F5A4> @pledis_17 https://t.co/edxEUxtLec
## 29:               RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 30:                                                                                                                                                                                                                                                                                     <U+0001F430><U+0001F4AD>\n\n-‘i love you’
## 31:               RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 32:               RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 33: RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 34: RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 35:                                                                                                                                                                                                                                                                                           i love you~ https://t.co/PzZeLUHouN
## 36: RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 37: RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 38:                                                                                                                                                                                                                                                                                   i love u 1 INT anne https://t.co/vKzQcy0J4o
## 39:                                                                                                                                              Nisam ni znao da uz Frica i žene, sa mnom u stanu živi još dvadeset šestoro nepoznatih ljudi. Dobro dok Vodovod ne provali pa mi za sve nas naplati vodu https://t.co/VueJQKKMwc
## 40:                                                                  Moj dragi drug, citavog zivota, moj komsija, njegova djeca, njegova Asima, svi mi … u najtezoj boli… otosao nam je NAJVECI I NAJBOLJI SARAJLIJA…na slici, nakon sto smo Ivica Saric i ja otpjevali ..Konjuh planinom… njemu u cast.. https://t.co/7sv4MK6cOv
##                                                                                                                                                                                                                                                                                                                         FULL_TEXT
##      REACH                                                            URL
##  1: 256021       https://twitter.com/HRJVISUAL/status/1510188863175634944
##  2: 210848         https://twitter.com/convomf/status/1506662084830179330
##  3: 166730         https://twitter.com/convomf/status/1506464608478851073
##  4: 125758     https://twitter.com/lisasuprmcy/status/1509070998074667008
##  5: 125233 https://twitter.com/madeoflavenderr/status/1506377255550861312
##  6: 124779       https://twitter.com/waifuumia/status/1512555707677265922
##  7: 106402    https://twitter.com/crying2frank/status/1510810844526829568
##  8:  74993 https://twitter.com/BTS_twt_TAEHYNG/status/1519351095226937344
##  9:  74031   https://twitter.com/jeonghanszone/status/1505537038858158082
## 10:  67569          https://twitter.com/MejMin/status/1513641129274757128
## 11:  62733         https://twitter.com/convomf/status/1505844167775518721
## 12:  53052   https://twitter.com/archivesofjay/status/1506634795220185095
## 13:  52982        https://twitter.com/G0THRUBY/status/1508472579119587331
## 14:  51341         https://twitter.com/convomf/status/1515595937879830531
## 15:  49911    https://twitter.com/baIenciagajk/status/1503244320991707137
## 16:  48477         https://twitter.com/convomf/status/1514536446387884035
## 17:  48332  https://twitter.com/SamsungCroatia/status/1514893978554056711
## 18:  48003      https://twitter.com/JKSIMPRINT/status/1511584224255496202
## 19:  43178         https://twitter.com/convomf/status/1506674654370738180
## 20:  41116     https://twitter.com/AndjaMarics/status/1518316761980018688
## 21:  41070 https://twitter.com/MayaSar77291463/status/1510281356118355971
## 22:  38017         https://twitter.com/convomf/status/1503918126529843200
## 23:  36317       https://twitter.com/branchica/status/1511069916270669824
## 24:  35307          https://twitter.com/BEG97N/status/1510269828963074054
## 25:  35178      https://twitter.com/googiemilk/status/1510269574784385024
## 26:  35161        https://twitter.com/kvluvs_7/status/1510269571131113478
## 27:  33596   https://twitter.com/UspehPetrovic/status/1510748548895236099
## 28:  31633   https://twitter.com/jeonghanszone/status/1507294564763066370
## 29:  27637 https://twitter.com/nemanja_stankov/status/1511034412737208325
## 30:  27413     https://twitter.com/ex9thoughts/status/1509571590903767045
## 31:  26049 https://twitter.com/SneanaKrstovic1/status/1510898146372636672
## 32:  25697      https://twitter.com/negde_tamo/status/1510898566671310851
## 33:  25089 https://twitter.com/SneanaKrstovic1/status/1510898070577463299
## 34:  24947      https://twitter.com/negde_tamo/status/1510898486107152385
## 35:  24272         https://twitter.com/PJSFEED/status/1512257416468963330
## 36:  23773   https://twitter.com/MajaVojinovi1/status/1510864257516253184
## 37:  23761       https://twitter.com/vojinovi2/status/1510864396909789186
## 38:  23021         https://twitter.com/raebits/status/1508018240382939136
## 39:  22415      https://twitter.com/_Apostle13/status/1507689887453040641
## 40:  21980  https://twitter.com/BogdanTanjevic/status/1520757104046399488
##      REACH                                                            URL
```

```r
# most influential II
CTW %>% 
  select(PROFILE, FULL_TEXT, INTERACTIONS,URL) %>%
  arrange(desc(INTERACTIONS))  %>% 
  head(40)
```

```
##             PROFILE
##  1:       HRJVISUAL
##  2:         convomf
##  3:         convomf
##  4:       waifuumia
##  5: madeoflavenderr
##  6:     lisasuprmcy
##  7:    crying2frank
##  8:          MejMin
##  9: BTS_twt_TAEHYNG
## 10:   jeonghanszone
## 11:        G0THRUBY
## 12:     AndjaMarics
## 13:      JKSIMPRINT
## 14:   archivesofjay
## 15:  SamsungCroatia
## 16:    baIenciagajk
## 17:   UspehPetrovic
## 18:       branchica
## 19:      Dzoni77777
## 20:  BogdanTanjevic
## 21:     ex9thoughts
## 22:   jeonghanszone
## 23:         ttaasam
## 24:         raebits
## 25:    KOJO54579153
## 26: DejanVuckovic12
## 27:     munkov_krik
## 28:      _Apostle13
## 29:         convomf
## 30:    kunimisbangs
## 31:    KittyFormanA
## 32: MnogoNebitanLik
## 33: MayaSar77291463
## 34:  BogdanTanjevic
## 35:       branchica
## 36:        DaAnunna
## 37:       riajentle
## 38:         PJSFEED
## 39:          BEG97N
## 40:      googiemilk
##             PROFILE
##                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##  1:                                                                                                                                                                                                               renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
##  2:                                                                                                                                                                                                                                                             <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
##  3:                                                                                                                                                                                                                                  <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
##  4:                                                                                                                                                                                                                                                                do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
##  5:                                                                                                                                                                                                                                                                               i love you lizz https://t.co/sbXNd9QejH
##  6:                                                                                                                                                                                                                                                                        basta i love u both :< https://t.co/u0KPlzwhSH
##  7:                                                                                                                                                                                                                                                                                                    i love frank ocean
##  8:                                                        Hoću li u pakao jer čitav dan umirem od smijeha na priču o gospođi koja je uspjela nakon milionite vještačke u Pragu da zatrudni i rodi sinčića pa nedavno na nekom rođendanu rekla A VIDI KAKAV JE NEMIRAN, ENO GA OPET JE PAO A JOŠ GA NI OTPLATILI NISMO <3
##  9:                                                                                                                                                                                                                                                                                                       I love you ARMY
## 10:                                                                                                                                                                                                                                                      i love you jeonghan ¦<U+FE0E> @pledis_17 https://t.co/hfilqActNZ
## 11:                                                                                                                                                                                                                                                                      "i love jennie kim but-" https://t.co/lO3kma1moY
## 12:                                  inače doživjela sam pravo renesansu u bajnoj Opatiji (Thalassoterapija) i prije 4 dana učinila prve samostalne korake nakon 7 godina Bivanja u okovima svog tijela smrznuta i zaleđena od emocionalnih šokova. sada hodam kao beba ali znate bebe uskoro ću… https://t.co/gatgYSOoWz
## 13:                                                                                                                                                                                                                                                              i wanna bop jungkook’s cute nose https://t.co/CM20bO8zIJ
## 14:                                                                                                                                                                                                                                                         Shhh… i love u ¦<U+FE0E> #enhypen_jay https://t.co/EGjaOuddPw
## 15:                                                                                                                                                                                                                Imamo odgovor: #Jin! #MakeNightsEpic uz novi #GalaxyS22 i Nightography kameru. https://t.co/B76ccFM2Lv
## 16:                                                                                                                                                                                                                                                                                 i am OBSESSED https://t.co/AjDhmprdJ7
## 17:                                                                                                                                                                                          Svo zlo i psihopatija Aleksandra Vučića se vidi u rečenici: Tamo su ljudi još siromašniji i tamo sam još više glasova dobio!
## 18:                                                                                                                                                                             Budem ljuta na ljude zbog odnosa prema meni, a onda shvatim da sam sve, ali sve dozvolila. Prećutala, odobrila, preskočila. I tako redom.
## 19:                               S kime ste se vi vucarali kad ste imali 20 g.? Ja s jednom Zerinom, dušu mi je izjela ta osoba iznutra kao termit. Poslije se udala za nekakvog IFOR-ovca i skrasila se u Švedskoj kraljevini. Zerina, ako čitaš ovo, vrati mi dupli CD Electric Ladyland od Hendrixa kriminogena osobo
## 20:                                                          Moj dragi drug, citavog zivota, moj komsija, njegova djeca, njegova Asima, svi mi … u najtezoj boli… otosao nam je NAJVECI I NAJBOLJI SARAJLIJA…na slici, nakon sto smo Ivica Saric i ja otpjevali ..Konjuh planinom… njemu u cast.. https://t.co/7sv4MK6cOv
## 21:                                                                                                                                                                                                                                                                             <U+0001F430><U+0001F4AD>\n\n-‘i love you’
## 22:                                                                                                                                                                                                                                                   i love you jeonghan <U+0001F5A4> @pledis_17 https://t.co/edxEUxtLec
## 23:                                                                                                                                                                                                                                          bože ali nindža ratnici i ona dva luda voditelja obilježili moje djetinjstvo
## 24:                                                                                                                                                                                                                                                                           i love u 1 INT anne https://t.co/vKzQcy0J4o
## 25:                                                                                                                                                                                                                                      Narod koji odbija besplatne udzbenike zasluzuje vucica i njevovih 40 razbojnika.
## 26:                                                                                                                                                                                                                                                                  Dobrodošao Rio Tinto i doviđenja EPS.\nZa početak...
## 27:                                                                                                           joj bože ulazi čovjek u malu kafeteriju i traži neki alkohol i barista mu kaže "ne držimo alkohol gospodine samo kafu" i čovjek stoji u nevjerici too stunned to speak i na kraju ispusti neki krik i izađe
## 28:                                                                                                                                      Nisam ni znao da uz Frica i žene, sa mnom u stanu živi još dvadeset šestoro nepoznatih ljudi. Dobro dok Vodovod ne provali pa mi za sve nas naplati vodu https://t.co/VueJQKKMwc
## 29:                                                                                                                                                                                                                                                                                           <U+0001F499> i love you, A.
## 30:                                                                                            suna: so i have a crush on a miya\nginjima: oh, i didn’t know you liked guys. which miya?\nsuna: the oldest one\nginjima: atsumu?!\nsuna: no\nsuna: miya…san\nginjima:\nginjima: <U+0001F9CD><U+200D>><U+FE0F><U+0001F6AA>
## 31: Imala sam kolegu koji me je svakog dana vozio na posao, a imao neku dosadnu ženu. Jedno jutro sam ga pozvala gore jer sam se kao uspavala, obukla tange i prošetala se ispred njega. Odmah se zaljubio! <U+0001F970>\nTo je moj sadašnji muž.\nLjubav uvijek pobjedi <U+0001F49E><U+0001F493><U+0001F49C><U+0001F970>
## 32:                                                                                                                                Kod muškaraca su najbitnija široka ramena, biceps, triceps, kvadriceps, falus i da mu nije mala plata.\nEventualno da je duhovit i elokventan.\nI da ne živi s majkom.\nI da ima auto.
## 33:                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 34:                                                     …( kad svi pomremo, ko ce mladim objasniti da smo se nekad iskreno voljeli ..!??)..danas nas je napustio Pero Skansi…LIPI MOJ DALMATA… kako ga je zvao Otavio Missoni… veliki igrac i veliki trener… priatelj oduvjek… tuga… neizmjerna.. https://t.co/rkE8rXlt8G
## 35:                                                                                                                                                                                                                                                                                 Treba sve odjebati i ići ovce čuvati.
## 36:                                                                                                                                                                                                                                                                           ja i svaki lik ikad https://t.co/G3ihjV4ko3
## 37:                                                                                                                                                                                                                                                                         i love u <U+0001F979> https://t.co/KzCfxMB6CQ
## 38:                                                                                                                                                                                                                                                                                   i love you~ https://t.co/PzZeLUHouN
## 39:                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
## 40:                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##     INTERACTIONS                                                            URL
##  1:        17177       https://twitter.com/HRJVISUAL/status/1510188863175634944
##  2:        15821         https://twitter.com/convomf/status/1506662084830179330
##  3:        11324         https://twitter.com/convomf/status/1506464608478851073
##  4:        10750       https://twitter.com/waifuumia/status/1512555707677265922
##  5:         9496 https://twitter.com/madeoflavenderr/status/1506377255550861312
##  6:         9133     https://twitter.com/lisasuprmcy/status/1509070998074667008
##  7:         7578    https://twitter.com/crying2frank/status/1510810844526829568
##  8:         6126          https://twitter.com/MejMin/status/1513641129274757128
##  9:         5349 https://twitter.com/BTS_twt_TAEHYNG/status/1519351095226937344
## 10:         5317   https://twitter.com/jeonghanszone/status/1505537038858158082
## 11:         3943        https://twitter.com/G0THRUBY/status/1508472579119587331
## 12:         3930     https://twitter.com/AndjaMarics/status/1518316761980018688
## 13:         3845      https://twitter.com/JKSIMPRINT/status/1511584224255496202
## 14:         3830   https://twitter.com/archivesofjay/status/1506634795220185095
## 15:         3395  https://twitter.com/SamsungCroatia/status/1514893978554056711
## 16:         3175    https://twitter.com/baIenciagajk/status/1503244320991707137
## 17:         2894   https://twitter.com/UspehPetrovic/status/1510748548895236099
## 18:         2769       https://twitter.com/branchica/status/1511069916270669824
## 19:         2054      https://twitter.com/Dzoni77777/status/1518233886978764800
## 20:         2026  https://twitter.com/BogdanTanjevic/status/1520757104046399488
## 21:         1978     https://twitter.com/ex9thoughts/status/1509571590903767045
## 22:         1916   https://twitter.com/jeonghanszone/status/1507294564763066370
## 23:         1782         https://twitter.com/ttaasam/status/1508920538160316420
## 24:         1772         https://twitter.com/raebits/status/1508018240382939136
## 25:         1751    https://twitter.com/KOJO54579153/status/1516639873737842691
## 26:         1750 https://twitter.com/DejanVuckovic12/status/1510866307050356738
## 27:         1703     https://twitter.com/munkov_krik/status/1503118989379133449
## 28:         1635      https://twitter.com/_Apostle13/status/1507689887453040641
## 29:         1608         https://twitter.com/convomf/status/1505844167775518721
## 30:         1590    https://twitter.com/kunimisbangs/status/1504049392587886594
## 31:         1527    https://twitter.com/KittyFormanA/status/1512040722815410181
## 32:         1370 https://twitter.com/MnogoNebitanLik/status/1503451092461445128
## 33:         1369 https://twitter.com/MayaSar77291463/status/1510281356118355971
## 34:         1365  https://twitter.com/BogdanTanjevic/status/1511031465525583878
## 35:         1328       https://twitter.com/branchica/status/1505483383799005193
## 36:         1273        https://twitter.com/DaAnunna/status/1516437634129842191
## 37:         1262       https://twitter.com/riajentle/status/1511352634900611077
## 38:         1185         https://twitter.com/PJSFEED/status/1512257416468963330
## 39:         1172          https://twitter.com/BEG97N/status/1510269828963074054
## 40:         1172      https://twitter.com/googiemilk/status/1510269574784385024
##     INTERACTIONS                                                            URL
```

```r
# most appreciated
CTW %>% 
  select(PROFILE, FULL_TEXT, FAVORITE_COUNT,URL) %>%
  arrange(desc(FAVORITE_COUNT))  %>% 
  head(40)
```

```
##             PROFILE
##  1:         convomf
##  2:       HRJVISUAL
##  3:         convomf
##  4:       waifuumia
##  5: madeoflavenderr
##  6:     lisasuprmcy
##  7:    crying2frank
##  8:          MejMin
##  9: BTS_twt_TAEHYNG
## 10:   jeonghanszone
## 11:     AndjaMarics
## 12:      JKSIMPRINT
## 13:        G0THRUBY
## 14:   archivesofjay
## 15:    baIenciagajk
## 16:  SamsungCroatia
## 17:   UspehPetrovic
## 18:       branchica
## 19:      Dzoni77777
## 20:  BogdanTanjevic
## 21: DejanVuckovic12
## 22:     munkov_krik
## 23:    KOJO54579153
## 24:     ex9thoughts
## 25:         ttaasam
## 26:         raebits
## 27:    KittyFormanA
## 28:    kunimisbangs
## 29:   jeonghanszone
## 30:      _Apostle13
## 31: MnogoNebitanLik
## 32:  BogdanTanjevic
## 33:       riajentle
## 34:       branchica
## 35:        DaAnunna
## 36:         convomf
## 37:   UspehPetrovic
## 38:        DaAnunna
## 39: SvetolikPopadic
## 40:    dijalekticar
##             PROFILE
##                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##  1:                                                                                                                                                                                                                                                             <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
##  2:                                                                                                                                                                                                               renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
##  3:                                                                                                                                                                                                                                  <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
##  4:                                                                                                                                                                                                                                                                do i make u nervous? \242<U+FE0F> https://t.co/PfB1yBrTrz
##  5:                                                                                                                                                                                                                                                                               i love you lizz https://t.co/sbXNd9QejH
##  6:                                                                                                                                                                                                                                                                        basta i love u both :< https://t.co/u0KPlzwhSH
##  7:                                                                                                                                                                                                                                                                                                    i love frank ocean
##  8:                                                        Hoću li u pakao jer čitav dan umirem od smijeha na priču o gospođi koja je uspjela nakon milionite vještačke u Pragu da zatrudni i rodi sinčića pa nedavno na nekom rođendanu rekla A VIDI KAKAV JE NEMIRAN, ENO GA OPET JE PAO A JOŠ GA NI OTPLATILI NISMO <3
##  9:                                                                                                                                                                                                                                                                                                       I love you ARMY
## 10:                                                                                                                                                                                                                                                      i love you jeonghan ¦<U+FE0E> @pledis_17 https://t.co/hfilqActNZ
## 11:                                  inače doživjela sam pravo renesansu u bajnoj Opatiji (Thalassoterapija) i prije 4 dana učinila prve samostalne korake nakon 7 godina Bivanja u okovima svog tijela smrznuta i zaleđena od emocionalnih šokova. sada hodam kao beba ali znate bebe uskoro ću… https://t.co/gatgYSOoWz
## 12:                                                                                                                                                                                                                                                              i wanna bop jungkook’s cute nose https://t.co/CM20bO8zIJ
## 13:                                                                                                                                                                                                                                                                      "i love jennie kim but-" https://t.co/lO3kma1moY
## 14:                                                                                                                                                                                                                                                         Shhh… i love u ¦<U+FE0E> #enhypen_jay https://t.co/EGjaOuddPw
## 15:                                                                                                                                                                                                                                                                                 i am OBSESSED https://t.co/AjDhmprdJ7
## 16:                                                                                                                                                                                                                Imamo odgovor: #Jin! #MakeNightsEpic uz novi #GalaxyS22 i Nightography kameru. https://t.co/B76ccFM2Lv
## 17:                                                                                                                                                                                          Svo zlo i psihopatija Aleksandra Vučića se vidi u rečenici: Tamo su ljudi još siromašniji i tamo sam još više glasova dobio!
## 18:                                                                                                                                                                             Budem ljuta na ljude zbog odnosa prema meni, a onda shvatim da sam sve, ali sve dozvolila. Prećutala, odobrila, preskočila. I tako redom.
## 19:                               S kime ste se vi vucarali kad ste imali 20 g.? Ja s jednom Zerinom, dušu mi je izjela ta osoba iznutra kao termit. Poslije se udala za nekakvog IFOR-ovca i skrasila se u Švedskoj kraljevini. Zerina, ako čitaš ovo, vrati mi dupli CD Electric Ladyland od Hendrixa kriminogena osobo
## 20:                                                          Moj dragi drug, citavog zivota, moj komsija, njegova djeca, njegova Asima, svi mi … u najtezoj boli… otosao nam je NAJVECI I NAJBOLJI SARAJLIJA…na slici, nakon sto smo Ivica Saric i ja otpjevali ..Konjuh planinom… njemu u cast.. https://t.co/7sv4MK6cOv
## 21:                                                                                                                                                                                                                                                                  Dobrodošao Rio Tinto i doviđenja EPS.\nZa početak...
## 22:                                                                                                           joj bože ulazi čovjek u malu kafeteriju i traži neki alkohol i barista mu kaže "ne držimo alkohol gospodine samo kafu" i čovjek stoji u nevjerici too stunned to speak i na kraju ispusti neki krik i izađe
## 23:                                                                                                                                                                                                                                      Narod koji odbija besplatne udzbenike zasluzuje vucica i njevovih 40 razbojnika.
## 24:                                                                                                                                                                                                                                                                             <U+0001F430><U+0001F4AD>\n\n-‘i love you’
## 25:                                                                                                                                                                                                                                          bože ali nindža ratnici i ona dva luda voditelja obilježili moje djetinjstvo
## 26:                                                                                                                                                                                                                                                                           i love u 1 INT anne https://t.co/vKzQcy0J4o
## 27: Imala sam kolegu koji me je svakog dana vozio na posao, a imao neku dosadnu ženu. Jedno jutro sam ga pozvala gore jer sam se kao uspavala, obukla tange i prošetala se ispred njega. Odmah se zaljubio! <U+0001F970>\nTo je moj sadašnji muž.\nLjubav uvijek pobjedi <U+0001F49E><U+0001F493><U+0001F49C><U+0001F970>
## 28:                                                                                            suna: so i have a crush on a miya\nginjima: oh, i didn’t know you liked guys. which miya?\nsuna: the oldest one\nginjima: atsumu?!\nsuna: no\nsuna: miya…san\nginjima:\nginjima: <U+0001F9CD><U+200D>><U+FE0F><U+0001F6AA>
## 29:                                                                                                                                                                                                                                                   i love you jeonghan <U+0001F5A4> @pledis_17 https://t.co/edxEUxtLec
## 30:                                                                                                                                      Nisam ni znao da uz Frica i žene, sa mnom u stanu živi još dvadeset šestoro nepoznatih ljudi. Dobro dok Vodovod ne provali pa mi za sve nas naplati vodu https://t.co/VueJQKKMwc
## 31:                                                                                                                                Kod muškaraca su najbitnija široka ramena, biceps, triceps, kvadriceps, falus i da mu nije mala plata.\nEventualno da je duhovit i elokventan.\nI da ne živi s majkom.\nI da ima auto.
## 32:                                                     …( kad svi pomremo, ko ce mladim objasniti da smo se nekad iskreno voljeli ..!??)..danas nas je napustio Pero Skansi…LIPI MOJ DALMATA… kako ga je zvao Otavio Missoni… veliki igrac i veliki trener… priatelj oduvjek… tuga… neizmjerna.. https://t.co/rkE8rXlt8G
## 33:                                                                                                                                                                                                                                                                         i love u <U+0001F979> https://t.co/KzCfxMB6CQ
## 34:                                                                                                                                                                                                                                                                                 Treba sve odjebati i ići ovce čuvati.
## 35:                                                                                                                                                                                                                                                                           ja i svaki lik ikad https://t.co/G3ihjV4ko3
## 36:                                                                                                                                                                                                                                                                                           <U+0001F499> i love you, A.
## 37:                                                                                                                                                                                                                                          I naklon za Pavla Grbovića jer je uzvratio udarce i nije samo čekao žrtving!
## 38:                                                                                                                                                                                                                                                                           ja i svaki lik ikad https://t.co/G3ihjV4ko3
## 39:                                                                                                                                                                                                                                                            Marinika Tepić napušta Đilasa i prelazi kod Ponoša.\nOpaaa
## 40:                                                                                                                                                                                                Narod koga možeš ubijediti da Njemačkoj fali brašna i kruha, a Italiji maslinovog ulja, možeš mu raditi sve. Doslovno!
##                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##     FAVORITE_COUNT
##  1:          15087
##  2:          13056
##  3:          10544
##  4:          10318
##  5:           8062
##  6:           7442
##  7:           6047
##  8:           5818
##  9:           5111
## 10:           4348
## 11:           3854
## 12:           3370
## 13:           3315
## 14:           3138
## 15:           2879
## 16:           2690
## 17:           2675
## 18:           2450
## 19:           2009
## 20:           1946
## 21:           1675
## 22:           1641
## 23:           1612
## 24:           1603
## 25:           1598
## 26:           1586
## 27:           1501
## 28:           1459
## 29:           1368
## 30:           1368
## 31:           1327
## 32:           1280
## 33:           1223
## 34:           1217
## 35:           1205
## 36:           1175
## 37:           1086
## 38:           1055
## 39:           1054
## 40:           1048
##     FAVORITE_COUNT
##                                                                URL
##  1:         https://twitter.com/convomf/status/1506662084830179330
##  2:       https://twitter.com/HRJVISUAL/status/1510188863175634944
##  3:         https://twitter.com/convomf/status/1506464608478851073
##  4:       https://twitter.com/waifuumia/status/1512555707677265922
##  5: https://twitter.com/madeoflavenderr/status/1506377255550861312
##  6:     https://twitter.com/lisasuprmcy/status/1509070998074667008
##  7:    https://twitter.com/crying2frank/status/1510810844526829568
##  8:          https://twitter.com/MejMin/status/1513641129274757128
##  9: https://twitter.com/BTS_twt_TAEHYNG/status/1519351095226937344
## 10:   https://twitter.com/jeonghanszone/status/1505537038858158082
## 11:     https://twitter.com/AndjaMarics/status/1518316761980018688
## 12:      https://twitter.com/JKSIMPRINT/status/1511584224255496202
## 13:        https://twitter.com/G0THRUBY/status/1508472579119587331
## 14:   https://twitter.com/archivesofjay/status/1506634795220185095
## 15:    https://twitter.com/baIenciagajk/status/1503244320991707137
## 16:  https://twitter.com/SamsungCroatia/status/1514893978554056711
## 17:   https://twitter.com/UspehPetrovic/status/1510748548895236099
## 18:       https://twitter.com/branchica/status/1511069916270669824
## 19:      https://twitter.com/Dzoni77777/status/1518233886978764800
## 20:  https://twitter.com/BogdanTanjevic/status/1520757104046399488
## 21: https://twitter.com/DejanVuckovic12/status/1510866307050356738
## 22:     https://twitter.com/munkov_krik/status/1503118989379133449
## 23:    https://twitter.com/KOJO54579153/status/1516639873737842691
## 24:     https://twitter.com/ex9thoughts/status/1509571590903767045
## 25:         https://twitter.com/ttaasam/status/1508920538160316420
## 26:         https://twitter.com/raebits/status/1508018240382939136
## 27:    https://twitter.com/KittyFormanA/status/1512040722815410181
## 28:    https://twitter.com/kunimisbangs/status/1504049392587886594
## 29:   https://twitter.com/jeonghanszone/status/1507294564763066370
## 30:      https://twitter.com/_Apostle13/status/1507689887453040641
## 31: https://twitter.com/MnogoNebitanLik/status/1503451092461445128
## 32:  https://twitter.com/BogdanTanjevic/status/1511031465525583878
## 33:       https://twitter.com/riajentle/status/1511352634900611077
## 34:       https://twitter.com/branchica/status/1505483383799005193
## 35:        https://twitter.com/DaAnunna/status/1516437634129842191
## 36:         https://twitter.com/convomf/status/1505844167775518721
## 37:   https://twitter.com/UspehPetrovic/status/1510578782293446660
## 38:        https://twitter.com/DaAnunna/status/1516437634129842191
## 39: https://twitter.com/SvetolikPopadic/status/1516071622762975232
## 40:    https://twitter.com/dijalekticar/status/1508118722715734023
##                                                                URL
```

```r
# most appreciated
CTW %>% 
  select(PROFILE, FULL_TEXT, RETWEET_COUNT,URL) %>%
  arrange(desc(RETWEET_COUNT))  %>% 
  head(40)
```

```
##             PROFILE
##  1:       HRJVISUAL
##  2:     lisasuprmcy
##  3:    crying2frank
##  4: madeoflavenderr
##  5: MayaSar77291463
##  6:          BEG97N
##  7:      googiemilk
##  8:        kvluvs_7
##  9:   jeonghanszone
## 10: nemanja_stankov
## 11: SneanaKrstovic1
## 12:      negde_tamo
## 13:      negde_tamo
## 14: SneanaKrstovic1
## 15:       vojinovi2
## 16:   MajaVojinovi1
## 17:         convomf
## 18:         convomf
## 19:       Nevenaa84
## 20:       Nevenaa84
## 21:  SamsungCroatia
## 22:   archivesofjay
## 23:      negde_tamo
## 24: SneanaKrstovic1
## 25:   markovic_dana
## 26:        G0THRUBY
## 27:   markovic_dana
## 28:   markovic_dana
## 29:    VucicaMilica
## 30:      NevenaBo11
## 31:   anaastasija12
## 32:       Lavica015
## 33:  Stamenkovic037
## 34:    ivanasabo_10
## 35:      NevenaBo11
## 36:       Lavica015
## 37:   anaastasija12
## 38:  Stamenkovic037
## 39:      eugenija75
## 40:      BeloNatasa
##             PROFILE
##                                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##  1:                                                                                                                                                                                                                               renjun, i wanna cry<U+0001F97A><U+0001F97A><U+0001F62D><U+0001F62D><U+0001F49B> https://t.co/cDbcgb9zzd
##  2:                                                                                                                                                                                                                                                                                        basta i love u both :< https://t.co/u0KPlzwhSH
##  3:                                                                                                                                                                                                                                                                                                                    i love frank ocean
##  4:                                                                                                                                                                                                                                                                                               i love you lizz https://t.co/sbXNd9QejH
##  5:                                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##  6:                                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##  7:                                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##  8:                                                                                                                                                                                                    RT @rmkkyu <U+0001F464>: what kind of pizza do you like?\n<U+0001F430>: i like pepperoni pizza. thank you~ https://t.co/XTJUeQinwN
##  9:                                                                                                                                                                                                                                                                      i love you jeonghan ¦<U+FE0E> @pledis_17 https://t.co/hfilqActNZ
## 10:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 11:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 12:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 13:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 14:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 15:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 16:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 17:                                                                                                                                                                                                                                                  <U+0001F499> balasan kata " i love you" selain "i love you too, i love more" apa ya?
## 18:                                                                                                                                                                                                                                                                             <U+0001F499> i hv u crush on uuuu https://t.co/mSiupQabCh
## 19:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 20:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 21:                                                                                                                                                                                                                                Imamo odgovor: #Jin! #MakeNightsEpic uz novi #GalaxyS22 i Nightography kameru. https://t.co/B76ccFM2Lv
## 22:                                                                                                                                                                                                                                                                         Shhh… i love u ¦<U+FE0E> #enhypen_jay https://t.co/EGjaOuddPw
## 23:      RT @LaziDetektor HRVATSKI MINISTAR NIJE OČEKIVAO OVAKVU NADMOĆ VUČIĆA!\n\nHrvatski N1: "Niko ne sumnja u Vučićevu pobjedu, to je očekivano! Čestitke stižu iz usta predsjednika Milanovića kao i ministra vanjskih poslova Gordana Grlića Radmana koji je rekao da NIJE OČEKIVAO TAKO NADMOĆNU POBJEDU!" https://t.co/Epb12yMUGK
## 24:      RT @LaziDetektor HRVATSKI MINISTAR NIJE OČEKIVAO OVAKVU NADMOĆ VUČIĆA!\n\nHrvatski N1: "Niko ne sumnja u Vučićevu pobjedu, to je očekivano! Čestitke stižu iz usta predsjednika Milanovića kao i ministra vanjskih poslova Gordana Grlića Radmana koji je rekao da NIJE OČEKIVAO TAKO NADMOĆNU POBJEDU!" https://t.co/Epb12yMUGK
## 25:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 26:                                                                                                                                                                                                                                                                                      "i love jennie kim but-" https://t.co/lO3kma1moY
## 27: RT @LaziDetektor PUKAO N1 BALON: PODRŽAVATE KORPORATIVNOG KANDIDATA!\n\nBojana Novaković - glumica i ekološka aktivistkinja:\n\n"N1, GDE STE VI? Nemate m**a da snimite kandidaturu Biljane Stojković! Koristite model CNN-a, podržavate samo korporativne kandidate! MI VAM TREBAMO SAMO ZA SLIKE SA ULICA!" https://t.co/25cjW5ZCtI
## 28:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 29:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 30:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 31:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 32:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 33:         RT @LaziDetektor UVEK ME OBRADUJU NAPADI U HRVATSKIM MEDIJIMA!\n\nVučić o napadima hrvatskih medija na njega:\n\n"Uvek me obraduju napadi na mene u hrvatskim medijima! Samo gledajte hrvatske medije i odlično ćete znati za koga da glasate! O kompleksima i frustracijama ne bih ni da govorim..." https://t.co/d8ZVcWpgPk
## 34:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 35:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 36:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 37:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 38:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 39:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
## 40:                       RT @LaziDetektor OČEKIVANO JE DA HRVATSKI MEDIJI PODRŽAVAJU PONOŠA!\n\nAleksandar Vučić o podršci Đilasovom Ponošu koja dolazi iz hrvatskih medija:\n\n"To je sve razumljivo i to ljudi treba da vide! Sve je to u skladu sa onim što je očekivano i sve to ljudi vrlo dobro shvataju!" https://t.co/q2Xh3zuQiJ
##                                                                                                                                                                                                                                                                                                                                 FULL_TEXT
##     RETWEET_COUNT
##  1:          4121
##  2:          1691
##  3:          1531
##  4:          1434
##  5:          1369
##  6:          1172
##  7:          1172
##  8:          1172
##  9:           969
## 10:           921
## 11:           863
## 12:           856
## 13:           831
## 14:           831
## 15:           792
## 16:           792
## 17:           780
## 18:           734
## 19:           720
## 20:           706
## 21:           705
## 22:           692
## 23:           649
## 24:           649
## 25:           642
## 26:           628
## 27:           617
## 28:           615
## 29:           594
## 30:           591
## 31:           591
## 32:           591
## 33:           591
## 34:           557
## 35:           557
## 36:           557
## 37:           557
## 38:           557
## 39:           557
## 40:           557
##     RETWEET_COUNT
##                                                                URL
##  1:       https://twitter.com/HRJVISUAL/status/1510188863175634944
##  2:     https://twitter.com/lisasuprmcy/status/1509070998074667008
##  3:    https://twitter.com/crying2frank/status/1510810844526829568
##  4: https://twitter.com/madeoflavenderr/status/1506377255550861312
##  5: https://twitter.com/MayaSar77291463/status/1510281356118355971
##  6:          https://twitter.com/BEG97N/status/1510269828963074054
##  7:      https://twitter.com/googiemilk/status/1510269574784385024
##  8:        https://twitter.com/kvluvs_7/status/1510269571131113478
##  9:   https://twitter.com/jeonghanszone/status/1505537038858158082
## 10: https://twitter.com/nemanja_stankov/status/1511034412737208325
## 11: https://twitter.com/SneanaKrstovic1/status/1510898146372636672
## 12:      https://twitter.com/negde_tamo/status/1510898566671310851
## 13:      https://twitter.com/negde_tamo/status/1510898486107152385
## 14: https://twitter.com/SneanaKrstovic1/status/1510898070577463299
## 15:       https://twitter.com/vojinovi2/status/1510864396909789186
## 16:   https://twitter.com/MajaVojinovi1/status/1510864257516253184
## 17:         https://twitter.com/convomf/status/1506464608478851073
## 18:         https://twitter.com/convomf/status/1506662084830179330
## 19:       https://twitter.com/Nevenaa84/status/1510736984129556480
## 20:       https://twitter.com/Nevenaa84/status/1510736771067392009
## 21:  https://twitter.com/SamsungCroatia/status/1514893978554056711
## 22:   https://twitter.com/archivesofjay/status/1506634795220185095
## 23:      https://twitter.com/negde_tamo/status/1511239550508703748
## 24: https://twitter.com/SneanaKrstovic1/status/1511238477794455558
## 25:   https://twitter.com/markovic_dana/status/1510706251042377738
## 26:        https://twitter.com/G0THRUBY/status/1508472579119587331
## 27:   https://twitter.com/markovic_dana/status/1503838895183740928
## 28:   https://twitter.com/markovic_dana/status/1510706556593188874
## 29:    https://twitter.com/VucicaMilica/status/1510681864117305349
## 30:      https://twitter.com/NevenaBo11/status/1510679905712934921
## 31:   https://twitter.com/anaastasija12/status/1510679421497356292
## 32:       https://twitter.com/Lavica015/status/1510679386697121802
## 33:  https://twitter.com/Stamenkovic037/status/1510677338274607107
## 34:    https://twitter.com/ivanasabo_10/status/1510680462829801483
## 35:      https://twitter.com/NevenaBo11/status/1510680408228257792
## 36:       https://twitter.com/Lavica015/status/1510679511263850505
## 37:   https://twitter.com/anaastasija12/status/1510678964473376769
## 38:  https://twitter.com/Stamenkovic037/status/1510677894938439683
## 39:      https://twitter.com/eugenija75/status/1510638284745158662
## 40:      https://twitter.com/BeloNatasa/status/1510638188846538762
##                                                                URL
```


```r
posts[,.N,DATE][order(-DATE)]

all <- posts[DATE > "2021-12-01",]


# library(extrafont)
# loadfonts()
# library(xkcd)
# font_import(pattern="[H/h]umor")
# loadfonts()
# windowsFonts(Times=windowsFont("Times New Roman"))
# library(ggtext)
# library(showtext)
# showtext_auto()
# 
# font_add_google('Roboto')

all[SOURCE_TYPE != "comment",.N,SOURCE_TYPE][,.(SOURCE_TYPE,N = round(N/1000,2))] %>% 
  ggplot(., aes(reorder(SOURCE_TYPE,N), N ,fill=as.factor(SOURCE_TYPE))) + 
  geom_bar(stat = "identity") + 
  scale_fill_grey() +
  theme(legend.position="none") +
  coord_flip() +
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
panel.background = element_blank(), axis.line = element_line(colour = "black"),
text = element_text(family = "serif")) +
  ylab("Tisuca medijskih objva") + 
  xlab("") +
  scale_y_continuous(labels = scales::comma, breaks = c(50,150,300,550,1200)) + 
  labs(title = "Aktivnost na društvenim medijma u Hrvatskoj",
              subtitle = "Cjelokupni medijski prostor u razdoblju 6 mjeseci",
              caption = "Podatci: Mediatoolkit | Izradio: Lukos")
```

# Check forum activity 


```r
# read in data
forum <- read.csv2( "C:/Users/Lukas/OneDrive/Desktop/forum.csv")
forum <- as.data.table(forum)


forum[,.N, TITLE][order(-N)] %>% head(50)
```

```
##                                                                          TITLE
##  1:                                                                       <NA>
##  2:                                                        Ofsajd (chat topic)
##  3:              BiH - Bosna i Hercegovina X - danas i sutra, bez ratnih priča
##  4:                    Zoran Milanović, predsjednik Republike Hrvatske vol. IV
##  5:                          Influencerice vol. 14: Pun mi je lungić kobasice!
##  6:  Tomislav Tomašević, gradonačelnik Grada Zagreba III-pročitati uvodni post
##  7:                                   Trač vol. 28 - Kurvo, izdala si ratnika!
##  8:                                                     Valentina Kordić Tijan
##  9:                             Potraga za razlogom ili teorije zavjere vol. 8
## 10:             Influencerice vol 13.: Onaj mali reve kao da će umrit isti čas
## 11:                                      Modne kritike za hrabre kokoši vol. 5
## 12:                                                              Yargi / Osuda
## 13:                             Potraga za razlogom ili teorije zavjere vol. 7
## 14:                                 Bili kutak - štorija 39. - Pirmyn baltieji
## 15: Isključivo za mene i ljude kojima ja dopustim da pišu. First Blood Part II
## 16:                                                                   A B Chat
## 17:                                                      Trudnice zbor Vol. 49
## 18:                       Lounge Bar Psycho.Logy - klub potpore i chat vol. XV
## 19:                             Potraga za razlogom ili teorije zavjere vol. 9
## 20:                                         Ruska invazija na Ukrajinu, 3. dio
## 21:                                   Trač vol. 27 - Bog prašta, Kokošinjac ne
## 22:                                                          Stanje na ratištu
## 23:      Budućnost nadzvučne komponente HRZ XI (ili: Imamo ugovor! A što sad?)
## 24:                                         Ruska invazija na Ukrajinu, 4. dio
## 25:                               OGLASI - ideje, savjeti, iskustva, komentari
## 26:               Forumski Avaz - komentiranje medijskih aktualnosti volume II
## 27:                    Rimac Automobili, Greyp Bikes & Project 3 Mobility (VI)
## 28:                                            Električna i hibridna vozila II
## 29:                           Proljeće se budi, svaki US-ovac metnut se trudi!
## 30:                                                         1. HNL 2021./2022.
## 31:                                                    Kad jaganjci utihnu III
## 32:                                                    Teşkilat (Organizacija)
## 33:                                           Anksioznost - opća tema (Vol. V)
## 34:                               EU fit for 55 plan - smanjivanje emisije CO2
## 35:                           Hollywood Buzz (tračeraj o svjetskim celebovima)
## 36:             Andrej Plenković - predsjednik HDZ-a i predsjednik Vlade RH IV
## 37:                                                   Bili kutak - štorija 40.
## 38:                                              Alpsko skijanje - 2021./2022.
## 39:                                                  Novogradnja (ZG) - vol. 3
## 40:                                                            Sankcije Rusiji
## 41:                                                     Korona razgovori vol.2
## 42:                SRB - Republika Srbija XV - danas i sutra, bez ratnih priča
## 43:                                              Gospodin Savršeni (RTL 2022.)
## 44:                                                            'ću da ti gatam
## 45:                                         Nocna smjena (a cesto i dnevna) 2.
## 46:                                      Modne kritike za hrabre kokoši vol. 4
## 47:                                                   Kokoši rješavaju zločine
## 48:                                             I mačke se množe, a US ne može
## 49:                                                               NNNI, vol. 4
## 50:                                                      Divljaci u prometu IV
##                                                                          TITLE
##          N
##  1: 264189
##  2:   3504
##  3:   2953
##  4:   2490
##  5:   2103
##  6:   2098
##  7:   2087
##  8:   2082
##  9:   2011
## 10:   1907
## 11:   1898
## 12:   1759
## 13:   1724
## 14:   1708
## 15:   1593
## 16:   1588
## 17:   1580
## 18:   1574
## 19:   1573
## 20:   1524
## 21:   1489
## 22:   1467
## 23:   1360
## 24:   1261
## 25:   1252
## 26:   1239
## 27:   1237
## 28:   1222
## 29:   1219
## 30:   1218
## 31:   1181
## 32:   1174
## 33:   1161
## 34:   1116
## 35:   1116
## 36:   1103
## 37:   1078
## 38:   1074
## 39:   1056
## 40:   1048
## 41:   1035
## 42:   1026
## 43:   1022
## 44:    995
## 45:    982
## 46:    980
## 47:    961
## 48:    961
## 49:    955
## 50:    949
##          N
```

```r
# forum[TITLE == "Zoran Milanović, predsjednik Republike Hrvatske vol. IV",] %>% View()
```













