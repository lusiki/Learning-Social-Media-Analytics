---
title: "Learning Social Media Analytics"
# subtitle: "<html><div style='float:left'></div><hr color='#EB811B' size=1px width=796px></html>"
subtitle: "Lecture 7: Text Analysis"
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









# OUTLINE
<br>
<br>
<br>
- DATA IMPORT 
<br>
<br>
- PREPARE TEXT DATA
<br>
<br>
- 
<br>
<br>



# DATA IMPORT

#### ARTICLE DATA


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
```


```r
# some basic data overview
options(digits = 2)
# data size 
dim(posts)
```

```
## [1] 2512756      49
```

```r
# time span
range(posts$DATETIME)
```

```
## [1] "2021-11-07 00:00:00 CET"  "2022-04-19 23:59:59 CEST"
```

```r
# descriptives
posts[,.N,SOURCE_TYPE][order(-N)]
```

```
##    SOURCE_TYPE       N
## 1:         web 1157720
## 2:       forum  561580
## 3:    facebook  278453
## 4:      reddit  156083
## 5:     youtube  128433
## 6:     twitter  109498
## 7:     comment   67185
## 8:   instagram   53804
```

```r
# how many letters in a title
posts[,
       .(Avg = mean(nchar(TITLE), na.rm = T),
         STD = sd(nchar(TITLE), na.rm = T),
         min = min(nchar(TITLE), na.rm = T),
         max = max(nchar(TITLE), na.rm = T)),
      SOURCE_TYPE][order(-Avg),]
```

```
##    SOURCE_TYPE Avg   STD min  max
## 1:   instagram 305  75.9   5  350
## 2:     twitter 175 109.2   4 1182
## 3:      reddit 150 123.5   1  350
## 4:    facebook 128  39.1   4  160
## 5:         web  72  32.1   1  160
## 6:     youtube  52  23.7   1  100
## 7:       forum  34  18.5   1  177
## 8:     comment  22   1.9  22   72
```

```r
# how many letters in a text
posts[,
       .(Avg = mean(nchar(FULL_TEXT)),
         STD = sd(nchar(FULL_TEXT)),
         min = min(nchar(FULL_TEXT)),
         max = max(nchar(FULL_TEXT))),
      SOURCE_TYPE][order(-Avg),]
```

```
##    SOURCE_TYPE  Avg  STD min   max
## 1:         web 2442 2453   7 32003
## 2:     youtube  852 1057   3  5565
## 3:       forum  584  668   5 32003
## 4:   instagram  555  363   5  2250
## 5:    facebook  318  446   4 32002
## 6:     comment  283  262  15  4016
## 7:     twitter  175  109   4  1182
## 8:      reddit   NA   NA  NA    NA
```

#### LEXICON DATA


```r
# read in lexicons
CroSentilex_n <- read.delim("C:/Users/Lukas/Dropbox/Mislav@Luka/crosentilex-negatives.txt",
                                   header = FALSE,
                                   sep = " ",
                                   stringsAsFactors = FALSE,
                                   fileEncoding = "UTF-8")  %>%
                   rename(word = "V1", sentiment = "V2" ) %>%
                   mutate(brija = "NEG")
 
CroSentilex_p  <- read.delim("C:/Users/Lukas/Dropbox/Mislav@Luka/crosentilex-positives.txt",
                                   header = FALSE,
                                   sep = " ",
                                   stringsAsFactors = FALSE,
                                   fileEncoding = "UTF-8") %>%
                    rename(word = "V1", sentiment = "V2" ) %>%
                    mutate(brija = "POZ")
 
Crosentilex_sve <- rbind(setDT(CroSentilex_n), setDT(CroSentilex_p))
# check lexicon data 
head(sample_n(Crosentilex_sve,1000),15)
```

```
##               word sentiment brija
##  1:       salvador      0.46   POZ
##  2:           očit      0.25   POZ
##  3:       rusedski      0.27   POZ
##  4:       gorostas      0.26   POZ
##  5:         pintar      0.10   POZ
##  6:    mezzosopran      0.12   NEG
##  7:      izokrenut      0.36   NEG
##  8:       krajolik      0.48   NEG
##  9:      izrastati      0.48   NEG
## 10:    provocirati      0.64   NEG
## 11:       žlijezda      0.16   POZ
## 12:     ograničiti      0.50   POZ
## 13:       mečiarov      0.20   NEG
## 14:     povijestan      0.23   NEG
## 15: rekapitulacija      0.19   POZ
```

```r
CroSentilex_Gold  <- read.delim2("C:/Users/Lukas/Dropbox/Mislav@Luka/gs-sentiment-annotations.txt",
                                 header = FALSE,
                                 sep = " ",
                                 stringsAsFactors = FALSE) %>%
                    rename(word = "V1", sentiment = "V2" ) 
 Encoding(CroSentilex_Gold$word) <- "UTF-8"
 CroSentilex_Gold[1,1] <- "dati"
 CroSentilex_Gold$sentiment <- str_replace(CroSentilex_Gold$sentiment , "-", "1")
 CroSentilex_Gold$sentiment <- str_replace(CroSentilex_Gold$sentiment , "\\+", "2")
 CroSentilex_Gold$sentiment <- as.numeric(unlist(CroSentilex_Gold$sentiment))
# check lexicon data 
head(sample_n(CroSentilex_Gold,100),15)
```

```
##          word sentiment
## 1    natječaj         0
## 2   trgovački         0
## 3     opisati         0
## 4      linija         0
## 5  priznavati         0
## 6     poslati         0
## 7   okolišati         1
## 8     vladati         0
## 9     valjati         2
## 10   riješiti         2
## 11   opravdan         2
## 12 napomenuti         0
## 13  istodoban         0
## 14    tribina         0
## 15   presudan         0
```

```r
# create stop words
stopwords_cro <- get_stopwords(language = "hr", source = "stopwords-iso")
# check stopwords data
head(sample_n(stopwords_cro,100),15)
```

```
## # A tibble: 15 x 2
##    word   lexicon      
##    <chr>  <chr>        
##  1 bih    stopwords-iso
##  2 od     stopwords-iso
##  3 našeg  stopwords-iso
##  4 su     stopwords-iso
##  5 joj    stopwords-iso
##  6 sa     stopwords-iso
##  7 nad    stopwords-iso
##  8 nećete stopwords-iso
##  9 sebe   stopwords-iso
## 10 što    stopwords-iso
## 11 iz     stopwords-iso
## 12 jim    stopwords-iso
## 13 već    stopwords-iso
## 14 li     stopwords-iso
## 15 tu     stopwords-iso
```

```r
# extend stop words
my_stop_words <- tibble(
  word = c(
    "jedan","mjera", "može", "mogu", "kad", "sada", "treba", "ima", "osoba",
    "e","prvi", "dva","dvije","drugi",
    "tri","treći","pet","kod",
    "ove","ova",  "ovo","bez", "kod",
    "evo","oko",  "om", "ek",
    "mil","tko","šest", "sedam",
    "osam",   "čim", "zbog",
    "prema", "dok","zato", "koji", 
    "im", "čak","među", "tek",
    "koliko", "tko","kod","poput", 
    "baš", "dakle", "osim", "svih", 
    "svoju", "odnosno", "gdje",
    "kojoj", "ovi", "toga",
     "ubera", "vozača", "hrvatskoj", "usluge", "godine", "više", "taksi", "taxi", "taksija", "taksija", "kaže", "rekao", "19"," aee", "ae"
  ),
  lexicon = "lux"
)
stop_corpus <- my_stop_words %>%
  bind_rows(stopwords_cro)
# check stopwords data
head(sample_n(stop_corpus,100),15)
```

```
## # A tibble: 15 x 2
##    word    lexicon      
##    <chr>   <chr>        
##  1 vaša    stopwords-iso
##  2 svom    stopwords-iso
##  3 kad     lux          
##  4 tome    stopwords-iso
##  5 naš     stopwords-iso
##  6 njima   stopwords-iso
##  7 jedna   stopwords-iso
##  8 pa      stopwords-iso
##  9 iako    stopwords-iso
## 10 smo     stopwords-iso
## 11 odnosno lux          
## 12 nju     stopwords-iso
## 13 koliko  lux          
## 14 taksija lux          
## 15 vozača  lux
```


# PREPARE TEXT DATA

### CHECK DATA AND TOKENIZE 


```r
#web <- posts[SOURCE_TYPE == "web",]
# fb <- posts[SOURCE_TYPE == "facebook",]
# youtube <- posts[SOURCE_TYPE == "youtube",]
instagram <- posts[SOURCE_TYPE == "instagram",]
rm(posts)
# twitter <- posts[SOURCE_TYPE == "twitter",]
# reddit <- posts[SOURCE_TYPE == "reddit",]
# forum <-  posts[SOURCE_TYPE == "forum",]
# comment <-  posts[SOURCE_TYPE == "comment",]
# influencers by ACTIVITY
printMD(instagram[,.N,FROM][order(-N)], big.mark=",")
```


-----------------------------------------
              FROM                  N    
-------------------------------- --------
         anonymous_user           46,282 

       beautypharmacy_hr           625   

          jutarnji.hr              544   

         vecernji.list             489   

           dnevnikhr               389   

         tvojaljekarna             273   

     obuca.metro.montenegro        165   

          telemach.hr              140   

           24sata.hr               129   

         maxfactor_bih             111   

         optima.telekom            109   

            rkzagreb               106   

        obuca.metro_bih             95   

         apoteka_monis              83   

        oriflame_croatia            76   

             rtl_hr                 76   

           farmasihr                72   

         moja_farmacia              72   

        mali_muzej_cuda             71   

          pharmacy_bcc              69   

         projekt_ilica              66   

         ljekarna.pablo             63   

         ceravehrvatska             63   

       ljekarnaonline.hr            56   

           wwf_adria                54   

          elladvornik               53   

          avonhrvatska              52   

            skitnica                51   

         a1hrvatskalife             50   

         more_less_ines             50   

           farmasibh                48   

          superapoteka              48   

       ninasliskovicgoles           47   

        varteks.fashion             47   

           mojbonbon                47   

            vindija                 45   

         eljekarna24.hr             45   

    lucija_lugomer_official         40   

        ljupka_tanevska             40   

              hana                  40   

     ljekarneprimafarmacia          40   

     ljekarne_vase_zdravlje         39   

       cromaris_hrvatska            37   

        apoteke_b_pharm             37   

          mojaljekarna              36   

        ljekarna_marusic            36   

         korana_marovic             35   

        apoteka_anapharm            35   

          slakipalaki               34   

          miffyandrea               33   

           cerave_ba                33   

             iwaiva                 33   

         andreaandrassy             32   

             avonba                 31   

        marijanabatinic             31   

       ave_dulcis_tamara            30   

         domacica.sanja             30   

        hrvatski.telekom            30   

        jelenamarinovic             30   

         nevenarendeli              30   

          obuca.metro               29   

        mirjana_mikulec             29   

       maxfactor_hrvatska           29   

           anaradisic               28   

       podravka_hrvatska            28   

        mario_petrekovic            28   

          ida_prester               28   

           jglobitelj               27   

         pliva_hrvatska             27   

           farmasirs                27   

          lidijalesic               26   

            nivea_hr                26   

        slavica.de.jong             26   

            ormarija                25   

       ljekarnajelichrgic           25   

           teamiskon                25   

         cvit_likarije              24   

           sarashine_               23   

          slavkosobin               23   

   oriflamebosnaihercegovina        23   

        danijeladvornik             22   

          wolt.croatia              22   

          aboutyou_hr               22   

           rtl.danas                22   

          stitnjaca.hr              22   

          nanulananula              21   

          inacrnagora               21   

         ljekarnatalan              21   

         dukat_hrvatska             20   

          lukanizetic               20   

          boltfood_hr               20   

          josipa_lisac              20   

          putoholicari              20   

          ecijaojdanic              20   

         lanatheklingor             20   

       dashofblue_makeup            19   

         ornelavistica              19   

          petrakurtela              18   

        mercator_centar             18   

          marko.tolja               18   

          zoransprajc               17   

           safariduha               17   

            paulasik                17   

           sasalozar                17   

 savez_elektronskih_sportova_cg     17   

          telegram.hr               17   

        prima_namjestaj             17   

        mashinthebeauty             17   

         katarinababan              16   

          indiralevak               16   

           balkan_bet               16   

          maremikulic               16   

        vern_university             16   

         bornabutijer5              16   

         pamelaramljak              16   

          renatasopek               16   

           podravkacg               16   

         malajskitapir              15   

            glovo_hr                15   

           tportal.hr               15   

         rene_bitorajac             15   

           ninabljak                14   

       smartphonehrvatska           14   

           boombox.hr               14   

          uvijekgladna              14   

           majasuput                13   

        maxbetkladionice            13   

           august_xvi               13   

           lidlsrbija               13   

        telemachcrnagora            12   

         lilly_drogerie             12   

        sementa_rajhard             12   

         antonijablace              12   

           mr.moncina               12   

           a1hrvatska               12   

         discus70queen              11   

           damirkedzo               11   

           bojana.g.v               11   

         apotekalaurus              11   

       father_of_djordje            11   

          iva___radic               11   

    ljekarnanaglicsostarcic         11   

        mariahs_chambers            11   

          stazisweets               10   

            phama.hr                10   

          suzana_emes               10   

          ceravesrbija              10   

           ideasrbija               10   

             mtelcg                 10   

          drmaxapoteka              10   

          davorgerbus               10   

           markodrcic               9    

             mtelbh                 9    

           mozzartbet               9    

         meridianbet_rs             9    

            severina                9    

          knjazrobert               9    

          matea.miljan              9    

           avonsrbija               9    

         belamakeuplada             9    

           click4chic               9    

          nivescelsius              9    

          ivanablazoti              9    

             lalana                 9    

           lutrija.hr               9    

        llums_cosmetics             9    

          maja_bajamic              9    

       anamarasharmander            8    

            jelka12                 8    

     antonija_stupar_jurkin         8    

        petra_miksikova             8    

         vandawinter148             7    

            combis_                 7    

          gingerellica              7    

          mojaapoteka               7    

          martina_boss              7    

     ericsson_nikola_tesla          7    

          anabacinger               7    

         thecircuitmess             6    

         ivicakostelic              6    

   beslagicenis_jedini_pravi        6    

     tarikfilipovicofficial         6    

         teakravarscan              6    

         oriflamesrbija             6    

         mylittlezagreb             6    

            hardkoo                 6    

           lana_gojak               6    

           ivansaric                6    

          mandis_pharm              5    

           brita0507                5    

          bancaintesa               5    

        doris.stankovic             5    

          tinsedlar88               5    

          eapoteka.rs               5    

       srbotrade_apoteka            5    

         ivana_miseric              5    

           meggle_hr                5    

           emaluketin               5    

       veronika_rosandic            4    

           mirnamaras               4    

           mkm.marina               4    

          erste_banka               4    

        frankaofficial_             4    

         generalisrbija             4    

           chilli_di                4    

           dorica505                4    

           visitsplit               4    

     tomislavgustinfitness          4    

          soccerbet.rs              4    

          daliborpetko              4    

          markogrubnic              4    

         apotekaonlinea             4    

         koranagvozdic              4    

            zanamari                4    

         tatjana_juric              4    

         mario.valentic             4    

         andreafabricc              3    

         minea.official             3    

          yettelsrbija              3    

            narod.hr                3    

         mirna_medstep              3    

       ljekarnelukacinzg            3    

          elegoviceva               3    

       obuca.metro_outlet           3    

            ctelekom                3    

            tojevuk                 3    

            a1srbija                2    

         suddenly_nana              2    

          vitalsrbija               2    

          daniella.ods              2    

         oaza_zdravlja_             2    

        ljupkagojicmikic            2    

          ximenamoral               2    

         apoteka_cvejic             2    

           ritarumora               2    

         croatiacontrol             2    

       erstebankacrnagora           2    

        jankovicapoteka             2    

          thompson_hr               2    

        adrianadurdevic             2    

          elaajerkovic              2    

         anabegictahiri             2    

          marko.mrkic               2    

         stjepanowskaja             2    

          mtstvojsvet               2    

           usa_in_mne               2    

        lucianoplazibat             2    

           amiepetite               1    

        ljekarnepharmad             1    

            postacg                 1    

          gponedotcom               1    

           lorainne7                1    

         thecoolhunter_             1    

       eurobank.direktna            1    

        danijel.subasic             1    

         _rahul_raj_31              1    

            reci.hr                 1    

       galenpharmapoteke            1    

         nosim.varteks              1    

            psidacta                1    

   triglav.osiguranje.srbija        1    

         dejanlovren06              1    

          ljekarna.hr               1    

         bhv_education              1    

          dugandziceva              1    

          marinamamic               1    

          lizzyvdligt               1    

        lipalek_apoteka             1    

         debela.barbara             1    

         dorinlucissima             1    

          centar.cesi               1    

           lukabulic                1    

           altabanka                1    

        viktorija.radja             1    

          anna_soldo_               1    

         stylemesausan              1    

       edomaajka.official           1    

           nikailcic                1    

            mm.mia1                 1    

      rozgajelenaofficial           1    

        dunavosiguranje             1    

    andreasusnjara_official         1    

           anaviglam                1    

         marko_vuletic              1    

         deniss_jo_dena             1    

            _a_k_752                1    

          barbarakolar              1    

         hanii_official             1    

         ams_osiguranje             1    

    imerovic_sandi_budivelik        1    

        ddor_osiguranje             1    

        patriciavodopija            1    

        vegafarmaapoteke            1    

          ecija_ivusic              1    

          five.agency               1    

           dm_srbija                1    

            maraboop                1    

        wienerstadtische            1    

          katjagrudnik              1    

            isnice4u                1    
-----------------------------------------

```r
# influencers by REACH
printMD(unique(instagram[,REACH := sum(REACH), FROM][FROM != "anonymous_user",.(FROM,REACH)][order(-REACH),]), big.mark=",")
```


--------------------------------------------
              FROM                  REACH   
-------------------------------- -----------
          elladvornik             7,462,990 

              hana                3,012,500 

          jutarnji.hr             3,001,350 

           majasuput              2,059,960 

            severina              2,053,710 

        jelenamarinovic           2,033,990 

           24sata.hr              1,882,720 

        marijanabatinic           1,841,510 

          nivescelsius            1,500,520 

           dnevnikhr              1,479,450 

            paulasik              1,369,960 

           safariduha             1,272,310 

           august_xvi              982,490  

          slakipalaki              956,640  

         andreaandrassy            939,660  

        slavica.de.jong            932,490  

         malajskitapir             916,190  

       dashofblue_makeup           913,820  

             lalana                876,340  

    lucija_lugomer_official        830,090  

            rkzagreb               801,050  

         more_less_ines            759,560  

        danijeladvornik            727,220  

           emaluketin              701,920  

          ida_prester              695,590  

        mashinthebeauty            660,830  

         lanatheklingor            619,290  

          martina_boss             589,160  

          slavkosobin              575,440  

          zoransprajc              501,960  

         rene_bitorajac            501,340  

          elaajerkovic             493,140  

         vecernji.list             469,520  

         marko_vuletic             468,270  

         pamelaramljak             463,600  

         katarinababan             455,330  

         antonijablace             434,620  

          stazisweets              433,500  

             iwaiva                422,130  

         domacica.sanja            414,610  

            zanamari               373,770  

         korana_marovic            350,580  

          maremikulic              346,520  

          ivanablazoti             327,540  

          iva___radic              322,870  

          boltfood_hr              319,250  

          davorgerbus              316,960  

          josipa_lisac             316,550  

          miffyandrea              303,960  

         discus70queen             303,460  

          nanulananula             295,320  

   beslagicenis_jedini_pravi       289,920  

            skitnica               283,680  

           dorica505               281,820  

          lukanizetic              269,650  

        danijel.subasic            269,460  

          matea.miljan             264,480  

         nevenarendeli             256,890  

            nivea_hr               255,620  

          ecijaojdanic             248,580  

        adrianadurdevic            235,930  

         ivicakostelic             234,580  

        mirjana_mikulec            234,300  

          indiralevak              228,640  

         apoteka_monis             227,700  

      rozgajelenaofficial          222,320  

          ximenamoral              221,110  

           bojana.g.v              217,970  

         teakravarscan             210,680  

          putoholicari             210,030  

       ninasliskovicgoles          201,840  

         koranagvozdic             201,600  

         bornabutijer5             199,880  

           balkan_bet              187,490  

          maja_bajamic             187,420  

        ljupka_tanevska            182,110  

           mozzartbet              177,800  

        frankaofficial_            175,090  

           anaradisic              172,840  

           ivansaric               164,200  

        lucianoplazibat            156,650  

          pharmacy_bcc             156,610  

            jelka12                156,010  

         ivana_miseric             154,600  

           damirkedzo              152,760  

           ninabljak               146,870  

        doris.stankovic            142,530  

            mm.mia1                135,930  

           markodrcic              129,920  

        mario_petrekovic           128,380  

         ornelavistica             128,160  

             rtl_hr                124,260  

          marko.tolja              121,670  

          petrakurtela             119,910  

          telemach.hr              115,090  

         tatjana_juric             114,580  

           brita0507               108,900  

       veronika_rosandic           108,710  

          gingerellica             105,000  

          daliborpetko             104,980  

           sasalozar               104,760  

          anabacinger              104,180  

          suzana_emes              99,260   

     tarikfilipovicofficial        94,490   

       ave_dulcis_tamara           93,540   

         thecoolhunter_            92,550   

            ormarija               86,580   

        varteks.fashion            85,170   

            hardkoo                82,800   

          aboutyou_hr              79,250   

         minea.official            72,460   

         belamakeuplada            72,100   

          renatasopek              71,750   

         maxfactor_bih             70,040   

        ljupkagojicmikic           69,220   

          markogrubnic             69,200   

          thompson_hr              68,730   

           click4chic              67,600   

         dejanlovren06             67,590   

           lidlsrbija              67,240   

         andreafabricc             66,720   

        mariahs_chambers           65,010   

         stjepanowskaja            63,970   

         tvojaljekarna             61,880   

          uvijekgladna             60,310   

        maxbetkladionice           56,980   

         ceravehrvatska            51,770   

         hanii_official            49,600   

        sementa_rajhard            48,520   

           mr.moncina              48,500   

           lorainne7               48,260   

         mylittlezagreb            47,180   

          avonhrvatska             46,580   

       beautypharmacy_hr           44,300   

        oriflame_croatia           42,020   

       podravka_hrvatska           40,290   

     antonija_stupar_jurkin        39,800   

          lizzyvdligt              39,540   

        petra_miksikova            39,470   

         a1hrvatskalife            38,770   

         projekt_ilica             38,010   

          knjazrobert              37,120   

          lidijalesic              37,110   

           lana_gojak              36,970   

           nikailcic               36,690   

         mirna_medstep             36,240   

           sarashine_              36,140   

       ljekarnelukacinzg           34,240   

          inacrnagora              33,590   

         lilly_drogerie            33,560   

         vandawinter148            32,840   

            vindija                32,580   

       smartphonehrvatska          31,770   

           mirnamaras              31,060   

        apoteka_anapharm           30,670   

           ritarumora              30,190   

         pliva_hrvatska            28,800   

         cvit_likarije             28,480   

     tomislavgustinfitness         28,250   

         debela.barbara            28,070   

           wwf_adria               26,470   

         eljekarna24.hr            25,660   

           teamiskon               25,350   

           boombox.hr              23,100   

       maxfactor_hrvatska          22,300   

           chilli_di               20,810   

         dukat_hrvatska            20,440   

           farmasihr               19,440   

         moja_farmacia             19,430   

           farmasibh               19,200   

          ecija_ivusic             18,890   

        patriciavodopija           18,160   

         stylemesausan             18,070   

   oriflamebosnaihercegovina       16,840   

        hrvatski.telekom           16,480   

          soccerbet.rs             16,040   

           lukabulic               15,740   

    andreasusnjara_official        15,250   

             avonba                15,040   

        prima_namjestaj            14,470   

          barbarakolar             13,440   

         deniss_jo_dena            13,400   

          obuca.metro              13,070   

           jglobitelj              13,060   

           avonsrbija              12,740   

       cromaris_hrvatska           12,690   

           mojbonbon               12,280   

       anamarasharmander           11,870   

         meridianbet_rs            11,310   

         dorinlucissima            11,300   

          wolt.croatia             11,070   

         suddenly_nana             10,740   

         mario.valentic            10,610   

           tportal.hr              10,430   

          elegoviceva               9,670   

     obuca.metro.montenegro         9,390   

          ceravesrbija              9,260   

           a1hrvatska               9,000   

         anabegictahiri             8,530   

 savez_elektronskih_sportova_cg     8,480   

       father_of_djordje            8,410   

        vern_university             8,310   

        apoteke_b_pharm             8,200   

            glovo_hr                7,950   

        obuca.metro_bih             7,570   

         ljekarna.pablo             7,370   

         oriflamesrbija             7,310   

          telegram.hr               7,080   

           cerave_ba                6,930   

          yettelsrbija              6,880   

        llums_cosmetics             6,700   

          marko.mrkic               6,690   

           farmasirs                6,290   

        viktorija.radja             6,250   

        ljekarna_marusic            6,240   

          drmaxapoteka              6,060   

           rtl.danas                5,900   

          tinsedlar88               5,790   

           amiepetite               5,650   

     ljekarne_vase_zdravlje         5,600   

          mojaljekarna              5,540   

            maraboop                5,500   

            reci.hr                 5,450   

           ideasrbija               5,430   

       edomaajka.official           5,170   

         optima.telekom             4,640   

       ljekarnaonline.hr            4,570   

           podravkacg               4,320   

             mtelcg                 4,150   

        mercator_centar             4,030   

     ericsson_nikola_tesla          3,840   

     ljekarneprimafarmacia          3,830   

         generalisrbija             3,700   

          bancaintesa               2,930   

          superapoteka              2,820   

       ljekarnajelichrgic           2,780   

          katjagrudnik              2,660   

            a1srbija                2,620   

        mali_muzej_cuda             2,600   

         thecircuitmess             2,560   

           dm_srbija                2,550   

           lutrija.hr               2,310   

          mojaapoteka               2,130   

            psidacta                2,050   

          stitnjaca.hr              1,930   

         _rahul_raj_31              1,890   

          dugandziceva              1,800   

            combis_                 1,730   

           usa_in_mne               1,710   

           visitsplit               1,630   

            phama.hr                1,550   

          mtstvojsvet               1,530   

            tojevuk                 1,430   

       srbotrade_apoteka            1,360   

         apotekalaurus              1,060   

        telemachcrnagora             980    

          erste_banka                910    

             mtelbh                  890    

         croatiacontrol              880    

          five.agency                860    

           meggle_hr                 820    

          vitalsrbija                740    

          centar.cesi                730    

            ctelekom                 720    

        dunavosiguranje              720    

          anna_soldo_                650    

         apoteka_cvejic              640    

            narod.hr                 630    

         apotekaonlinea              570    

        jankovicapoteka              500    

         ljekarnatalan               470    

        wienerstadtische             470    

          eapoteka.rs                450    

          mandis_pharm               440    

         oaza_zdravlja_              390    

       eurobank.direktna             370    

        lipalek_apoteka              360    

       galenpharmapoteke             350    

        vegafarmaapoteke             340    

          marinamamic                280    

           mkm.marina                240    

       erstebankacrnagora            230    

            postacg                  220    

    ljekarnanaglicsostarcic          200    

         ams_osiguranje              180    

        ddor_osiguranje              180    

          daniella.ods               160    

         nosim.varteks               140    

          gponedotcom                120    

   triglav.osiguranje.srbija         120    

    imerovic_sandi_budivelik         120    

       obuca.metro_outlet            90     

           altabanka                 90     

          ljekarna.hr                80     

           anaviglam                 40     

        ljekarnepharmad              30     

            _a_k_752                 14     

            isnice4u                 10     

         bhv_education                0     
--------------------------------------------

```r
# influencers by LIKE
printMD(unique(instagram[,LIKE := sum(LIKE_COUNT), FROM][FROM != "anonymous_user",.(FROM,LIKE)][order(-LIKE),]), big.mark=",")
```


------------------------------------------
              FROM                 LIKE   
-------------------------------- ---------
          elladvornik             468,694 

          jutarnji.hr             286,741 

              hana                233,577 

            severina              203,127 

           majasuput              201,773 

           24sata.hr              184,055 

        jelenamarinovic           164,289 

          nivescelsius            145,457 

        marijanabatinic           141,985 

           dnevnikhr              141,638 

           safariduha             123,138 

            paulasik              99,857  

         malajskitapir            90,472  

        slavica.de.jong           89,970  

             lalana               86,718  

            rkzagreb              78,885  

         andreaandrassy           78,164  

           august_xvi             72,503  

        danijeladvornik           70,285  

         more_less_ines           67,639  

          ida_prester             67,210  

           emaluketin             66,031  

        mashinthebeauty           65,361  

          slakipalaki             59,535  

       dashofblue_makeup          56,782  

          slavkosobin             55,388  

          martina_boss            51,109  

         rene_bitorajac           49,651  

          zoransprajc             49,153  

         lanatheklingor           48,995  

          elaajerkovic            48,882  

         marko_vuletic            46,735  

         katarinababan            45,151  

         vecernji.list            44,643  

         domacica.sanja           40,175  

             iwaiva               37,732  

            zanamari              36,273  

         antonijablace            34,240  

         pamelaramljak            33,621  

          maremikulic             33,422  

          davorgerbus             31,314  

          josipa_lisac            31,132  

         discus70queen            29,861  

          stazisweets             28,879  

   beslagicenis_jedini_pravi      28,757  

          nanulananula            28,132  

          miffyandrea             27,562  

          ivanablazoti            27,460  

          iva___radic             26,926  

            skitnica              26,622  

        danijel.subasic           26,404  

          lukanizetic             25,280  

          ecijaojdanic            24,464  

        mirjana_mikulec           23,048  

         ivicakostelic            22,984  

          indiralevak             22,504  

      rozgajelenaofficial         22,019  

          ximenamoral             22,016  

           bojana.g.v             21,180  

         teakravarscan            20,657  

         nevenarendeli            20,237  

        adrianadurdevic           20,186  

         korana_marovic           19,599  

          putoholicari            19,590  

       ninasliskovicgoles         18,714  

           balkan_bet             18,673  

          maja_bajamic            18,484  

        ljupka_tanevska           17,842  

         bornabutijer5            17,821  

         koranagvozdic            17,726  

           mozzartbet             17,703  

          matea.miljan            17,581  

        frankaofficial_           17,426  

           dorica505              17,123  

           anaradisic             16,732  

        lucianoplazibat           15,554  

            jelka12               15,452  

         ivana_miseric            15,269  

           damirkedzo             14,764  

           ninabljak              14,209  

           ivansaric              13,769  

            mm.mia1               13,270  

        doris.stankovic           12,968  

           markodrcic             12,838  

         ornelavistica            12,608  

        mario_petrekovic          12,425  

             rtl_hr               12,248  

          marko.tolja             12,082  

          petrakurtela            11,512  

         tatjana_juric            11,333  

         apoteka_monis            10,746  

          gingerellica            10,471  

            nivea_hr              10,316  

           sasalozar              10,307  

          daliborpetko            10,149  

       veronika_rosandic           9,783  

     tarikfilipovicofficial        9,278  

          obuca.metro              9,239  

          telemach.hr              9,118  

         thecoolhunter_            9,113  

          suzana_emes              9,078  

       ave_dulcis_tamara           8,748  

        varteks.fashion            8,348  

          boltfood_hr              8,184  

            hardkoo                8,178  

          aboutyou_hr              7,921  

          anabacinger              7,485  

            ormarija               7,407  

         belamakeuplada            7,137  

         minea.official            7,077  

          renatasopek              7,030  

          pharmacy_bcc             6,970  

         maxfactor_bih             6,836  

          thompson_hr              6,757  

         dejanlovren06             6,740  

           lidlsrbija              6,616  

         andreafabricc             6,613  

           click4chic              6,590  

         stjepanowskaja            6,344  

     obuca.metro.montenegro        6,241  

        mariahs_chambers           5,966  

          markogrubnic             5,938  

        ljupkagojicmikic           5,850  

          uvijekgladna             5,810  

           brita0507               5,703  

        maxbetkladionice           5,671  

         ceravehrvatska            5,005  

         hanii_official            4,943  

        obuca.metro_bih            4,876  

        sementa_rajhard            4,771  

          avonhrvatska             4,262  

       beautypharmacy_hr           4,159  

         mylittlezagreb            4,157  

        oriflame_croatia           4,130  

          lizzyvdligt              3,933  

       podravka_hrvatska           3,861  

     antonija_stupar_jurkin        3,832  

         a1hrvatskalife            3,812  

         projekt_ilica             3,759  

          knjazrobert              3,676  

           nikailcic               3,662  

         mirna_medstep             3,586  

           lorainne7               3,558  

           sarashine_              3,486  

           lana_gojak              3,484  

           mr.moncina              3,359  

         tvojaljekarna             3,357  

         lilly_drogerie            3,342  

         vandawinter148            3,219  

            vindija                3,216  

       smartphonehrvatska          3,149  

           mirnamaras              3,036  

        petra_miksikova            2,933  

         cvit_likarije             2,853  

         pliva_hrvatska            2,825  

         debela.barbara            2,800  

     tomislavgustinfitness         2,700  

           wwf_adria               2,626  

        apoteka_anapharm           2,249  

       maxfactor_hrvatska          2,188  

         eljekarna24.hr            2,099  

           chilli_di               2,069  

         dukat_hrvatska            2,008  

           farmasihr               1,927  

           farmasibh               1,910  

         moja_farmacia             1,899  

          ecija_ivusic             1,856  

        patriciavodopija           1,809  

           ritarumora              1,785  

         stylemesausan             1,775  

        vern_university            1,672  

   oriflamebosnaihercegovina       1,606  

        hrvatski.telekom           1,568  

           lukabulic               1,563  

           boombox.hr              1,562  

    andreasusnjara_official        1,494  

             avonba                1,490  

        prima_namjestaj            1,424  

         deniss_jo_dena            1,322  

           avonsrbija              1,269  

           jglobitelj              1,243  

          barbarakolar             1,213  

           mojbonbon               1,205  

          soccerbet.rs             1,195  

       anamarasharmander           1,146  

       cromaris_hrvatska           1,129  

         meridianbet_rs            1,112  

         dorinlucissima            1,091  

         suddenly_nana             1,040  

           tportal.hr              1,027  

          ceravesrbija             1,016  

          inacrnagora               972   

          wolt.croatia              953   

          elegoviceva               948   

           dm_srbija                939   

         mario.valentic             927   

           a1hrvatska               890   

           teamiskon                849   

 savez_elektronskih_sportova_cg     847   

         anabegictahiri             847   

        apoteke_b_pharm             827   

         ljekarna.pablo             735   

           cerave_ba                733   

          telegram.hr               680   

         oriflamesrbija             680   

        llums_cosmetics             657   

          marko.mrkic               657   

            glovo_hr                655   

          drmaxapoteka              646   

          yettelsrbija              644   

           farmasirs                625   

        viktorija.radja             599   

           rtl.danas                570   

        ljekarna_marusic            568   

           amiepetite               555   

          mojaljekarna              543   

          tinsedlar88               543   

           ideasrbija               541   

     ljekarne_vase_zdravlje         536   

            reci.hr                 527   

       edomaajka.official           514   

            maraboop                511   

          superapoteka              477   

       ljekarnelukacinzg            475   

       ljekarnaonline.hr            456   

         optima.telekom             433   

           podravkacg               431   

             mtelcg                 408   

        mercator_centar             402   

     ljekarneprimafarmacia          381   

     ericsson_nikola_tesla          375   

         generalisrbija             368   

       father_of_djordje            343   

          bancaintesa               278   

       ljekarnajelichrgic           273   

        mali_muzej_cuda             260   

          katjagrudnik              260   

            a1srbija                253   

         thecircuitmess             250   

          mojaapoteka               221   

             mtelbh                 220   

           lutrija.hr               218   

            psidacta                197   

          stitnjaca.hr              189   

          dugandziceva              179   

            combis_                 174   

           usa_in_mne               167   

           visitsplit               160   

          mtstvojsvet               153   

            phama.hr                145   

       srbotrade_apoteka            137   

         _rahul_raj_31              130   

         apotekalaurus              102   

        telemachcrnagora            96    

          erste_banka               90    

         croatiacontrol             88    

          five.agency               86    

           meggle_hr                82    

          vitalsrbija               75    

          centar.cesi               73    

            ctelekom                72    

        dunavosiguranje             72    

          eapoteka.rs               67    

         apotekaonlinea             66    

         apoteka_cvejic             64    

            narod.hr                62    

       obuca.metro_outlet           59    

         oaza_zdravlja_             55    

        jankovicapoteka             50    

        wienerstadtische            47    

         ljekarnatalan              44    

          mandis_pharm              42    

       eurobank.direktna            37    

       galenpharmapoteke            36    

   triglav.osiguranje.srbija        35    

        lipalek_apoteka             34    

        vegafarmaapoteke            34    

       erstebankacrnagora           23    

            postacg                 22    

    ljekarnanaglicsostarcic         20    

         ams_osiguranje             18    

        ddor_osiguranje             18    

         nosim.varteks              14    

          gponedotcom               12    

         bhv_education              12    

    imerovic_sandi_budivelik        12    

           altabanka                 9    

            _a_k_752                 8    

          ljekarna.hr                6    

        ljekarnepharmad              3    

            isnice4u                 1    

    lucija_lugomer_official          0    

          lidijalesic                0    

           mkm.marina                0    

          daniella.ods               0    

          marinamamic                0    

          anna_soldo_                0    

            tojevuk                  0    

           anaviglam                 0    
------------------------------------------

```r
# influencers by INTERACTIONS
printMD(unique(instagram[,INTERACTIONS := sum(INTERACTIONS), FROM][FROM != "anonymous_user",.(FROM,INTERACTIONS)][order(-INTERACTIONS),]), big.mark=",")
```


-----------------------------------------------
              FROM                INTERACTIONS 
-------------------------------- --------------
          elladvornik               746,299    

              hana                  301,250    

          jutarnji.hr               300,135    

           majasuput                205,999    

            severina                205,371    

        jelenamarinovic             203,400    

           24sata.hr                188,272    

        marijanabatinic             184,150    

          nivescelsius              150,052    

           dnevnikhr                147,968    

            paulasik                136,996    

           safariduha               127,231    

           august_xvi                98,249    

          slakipalaki                95,664    

         andreaandrassy              93,966    

        slavica.de.jong              93,249    

         malajskitapir               91,619    

       dashofblue_makeup             91,382    

             lalana                  87,634    

    lucija_lugomer_official          83,009    

            rkzagreb                 80,105    

         more_less_ines              75,957    

        danijeladvornik              72,722    

           emaluketin                70,192    

          ida_prester                69,560    

        mashinthebeauty              66,087    

         lanatheklingor              61,929    

          martina_boss               58,916    

          slavkosobin                57,544    

          zoransprajc                50,196    

         rene_bitorajac              50,134    

          elaajerkovic               49,314    

         vecernji.list               46,952    

         marko_vuletic               46,827    

         pamelaramljak               46,360    

         katarinababan               45,533    

         antonijablace               43,462    

          stazisweets                43,350    

             iwaiva                  42,213    

         domacica.sanja              41,461    

            zanamari                 37,377    

         korana_marovic              35,060    

          maremikulic                34,652    

          ivanablazoti               32,754    

          iva___radic                32,287    

          boltfood_hr                31,925    

          davorgerbus                31,689    

          josipa_lisac               31,654    

          miffyandrea                30,397    

         discus70queen               30,346    

          nanulananula               29,621    

   beslagicenis_jedini_pravi         28,992    

            skitnica                 28,384    

           dorica505                 28,182    

          lukanizetic                26,965    

        danijel.subasic              26,946    

          matea.miljan               26,448    

         apoteka_monis               26,349    

         nevenarendeli               25,689    

            nivea_hr                 25,562    

          ecijaojdanic               24,858    

        adrianadurdevic              23,593    

         ivicakostelic               23,458    

        mirjana_mikulec              23,430    

          indiralevak                22,863    

      rozgajelenaofficial            22,232    

          ximenamoral                22,110    

           bojana.g.v                21,797    

         teakravarscan               21,074    

          putoholicari               21,003    

       ninasliskovicgoles            20,184    

         koranagvozdic               20,160    

         bornabutijer5               19,988    

           balkan_bet                18,774    

          maja_bajamic               18,742    

        ljupka_tanevska              18,211    

           mozzartbet                17,793    

        frankaofficial_              17,509    

           anaradisic                17,284    

           ivansaric                 16,440    

          pharmacy_bcc               15,712    

        lucianoplazibat              15,673    

            jelka12                  15,601    

         ivana_miseric               15,460    

           damirkedzo                15,276    

           ninabljak                 14,689    

        doris.stankovic              14,253    

            mm.mia1                  13,593    

           markodrcic                12,992    

        mario_petrekovic             12,838    

         ornelavistica               12,816    

             rtl_hr                  12,427    

          marko.tolja                12,167    

          petrakurtela               11,991    

          telemach.hr                11,509    

         tatjana_juric               11,459    

           brita0507                 10,891    

       veronika_rosandic             10,871    

          gingerellica               10,508    

          daliborpetko               10,502    

           sasalozar                 10,476    

          anabacinger                10,418    

          suzana_emes                9,926     

     tarikfilipovicofficial          9,449     

          obuca.metro                9,406     

       ave_dulcis_tamara             9,354     

         thecoolhunter_              9,255     

            ormarija                 8,658     

        varteks.fashion              8,517     

            hardkoo                  8,280     

          aboutyou_hr                7,925     

         minea.official              7,246     

         belamakeuplada              7,227     

          renatasopek                7,180     

         maxfactor_bih               7,011     

        ljupkagojicmikic             6,922     

          markogrubnic               6,920     

          thompson_hr                6,873     

           click4chic                6,773     

         dejanlovren06               6,759     

           lidlsrbija                6,730     

         andreafabricc               6,672     

        mariahs_chambers             6,501     

         stjepanowskaja              6,397     

     obuca.metro.montenegro          6,307     

         tvojaljekarna               6,188     

          uvijekgladna               6,031     

        maxbetkladionice             5,698     

         ceravehrvatska              5,177     

         hanii_official              4,960     

        obuca.metro_bih              4,913     

        sementa_rajhard              4,852     

           mr.moncina                4,850     

           lorainne7                 4,826     

         mylittlezagreb              4,718     

          avonhrvatska               4,658     

       beautypharmacy_hr             4,430     

        oriflame_croatia             4,202     

       podravka_hrvatska             4,029     

     antonija_stupar_jurkin          3,980     

          lizzyvdligt                3,954     

        petra_miksikova              3,947     

         a1hrvatskalife              3,877     

         projekt_ilica               3,812     

          knjazrobert                3,712     

          lidijalesic                3,711     

           lana_gojak                3,697     

           nikailcic                 3,669     

         mirna_medstep               3,624     

           sarashine_                3,614     

       ljekarnelukacinzg             3,424     

         lilly_drogerie              3,367     

          inacrnagora                3,359     

         vandawinter148              3,284     

            vindija                  3,258     

       smartphonehrvatska            3,177     

        apoteka_anapharm             3,106     

           mirnamaras                3,106     

           ritarumora                3,019     

         cvit_likarije               2,930     

         pliva_hrvatska              2,880     

     tomislavgustinfitness           2,825     

         debela.barbara              2,807     

           wwf_adria                 2,658     

         eljekarna24.hr              2,566     

           teamiskon                 2,535     

           boombox.hr                2,310     

       maxfactor_hrvatska            2,230     

           chilli_di                 2,081     

         dukat_hrvatska              2,044     

           farmasihr                 1,944     

         moja_farmacia               1,943     

           farmasibh                 1,920     

          ecija_ivusic               1,889     

        patriciavodopija             1,816     

         stylemesausan               1,807     

   oriflamebosnaihercegovina         1,684     

        vern_university              1,674     

        hrvatski.telekom             1,648     

          soccerbet.rs               1,604     

           lukabulic                 1,574     

    andreasusnjara_official          1,525     

             avonba                  1,504     

        prima_namjestaj              1,447     

          barbarakolar               1,344     

         deniss_jo_dena              1,340     

           jglobitelj                1,306     

           avonsrbija                1,276     

       cromaris_hrvatska             1,269     

           mojbonbon                 1,228     

       anamarasharmander             1,187     

         meridianbet_rs              1,131     

         dorinlucissima              1,130     

          wolt.croatia               1,107     

         suddenly_nana               1,074     

         mario.valentic              1,061     

          ceravesrbija               1,044     

           tportal.hr                1,043     

          elegoviceva                 967      

           dm_srbija                  943      

           a1hrvatska                 900      

         anabegictahiri               853      

 savez_elektronskih_sportova_cg       848      

       father_of_djordje              841      

        apoteke_b_pharm               835      

            glovo_hr                  795      

           cerave_ba                  759      

         ljekarna.pablo               737      

         oriflamesrbija               731      

          telegram.hr                 708      

          yettelsrbija                688      

          drmaxapoteka                672      

        llums_cosmetics               670      

          marko.mrkic                 669      

           farmasirs                  629      

        viktorija.radja               625      

        ljekarna_marusic              624      

           rtl.danas                  590      

          tinsedlar88                 579      

           amiepetite                 565      

     ljekarne_vase_zdravlje           560      

          mojaljekarna                554      

            maraboop                  550      

            reci.hr                   545      

           ideasrbija                 543      

       edomaajka.official             517      

          superapoteka                489      

         optima.telekom               464      

       ljekarnaonline.hr              457      

           podravkacg                 432      

             mtelcg                   415      

        mercator_centar               403      

     ericsson_nikola_tesla            384      

     ljekarneprimafarmacia            383      

         generalisrbija               370      

          bancaintesa                 294      

       ljekarnajelichrgic             278      

          katjagrudnik                266      

            a1srbija                  262      

        mali_muzej_cuda               260      

         thecircuitmess               256      

           lutrija.hr                 231      

             mtelbh                   222      

          mojaapoteka                 222      

            psidacta                  205      

          stitnjaca.hr                193      

         _rahul_raj_31                189      

          dugandziceva                180      

            combis_                   175      

           usa_in_mne                 171      

           visitsplit                 163      

            phama.hr                  155      

          mtstvojsvet                 153      

            tojevuk                   143      

       srbotrade_apoteka              137      

         apotekalaurus                107      

        telemachcrnagora               98      

          erste_banka                  91      

         croatiacontrol                88      

          five.agency                  86      

           meggle_hr                   82      

          vitalsrbija                  75      

          centar.cesi                  73      

         apotekaonlinea                72      

            ctelekom                   72      

        dunavosiguranje                72      

          eapoteka.rs                  67      

          anna_soldo_                  65      

         apoteka_cvejic                64      

            narod.hr                   63      

       obuca.metro_outlet              61      

         oaza_zdravlja_                57      

        jankovicapoteka                50      

         ljekarnatalan                 47      

        wienerstadtische               47      

          mandis_pharm                 44      

           mkm.marina                  42      

       eurobank.direktna               37      

       galenpharmapoteke               36      

        lipalek_apoteka                36      

   triglav.osiguranje.srbija           35      

        vegafarmaapoteke               34      

          marinamamic                  28      

       erstebankacrnagora              23      

            postacg                    22      

    ljekarnanaglicsostarcic            20      

         ams_osiguranje                18      

        ddor_osiguranje                18      

          daniella.ods                 16      

         nosim.varteks                 14      

            _a_k_752                   13      

          gponedotcom                  12      

         bhv_education                 12      

    imerovic_sandi_budivelik           12      

           altabanka                   9       

          ljekarna.hr                  8       

           anaviglam                   4       

        ljekarnepharmad                3       

            isnice4u                   1       
-----------------------------------------------

```r
# influencers by COMMENT
printMD(unique(instagram[,COMMENT := sum(COMMENT_COUNT), FROM][FROM != "anonymous_user",.(FROM,COMMENT)][order(-COMMENT),]), big.mark=",")
```


------------------------------------------
              FROM                COMMENT 
-------------------------------- ---------
          elladvornik             277,605 

    lucija_lugomer_official       83,009  

              hana                67,673  

        marijanabatinic           42,165  

        jelenamarinovic           39,111  

            paulasik              37,139  

          slakipalaki             36,129  

       dashofblue_makeup          34,600  

           august_xvi             25,746  

          boltfood_hr             23,741  

         andreaandrassy           15,802  

         apoteka_monis            15,603  

         korana_marovic           15,461  

            nivea_hr              15,246  

          stazisweets             14,471  

          jutarnji.hr             13,394  

         lanatheklingor           12,934  

         pamelaramljak            12,739  

           dorica505              11,059  

         antonijablace             9,222  

          matea.miljan             8,867  

          pharmacy_bcc             8,742  

         more_less_ines            8,318  

          martina_boss             7,807  

           dnevnikhr               6,330  

         nevenarendeli             5,452  

          iva___radic              5,361  

          ivanablazoti             5,294  

           brita0507               5,188  

          nivescelsius             4,595  

             iwaiva                4,481  

           majasuput               4,226  

           24sata.hr               4,217  

           emaluketin              4,161  

           safariduha              4,093  

          lidijalesic              3,711  

        adrianadurdevic            3,407  

        slavica.de.jong            3,279  

       ljekarnelukacinzg           2,949  

          anabacinger              2,933  

          miffyandrea              2,835  

         tvojaljekarna             2,831  

           ivansaric               2,671  

        danijeladvornik            2,437  

         koranagvozdic             2,434  

          telemach.hr              2,391  

          inacrnagora              2,387  

          ida_prester              2,350  

         vecernji.list             2,309  

            severina               2,244  

         bornabutijer5             2,167  

          slavkosobin              2,156  

            skitnica               1,762  

           teamiskon               1,686  

          lukanizetic              1,685  

           mr.moncina              1,491  

          nanulananula             1,489  

       ninasliskovicgoles          1,470  

          putoholicari             1,413  

         domacica.sanja            1,286  

        doris.stankovic            1,285  

           lorainne7               1,268  

            ormarija               1,251  

           ritarumora              1,234  

          maremikulic              1,230  

            rkzagreb               1,220  

         malajskitapir             1,147  

            zanamari               1,104  

       veronika_rosandic           1,088  

        ljupkagojicmikic           1,072  

          zoransprajc              1,043  

        petra_miksikova            1,014  

          markogrubnic              982   

             lalana                 916   

        apoteka_anapharm            857   

          suzana_emes               848   

           boombox.hr               748   

        mashinthebeauty             726   

           bojana.g.v               617   

       ave_dulcis_tamara            606   

         mylittlezagreb             561   

           anaradisic               552   

        danijel.subasic             542   

        mariahs_chambers            535   

          josipa_lisac              522   

           damirkedzo               512   

       father_of_djordje            498   

         discus70queen              485   

         rene_bitorajac             483   

           ninabljak                480   

          petrakurtela              479   

         ivicakostelic              474   

         eljekarna24.hr             467   

          elaajerkovic              432   

         teakravarscan              417   

        mario_petrekovic            413   

          soccerbet.rs              409   

          avonhrvatska              396   

          ecijaojdanic              394   

         katarinababan              382   

        mirjana_mikulec             382   

          davorgerbus               375   

        ljupka_tanevska             369   

          indiralevak               359   

          daliborpetko              353   

            mm.mia1                 323   

       beautypharmacy_hr            271   

          maja_bajamic              258   

   beslagicenis_jedini_pravi        235   

          uvijekgladna              221   

           lana_gojak               213   

      rozgajelenaofficial           213   

         ornelavistica              208   

         ivana_miseric              191   

           click4chic               183   

             rtl_hr                 179   

         maxfactor_bih              175   

         ceravehrvatska             172   

     tarikfilipovicofficial         171   

        varteks.fashion             169   

           sasalozar                169   

         minea.official             169   

       podravka_hrvatska            168   

          obuca.metro               167   

           markodrcic               154   

          wolt.croatia              154   

          renatasopek               150   

            jelka12                 149   

     antonija_stupar_jurkin         148   

            tojevuk                 143   

         thecoolhunter_             142   

       cromaris_hrvatska            140   

            glovo_hr                140   

         mario.valentic             134   

          barbarakolar              131   

           sarashine_               128   

         tatjana_juric              126   

     tomislavgustinfitness          125   

        lucianoplazibat             119   

          thompson_hr               116   

           lidlsrbija               114   

            hardkoo                 102   

           balkan_bet               101   

          ximenamoral               94    

         marko_vuletic              92    

           mozzartbet               90    

         belamakeuplada             90    

          marko.tolja               85    

        frankaofficial_             83    

        sementa_rajhard             81    

        hrvatski.telekom            80    

   oriflamebosnaihercegovina        78    

         cvit_likarije              77    

        oriflame_croatia            72    

           mirnamaras               70    

     obuca.metro.montenegro         66    

         vandawinter148             65    

         a1hrvatskalife             65    

          anna_soldo_               65    

           jglobitelj               63    

         andreafabricc              59    

         _rahul_raj_31              59    

        ljekarna_marusic            56    

         pliva_hrvatska             55    

         projekt_ilica              53    

         stjepanowskaja             53    

         oriflamesrbija             51    

         moja_farmacia              44    

          yettelsrbija              44    

            vindija                 42    

       maxfactor_hrvatska           42    

           mkm.marina               42    

       anamarasharmander            41    

         dorinlucissima             39    

            maraboop                39    

         mirna_medstep              38    

        obuca.metro_bih             37    

          gingerellica              37    

         dukat_hrvatska             36    

          knjazrobert               36    

          tinsedlar88               36    

         suddenly_nana              34    

          ecija_ivusic              33    

           wwf_adria                32    

         stylemesausan              32    

         optima.telekom             31    

    andreasusnjara_official         31    

       smartphonehrvatska           28    

          ceravesrbija              28    

          telegram.hr               28    

          marinamamic               28    

        maxbetkladionice            27    

           cerave_ba                26    

          drmaxapoteka              26    

        viktorija.radja             26    

         lilly_drogerie             25    

     ljekarne_vase_zdravlje         24    

           mojbonbon                23    

        prima_namjestaj             23    

          lizzyvdligt               21    

           rtl.danas                20    

         meridianbet_rs             19    

         dejanlovren06              19    

          elegoviceva               19    

            reci.hr                 18    

         deniss_jo_dena             18    

           farmasihr                17    

         hanii_official             17    

           tportal.hr               16    

          bancaintesa               16    

          daniella.ods              16    

             avonba                 14    

           lutrija.hr               13    

        llums_cosmetics             13    

          superapoteka              12    

           chilli_di                12    

          marko.mrkic               12    

          mojaljekarna              11    

           lukabulic                11    

           amiepetite               10    

           farmasibh                10    

            phama.hr                10    

           a1hrvatska               10    

            a1srbija                 9    

     ericsson_nikola_tesla           9    

        apoteke_b_pharm              8    

            psidacta                 8    

           avonsrbija                7    

             mtelcg                  7    

         debela.barbara              7    

           nikailcic                 7    

        patriciavodopija             7    

         thecircuitmess              6    

         apotekaonlinea              6    

         anabegictahiri              6    

          katjagrudnik               6    

         apotekalaurus               5    

       ljekarnajelichrgic            5    

            _a_k_752                 5    

          aboutyou_hr                4    

           farmasirs                 4    

          stitnjaca.hr               4    

           anaviglam                 4    

           usa_in_mne                4    

           dm_srbija                 4    

         ljekarnatalan               3    

           visitsplit                3    

       edomaajka.official            3    

        telemachcrnagora             2    

     ljekarneprimafarmacia           2    

             mtelbh                  2    

         ljekarna.pablo              2    

          mandis_pharm               2    

        vern_university              2    

           ideasrbija                2    

         generalisrbija              2    

         oaza_zdravlja_              2    

          ljekarna.hr                2    

       obuca.metro_outlet            2    

        lipalek_apoteka              2    

        mercator_centar              1    

       ljekarnaonline.hr             1    

            combis_                  1    

 savez_elektronskih_sportova_cg      1    

          erste_banka                1    

          mojaapoteka                1    

           podravkacg                1    

            narod.hr                 1    

          dugandziceva               1    

        mali_muzej_cuda              0    

        ljekarnepharmad              0    

            postacg                  0    

          gponedotcom                0    

    ljekarnanaglicsostarcic          0    

          vitalsrbija                0    

          eapoteka.rs                0    

       eurobank.direktna             0    

       srbotrade_apoteka             0    

       galenpharmapoteke             0    

         nosim.varteks               0    

   triglav.osiguranje.srbija         0    

         bhv_education               0    

           meggle_hr                 0    

         apoteka_cvejic              0    

          centar.cesi                0    

           altabanka                 0    

         croatiacontrol              0    

       erstebankacrnagora            0    

            ctelekom                 0    

        jankovicapoteka              0    

        dunavosiguranje              0    

          mtstvojsvet                0    

         ams_osiguranje              0    

    imerovic_sandi_budivelik         0    

        ddor_osiguranje              0    

        vegafarmaapoteke             0    

          five.agency                0    

        wienerstadtische             0    

            isnice4u                 0    
------------------------------------------

```r
# dim before tokenize
dim(instagram)
```

```
## [1] 53804    51
```

```r
# tokenize
instagram %>% 
  unnest_tokens(word, FULL_TEXT) -> insta
# dim after tokenize
dim(insta)
```

```
## [1] 3969533      51
```

```r
# check
insta %>% 
  sample_n(.,10)
```

```
##           DATE     TIME
##  1: 2022-03-18 21:49:29
##  2: 2021-12-17 11:11:12
##  3: 2022-01-18 10:38:55
##  4: 2022-03-01 15:11:16
##  5: 2022-01-19 12:24:23
##  6: 2022-03-04 09:03:04
##  7: 2022-03-10 18:17:00
##  8: 2021-11-29 14:11:07
##  9: 2022-04-16 19:40:57
## 10: 2022-02-16 13:00:17
##                                                                                                                                                                                                                                                                                                                                                                                                                                                          TITLE
##  1:                                   Prvi dio našeg gostovanja 16.02.2022 na otvzagreb u emisiji "Svakodnevica" kod šarmantne urednice i voditeljice <U+2698> patricianovakovic Pogledajte i poslušajte a potom brzo kupite ulaznice za naše nove izvedbe! Puse čak dvije <U+0001F48B><U+0001F48B> svima od vaših Kolumbina! <U+0001F939><U+200D>+<U+FE0F>teica_bojica <U+0001F939><U+200D>+<U+FE0F>matas_irena #setnjaskolumbinama #walkwiththekolumbinas...
##  2:                                                                                                   [INSTA ADVENT U ISKONU] Tko ih pije poslije teretane, a tko stavlja u jutarnju zobenu kasu? Danas u insta darivanju imamo prve proteine proizvedene u Hrvatskoj... Kome bi ovaj poklon bio top? Proteos je hrvatski brend sportske prehrane koji razvija dodatke u prehrani u čiju ćete kvalitetu, podrijetlo sirovine, način proizvodnje i očekivane...
##  3:                                                                                                    Prve zračne snimke nakon vulkanske katastrofe! Cijela otočna država Tongo zarobljena je u pepelu i poplavama koje je prouzročio podvodni vulkan. Otočjem je zavladao i komunikacijski mrak jer je oštećen glavni kabel koji otoke povezuje s ostatkom svijeta, a na ovim se fotografijama vide razmjeri zastrašujuće štete. Više pročitajte na našem...
##  4:                                                                           Valamar Tamaris Resort smješten je na zelenom poluotoku Lanterna u očuvanoj prirodi. Krase ga njegovani parkovi, gusta hladovina hrasta, crnike i borova i aromatično mediteransko raslinje uz šetnice i uređene plaže. <U+0001F305><U+0001F305> Resort privlači svojim lokalnim šarmom i toplim obiteljskim ugođajem kao i sjajnim zabavnim programom za djecu u sklopu Maro...
##  5:                                                                                                  [PAMETNA RASVJETA ZA VAŠ SVAKODNEVNI ŽIVOT] WiZ pametne žarulje se uklapaju prilikom uređenja svakog doma, pogodni su za svaku priliku, bez obzira da li se radi o vašoj dnevnoj sobi, kuhinji, kupaonici pa čak i dvorištu. Pametnom rasvjetom možete upravljati na brz i praktičan način, čak i kad niste doma. Potrebno je instalirati aplikaciju i...
##  6: <U+0001F51D><U+0001F525>NOVA KOLEKCIJA<U+0001F525><U+0001F51D> Sako od umjetne kože dostupan u crnoj, smeđoj i bež boji. Online cijena 169.00 kn <U+0001F495> UNIVERZALNA VELIČINA Novu kolekciju naruči na našem web shopu Manilla.hr <U+0001F4E6> <U+0001F4B8> Cijena dostave je 30 kn <U+0001F552> Rok dostave 2-3 radna dana <U+0001F4E6> Proizvode s\241aljemo DPD brzom pos\241tom <U+0001F495> BESPLATNA POS\241TARINA iznad 399 kn #manillagirl #fashion...
##  7:                                                                     <U+2757><U+FE0F>PRODAJA<U+2757><U+FE0F> Gotov stan u Luštica Bay Centrale. Mozda na rate! Na prodaju su 3 stana: – Stan sa 1 spavaćom sobom, površine 56 m2, ostava, 2. sprat – potkrovlje. Stan je već pušten u upotrebu. Cijena – 224.000 eura. – Stan sa 1 spavaćom sobom, površine 60 m2, 3. sprat- potkrovlje. Puštanje u rad – april 2022. Cijena – 240.000 eura. – Stan sa 2...
##  8:                                                                                             Zahvaljujući donaciji od 350.000 kuna tvrtke Kaufland Hrvatska k.d. UNICEF je opremio 5 inkluzivnih kabineta na visokim učilištima u Zagrebu, Splitu, Osijeku, Puli i Rijeci. Ovom su inicijativom kauflandhrvatska i UNICEF, u suradnji s viskom učilištima, podržali unaprjeđenje kvalitete odgoja i obrazovanja sve djece, a posebno djece s teškoćama u...
##  9:                                  Danas još jedna objava, a onda na zasluženi odmor (pisanje kolokvija nije odmor <U+0001F605>). Neka ove godine vaš blagdanski stol bude ispunjen suosjećanjem i ukusnim veganskim jelima. <U+0001F331> Slavite život u kojem nitko ne stradava. <U+0001F423><U+0001F430><U+0001F338> Pinca je ispala odlično, a miris vraća na djetinjstvo. <U+0001F970> Sastojci za dvije pince: - 500 g brašna (ja sam koristila 250...
## 10:                                                                                                                                                                                                                                                                                                Za tebe smo dostupni na više digitalnih kanala podrške: WhatsAppu, Viberu i Webchatu! <U+0001F4AC> <U+27A1> Pronađite nas skeniranjem koda na web stranici.
##               FROM         AUTHOR                                      URL
##  1: anonymous_user anonymous_user https://www.instagram.com/p/CbQk5hOlxfp/
##  2:      teamiskon      teamiskon https://www.instagram.com/p/CXlINmnMBRu/
##  3:    jutarnji.hr    jutarnji.hr https://www.instagram.com/p/CY3d9M9otGM/
##  4: anonymous_user anonymous_user https://www.instagram.com/p/CakGgrusCH2/
##  5: anonymous_user anonymous_user https://www.instagram.com/p/CY6O0e5qD54/
##  6: anonymous_user anonymous_user https://www.instagram.com/p/CarKwhPMfmK/
##  7: anonymous_user anonymous_user https://www.instagram.com/p/Ca7m64nNY5u/
##  8: anonymous_user anonymous_user  https://www.instagram.com/p/CW3Gfrjqw-T
##  9: anonymous_user anonymous_user https://www.instagram.com/p/Cca7EtpMT_I/
## 10: optima.telekom optima.telekom https://www.instagram.com/p/CaCZL76KD6x/
##                                                                                                                                                                                                   URL_PHOTO
##  1: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  2: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  3: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  4: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  5: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  6: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  7: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  8: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  9: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
## 10: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##     SOURCE_TYPE GROUP_NAME KEYWORD_NAME FOUND_KEYWORDS LANGUAGES LOCATIONS TAGS
##  1:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  2:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  3:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  4:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  5:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  6:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  7:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  8:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
##  9:   instagram       Luka         opće              i    hr, bs    HR, BA   NA
## 10:   instagram       Luka         opće              i    hr, bs        HR   NA
##     MANUAL_SENTIMENT AUTO_SENTIMENT
##  1:               NA       positive
##  2:               NA       positive
##  3:               NA       negative
##  4:               NA       positive
##  5:               NA        neutral
##  6:               NA        neutral
##  7:               NA       positive
##  8:               NA       negative
##  9:               NA       positive
## 10:               NA        neutral
##                                                                                                                                                                                                                                                                                                                                   MENTION_SNIPPET
##  1:                                                                  Prvi dio našeg gostovanja 16.02.2022 na otvzagreb u emisiji "Svakodnevica" kod šarmantne urednice i voditeljice <U+2698> patricianovakovic Pogledajte i poslušajte a potom brzo kupite ulaznice za naše nove izvedbe! Puse čak dvije <U+0001F48B><U+0001F48B> svima od vaših
##  2:                                                                                         kasu? Danas u insta darivanju imamo prve proteine proizvedene u Hrvatskoj... Kome bi ovaj poklon bio top? Proteos je hrvatski brend sportske prehrane koji razvija dodatke u prehrani u čiju ćete kvalitetu, podrijetlo sirovine, način proizvodnje i
##  3:                                                                                      Cijela otočna država Tongo zarobljena je u pepelu i poplavama koje je prouzročio podvodni vulkan. Otočjem je zavladao i komunikacijski mrak jer je oštećen glavni kabel koji otoke povezuje s ostatkom svijeta, a na ovim se fotografijama vide razmjeri
##  4:                                                                  Krase ga njegovani parkovi, gusta hladovina hrasta, crnike i borova i aromatično mediteransko raslinje uz šetnice i uređene plaže. <U+0001F305><U+0001F305> Resort privlači svojim lokalnim šarmom i toplim obiteljskim ugođajem kao i sjajnim zabavnim programom za djecu u
##  5:                                                                                        [PAMETNA RASVJETA ZA VAŠ SVAKODNEVNI ŽIVOT] WiZ pametne žarulje se uklapaju prilikom uređenja svakog doma, pogodni su za svaku priliku, bez obzira da li se radi o vašoj dnevnoj sobi, kuhinji, kupaonici pa čak i dvorištu. Pametnom rasvjetom možete
##  6: <U+0001F51D><U+0001F525>NOVA KOLEKCIJA<U+0001F525><U+0001F51D> Sako od umjetne kože dostupan u crnoj, smeđoj i bež boji. Online cijena 169.00 kn <U+0001F495> UNIVERZALNA VELIČINA Novu kolekciju naruči na našem web shopu Manilla.hr <U+0001F4E6> <U+0001F4B8> Cijena dostave je 30 kn <U+0001F552> Rok dostave 2-3 radna dana <U+0001F4E6>
##  7:                                                                                           U CENTRALU se grade brojni objekti društvene infrastrukture – međunarodna škola, privatna bolnica, nasip i trg, policija i vatrogasci, sportski klub, dva teniska terena, zatvoreni bazen, te promatračnica u južnom dijelu grada. selo. Restorani,
##  8:                                                                                     UNICEF je opremio 5 inkluzivnih kabineta na visokim učilištima u Zagrebu, Splitu, Osijeku, Puli i Rijeci. Ovom su inicijativom kauflandhrvatska i UNICEF, u suradnji s viskom učilištima, podržali unaprjeđenje kvalitete odgoja i obrazovanja sve djece,
##  9:                                          Danas još jedna objava, a onda na zasluženi odmor (pisanje kolokvija nije odmor <U+0001F605>). Neka ove godine vaš blagdanski stol bude ispunjen suosjećanjem i ukusnim veganskim jelima. <U+0001F331> Slavite život u kojem nitko ne stradava. <U+0001F423><U+0001F430><U+0001F338> Pinca je ispala
## 10:                                                                                                                                                                                   Za tebe smo dostupni na više digitalnih kanala podrške: WhatsAppu, Viberu i Webchatu! <U+0001F4AC> <U+27A1> Pronađite nas skeniranjem koda na web stranici.
##       REACH VIRALITY ENGAGEMENT_RATE INTERACTIONS FOLLOWERS_COUNT LIKE_COUNT
##  1: 1.3e+07       NA              NA      1934320               0          3
##  2: 2.5e+04       NA              NA         2535               0         24
##  3: 3.0e+06       NA              NA       300135               0        207
##  4: 1.3e+07       NA              NA      1934320               0          7
##  5: 1.3e+07       NA              NA      1934320               0          5
##  6: 1.3e+07       NA              NA      1934320               0          3
##  7: 1.3e+07       NA              NA      1934320               0          2
##  8: 1.3e+07       NA              NA      1934320               0         62
##  9: 1.3e+07       NA              NA      1934320               0          6
## 10: 4.6e+03       NA              NA          464               0          3
##     COMMENT_COUNT SHARE_COUNT TWEET_COUNT LOVE_COUNT WOW_COUNT HAHA_COUNT
##  1:             0          NA          NA         NA        NA         NA
##  2:            73          NA          NA         NA        NA         NA
##  3:             7          NA          NA         NA        NA         NA
##  4:             0          NA          NA         NA        NA         NA
##  5:             0          NA          NA         NA        NA         NA
##  6:             0          NA          NA         NA        NA         NA
##  7:             0          NA          NA         NA        NA         NA
##  8:             2          NA          NA         NA        NA         NA
##  9:             3          NA          NA         NA        NA         NA
## 10:             0          NA          NA         NA        NA         NA
##     SAD_COUNT ANGRY_COUNT TOTAL_REACTIONS_COUNT FAVORITE_COUNT RETWEET_COUNT
##  1:        NA          NA                    NA             NA            NA
##  2:        NA          NA                    NA             NA            NA
##  3:        NA          NA                    NA             NA            NA
##  4:        NA          NA                    NA             NA            NA
##  5:        NA          NA                    NA             NA            NA
##  6:        NA          NA                    NA             NA            NA
##  7:        NA          NA                    NA             NA            NA
##  8:        NA          NA                    NA             NA            NA
##  9:        NA          NA                    NA             NA            NA
## 10:        NA          NA                    NA             NA            NA
##     VIEW_COUNT DISLIKE_COUNT COMMENTS_COUNT LIKES DISLIKES COUNT REPOST_COUNT
##  1:          0            NA             NA    NA       NA    NA           NA
##  2:          0            NA             NA    NA       NA    NA           NA
##  3:          0            NA             NA    NA       NA    NA           NA
##  4:          0            NA             NA    NA       NA    NA           NA
##  5:          0            NA             NA    NA       NA    NA           NA
##  6:          0            NA             NA    NA       NA    NA           NA
##  7:          0            NA             NA    NA       NA    NA           NA
##  8:          0            NA             NA    NA       NA    NA           NA
##  9:          0            NA             NA    NA       NA    NA           NA
## 10:          0            NA             NA    NA       NA    NA           NA
##     REDDIT_TYPE REDDIT_SCORE INFLUENCE_SCORE TWEET_TYPE TWEET_SOURCE_NAME
##  1:        <NA>           NA               1       <NA>              <NA>
##  2:        <NA>           NA               1       <NA>              <NA>
##  3:        <NA>           NA               1       <NA>              <NA>
##  4:        <NA>           NA               1       <NA>              <NA>
##  5:        <NA>           NA               1       <NA>              <NA>
##  6:        <NA>           NA               1       <NA>              <NA>
##  7:        <NA>           NA               1       <NA>              <NA>
##  8:        <NA>           NA               1       <NA>              <NA>
##  9:        <NA>           NA               1       <NA>              <NA>
## 10:        <NA>           NA               1       <NA>              <NA>
##     TWEET_SOURCE_URL            DATETIME    LIKE COMMENT          word
##  1:             <NA> 2022-03-18 21:49:29 1762169  172151    pogledajte
##  2:             <NA> 2021-12-17 11:11:12     849    1686             a
##  3:             <NA> 2022-01-18 10:38:55  286741   13394          više
##  4:             <NA> 2022-03-01 15:11:16 1762169  172151             i
##  5:             <NA> 2022-01-19 12:24:23 1762169  172151  wifilighting
##  6:             <NA> 2022-03-04 09:03:04 1762169  172151   univerzalna
##  7:             <NA> 2022-03-10 18:17:00 1762169  172151             i
##  8:             <NA> 2021-11-29 14:11:07 1762169  172151        sadrže
##  9:             <NA> 2022-04-16 19:40:57 1762169  172151 karmelizirana
## 10:             <NA> 2022-02-16 13:00:17     433      31      stranici
```


### CLEAN


```r
# remove stop words, numbers, single letters, NA
insta %>% 
  anti_join(stop_corpus, by = "word") %>%
  mutate(word = gsub("\\d+", NA, word)) %>%
  mutate(word = gsub("^[a-zA-Z]$", NA, word)) -> insta_tokenTidy

insta_tokenTidy %>%
  filter(!is.na(word)) -> insta_tokenTidy


# check
insta_tokenTidy %>%
  sample_n(.,15) %>%
  datatable(., rownames = FALSE, options = list(pageLength = 5, scrollX=T))
```

```{=html}
<div id="htmlwidget-db9d6ba4494499dad58a" style="width:100%;height:auto;" class="datatables html-widget"></div>
<script type="application/json" data-for="htmlwidget-db9d6ba4494499dad58a">{"x":{"filter":"none","data":[["2021-11-21","2022-04-10","2022-01-23","2021-12-11","2022-03-25","2022-02-02","2022-01-18","2021-11-24","2022-01-21","2022-01-31","2022-02-10","2022-04-19","2022-03-01","2021-11-15","2022-04-04"],["18:16:29","09:00:58","00:21:35","10:00:49","12:28:13","10:59:32","09:07:22","23:20:03","10:57:17","19:27:50","11:14:15","08:21:04","08:04:01","23:12:56","11:56:24"],["ROBERTO CAVALLI PARADISO AZZURO EDP 75ml• Otvorite vrata tropskog raja i uživajte u savršenom ljetnom ozračju uz miris Roberto Cavalli Paradiso Azzurro . Miris je prestavljen 2016. godine, a svoju inspiraciju pronašao je u odrazima zlatne sunčeve svjetlosti nad azurnom površinom Sredozemlja. Bočica tirkizne boje plave boje sigurno će Vas...","Znate li kako ispravno 𝗽𝗿𝗶𝗺𝗶𝗷𝗲𝗻𝗶𝘁𝗶 𝘀𝗲𝗿𝘂𝗺?🤔 Hrani, hidratizira, štiti i daje boost našoj koži lica…nije ni čudo što mnoge od nas u svoju skincare rutine uvrštavaju 𝗦𝗘𝗥𝗨𝗠! Ipak, kako bismo dobili sve benefite koncentriranih aktivnih tvari, vrlo je važno serum koristiti na ispravan način. 💡Najvažnije je ga je primijeniti...","Nobody love Lexus like I do !!! #lexus #lexusis350 #lexuslove","📽️Film BLUE FUTURE su pogledale učenice i učenici mnogih srednjih škola diljem Hrvatske. Podijelili su s nama svoje dojmove. A što se vam najviše svidjelo? Ako još niste stigli pogledati, možda je ovaj vikend pravo vrijeme za jedan mediteranski dokumentarac. 📌I podsjećamo na BLUE TALK u utorak 14.12. u 18.30 sati putem Zooma. Gostuju...","Renault Scenic 1.5 dci 2013 god., 175000 km Uredno servisiran, veliki servis je upravo napravljen napravljen. Nešto od opreme: - Navigacija Tom-Tom - tempomat - automatska dvozonska klima - el. pod svih stakala - el. retrovizori - parkirni senzori - LED svijetla - radio cd-mp3, bluetooth - 6 brzina - abs, esp, itd..... Svako naše vozilo se prije...","📌 SOUL RED je snaga zdravlja, posebno stvorena u znanosti kako bi poboljšalo funkciju i performonse vašeg tijela. U samo 60 ml upakirali smo ogromnu količinu antioksidansa, vitamina i minerala te drugih hranjivih sastojoka za poticanje tijela. Vrlo je pitko i prikladno pakirano. Kako? SOUL RED je pun sjemenki i potpuno prirodan. 📌 Stvorili smo...","%%% Veliko sezonsko sniženje potražite u našoj trgovini Manilla fashion store %%% Sve artikle potražite i naručite na našoj web stranici Manilla.hr 📦 Proizvode šaljemo DPD brzom poštom 💸 Cijena dostave je 30 kn 🕒 Rok dostave 2-3 radna dana 💕 BESPLATNA POŠTARINA iznad 399 kn #manillagirl #fashion #fashionstore #moda #crofashion #croatia...","🌟🌟🌟ROYAL VELVET🌟🌟🌟 👉Royal Velvet dnevna krema za učvršćivanje kože sa SPF15 👉Učvršćava i jača kožu kod žena dobi 40+. 👉Intenzivno hidratantna formulacija obogaćena jedinstvenim crnim irisom čini kožu mekom, čvršćom i vidljivo podmlađenom. 👉Vidljivo umanjuje pojavu finih linija i bora. 👉Učvršćava kožu i definiše konture lica...","𝐙𝐚𝐫𝐨𝐥𝐚𝐧𝐢 𝐫𝐮𝐜̌𝐚𝐤 Šparoge, slanina i 25 g mliječnih proteina 💙 📜 𝑺𝒂𝒔𝒕𝒐𝒋𝒄𝒊 : 250 g šparoga 150 g tanko narezane slanina 125 g 𝑫𝒖𝒌𝒂𝒕 𝑭𝒊𝒕 𝑸𝒖𝒂𝒓𝒌𝒂 1 kašika maslinovog ulja 📜 𝑷𝒐𝒔𝒕𝒖𝒑𝒂𝒌: Šparoge operite i odstranite im donje tvrde dijelove. Na dasku paralelno složite po tri trakice tanko rezane slanine pa ih...","🚘AUDI A4 4G (2016MY) ⚙Odrađen veliki i mali servis, zamjenja prednjih i zadnjih pločica i obrada diskova🛠 Pozovite nas i zakažite Vaš termin📲066/156-977 Vidimo se👋 #audi #update_navi #chip_tuning #egr_off #dpf_off #igla #adblue_off #automechanic #autoelektrika #autoklime #flex #bosch_kts540 #magicmotorsport #iglasistem #autoelektronika...","[EKOSHIP 4 SALES ASSISTANT] 🚀 U sklopu Spring edition Ekoshipa ne tražimo samo pojačanja u obliku Software developera, već tražimo i asistenta/asistenticu u Sales timu. 👨 👩 Spoj IT&amp;sales za tebe je jednostavno neraskidiv kao GaryVee i \"hustle\"? Odgovor na pitanje \"Sell me this pen\" imaš u malom prstu? Po prirodi ti leži prodaja? Odlično!...","Ne čitam ništa danima i onda Šifru pročitam kroz svega par sati... 😍⁣ ⁣ Divno je kad se iz reading slumpa vratim u punom sjaju, a sve zahvaljujući dobrom odabiru knjige. Šifra me podsjetila na sve one silne serije što se vrte na TV-u, ali na dobar način. Kao što ne možete odvojiti oči s TV ekrana dok ne otkrijete tko je ubojica, tako ni s ovom...","Poručite masku sa željenom fotografijom ili posvetom za Vama dragu osobu , ili odaberite neki od naših dizajna.. 😎☺️ Naše maskice kreirane su od posebnog silikona koji čuva vaš uređaj i odlična su zaštita od eventualnog ispadanja telefona i svakodnevnog habanja. Pišite nam u DM ili poručite odmah putem vibera: 067/105-105 #maksicezatelefone...","Part 9 • Praktički sam sve zapisao i povezao od trenutka kad sam shvatio da instagram može biti odlična ‘reklama’ (VOLI BUDUĆNOST!) - kad ovi svjetovi bez pupoljaka zabrane put do Nje - pa joj onda ipak mogu reći: “Čuvaj se Vilo - kad nevrijeme oduzme Krila Mladosti - obično bude prekasno!” • Ali - hoću i dodati - dok je točno 30 godina Achtung...","🆕🆕🆕Novooo u Prokuliciiiii🆕🆕 Od 04.04. U Prokulici na adresi Mlinovi 96A možete nabaviti asortiman @siranavedrine 😍😍😍😍 •Hrvatski OPG koji se nalazi u srcu Like u selu Brezik svoj mliječni asortiman dobiva od Jersey krava koje su hranjene bez silaže s najkvalitetnijom hranom(sijenom i sjenažom te smjesom mljevenih žitarica i ekološkom..."],["anonymous_user","beautypharmacy_hr","anonymous_user","wwf_adria","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user"],["anonymous_user","beautypharmacy_hr","anonymous_user","wwf_adria","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user","anonymous_user"],["https://www.instagram.com/p/CWi8NzaMfVX","https://www.instagram.com/p/CcKVEDxqbV1/","https://www.instagram.com/p/CZDPSCPOW_X/","https://www.instagram.com/p/CXVjY27KL0Q/","https://www.instagram.com/p/Cbhm7fTMNCw/","https://www.instagram.com/p/CZeIPWKMz_U/","https://www.instagram.com/p/CY3Tenpstt_/","https://www.instagram.com/p/CWrNVu0s4FK","https://www.instagram.com/p/CY_OakZqqfR/","https://www.instagram.com/p/CZZ40agsV1l/","https://www.instagram.com/p/CZywSDYtVVe/","https://www.instagram.com/p/CchbptIMyE7/","https://www.instagram.com/p/CajVkvMj-4Y/","https://www.instagram.com/p/CWUBXt_MPY1","https://www.instagram.com/p/Cb7MXu6Mw3k/"],["https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png","https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png"],["instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram","instagram"],["Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka","Luka"],["opće","opće","opće","opće","opće","opće","opće","opće","opće","opće","opće","opće","opće","opće","opće"],["i","i","I","i, I","i","i","i","i","i","i","i","i","i","i","i"],["hr, bs","hr, bs","hr, sk","hr, bs","hr, bs","hr, bs","hr","hr, bs","hr, bs","hr, bs","hr, bs","hr, bs","hr, bs","hr, bs","hr, bs"],["HR, BA","HR","SK, HR","HR, BA","HR, BA","HR, BA","HR","HR, BA","HR, BA","HR, BA","HR, BA","HR, BA","HR, BA","HR, BA","HR, BA"],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],["positive","positive","neutral","neutral","neutral","positive","neutral","positive","positive","neutral","positive","negative","positive","positive","positive"],["•ROBERTO CAVALLI PARADISO AZZURO EDP 75ml• Otvorite vrata tropskog raja i uživajte u savršenom ljetnom ozračju uz miris Roberto Cavalli Paradiso Azzurro . Miris je prestavljen 2016. godine, a svoju inspiraciju pronašao je u odrazima zlatne sunčeve","Znate li kako ispravno 𝗽𝗿𝗶𝗺𝗶𝗷𝗲𝗻𝗶𝘁𝗶 𝘀𝗲𝗿𝘂𝗺?🤔 Hrani, hidratizira, štiti i daje boost našoj koži lica…nije ni čudo što mnoge od nas u svoju skincare rutine uvrštavaju 𝗦𝗘𝗥𝗨𝗠! Ipak, kako bismo dobili sve benefite koncentriranih","Nobody love Lexus like I do !!! #lexus #lexusis350 #lexuslove","📽️Film BLUE FUTURE su pogledale učenice i učenici mnogih srednjih škola diljem Hrvatske. Podijelili su s nama svoje ... 📌I podsjećamo na BLUE TALK u utorak 14.12. u 18.30 sati putem Zooma. Gostuju @marinamasanovic (znanstvenica),","Vozilo je moguće pogledati i izvan radnog vremena. Auto salon AK Selected nalazi se na adresi Radnička cesta 184. Važeća cijena je u kunama i odnosi se na gotovinsko plaćanje, cijena u eurima je informativna. NAČINI PLAĆANJA: Gotovina,","📌 SOUL RED je snaga zdravlja, posebno stvorena u znanosti kako bi poboljšalo funkciju i performonse vašeg tijela. U samo 60 ml upakirali smo ogromnu količinu antioksidansa, vitamina i minerala te drugih hranjivih sastojoka za poticanje tijela. Vrlo","%%% Veliko sezonsko sniženje potražite u našoj trgovini Manilla fashion store %%% Sve artikle potražite i naručite na našoj web stranici Manilla.hr 📦 Proizvode šaljemo DPD brzom poštom 💸 Cijena dostave je 30 kn 🕒 Rok dostave 2-3 radna dana","🌟🌟🌟ROYAL VELVET🌟🌟🌟 👉Royal Velvet dnevna krema za učvršćivanje kože sa SPF15 👉Učvršćava i jača kožu kod žena dobi 40+. 👉Intenzivno hidratantna formulacija obogaćena jedinstvenim crnim irisom čini kožu mekom, čvršćom i vidljivo podmlađenom.","𝐙𝐚𝐫𝐨𝐥𝐚𝐧𝐢 𝐫𝐮𝐜̌𝐚𝐤 Šparoge, slanina i 25 g mliječnih proteina 💙 📜 𝑺𝒂𝒔𝒕𝒐𝒋𝒄𝒊 : 250 g šparoga 150 g tanko narezane slanina 125 g 𝑫𝒖𝒌𝒂𝒕 𝑭𝒊𝒕 𝑸𝒖𝒂𝒓𝒌𝒂 1 kašika maslinovog ulja 📜 𝑷𝒐𝒔𝒕𝒖𝒑𝒂𝒌: Šparoge operite i","🚘AUDI A4 4G (2016MY) ⚙Odrađen veliki i mali servis, zamjenja prednjih i zadnjih pločica i obrada diskova🛠 Pozovite nas i zakažite Vaš termin📲066/156-977 Vidimo se👋 #audi #update_navi #chip_tuning #egr_off #dpf_off #igla #adblue_off","[EKOSHIP 4 SALES ASSISTANT] 🚀 U sklopu Spring edition Ekoshipa ne tražimo samo pojačanja u obliku Software developera, već tražimo i asistenta/asistenticu u Sales timu. 👨 👩 Spoj IT&amp;sales za tebe je jednostavno neraskidiv kao GaryVee i \"hustle\"?","Ne čitam ništa danima i onda Šifru pročitam kroz svega par sati... 😍⁣ ⁣ Divno je kad se iz reading slumpa vratim u punom sjaju, a sve zahvaljujući dobrom odabiru knjige. Šifra me podsjetila na sve one silne serije što se vrte na TV-u, ali na dobar","😎☺️ Naše maskice kreirane su od posebnog silikona koji čuva vaš uređaj i odlična su zaštita od eventualnog ispadanja telefona i svakodnevnog habanja. Pišite nam u DM ili poručite odmah putem vibera: 067/105-105 #maksicezatelefone #maske #friends","Part 9 • Praktički sam sve zapisao i povezao od trenutka kad sam shvatio da instagram može biti odlična ‘reklama’ (VOLI BUDUĆNOST!) - kad ovi svjetovi bez pupoljaka zabrane put do Nje - pa joj onda ipak mogu reći: “Čuvaj se Vilo - kad nevrijeme","U Prokulici na adresi Mlinovi 96A možete nabaviti asortiman @siranavedrine 😍😍😍😍 •Hrvatski OPG koji se nalazi u srcu Like u selu Brezik svoj mliječni asortiman dobiva od Jersey krava koje su hranjene bez silaže s najkvalitetnijom hranom(sijenom i"],[12733896,44300,12733896,26470,12733896,12733896,12733896,12733896,12733896,12733896,12733896,12733896,12733896,12733896,12733896],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[1934320,4430,1934320,2658,1934320,1934320,1934320,1934320,1934320,1934320,1934320,1934320,1934320,1934320,1934320],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[0,1,4,33,2,1,1,6,23,18,4,-1,1,5,14],[0,0,2,0,0,1,0,0,0,0,0,2,0,0,1],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[1,1,1,1,1,1,1,1,1,1,1,1,1,1,1],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],[null,null,null,null,null,null,null,null,null,null,null,null,null,null,null],["2021-11-21T17:16:29Z","2022-04-10T07:00:58Z","2022-01-22T23:21:35Z","2021-12-11T09:00:49Z","2022-03-25T11:28:13Z","2022-02-02T09:59:32Z","2022-01-18T08:07:22Z","2021-11-24T22:20:03Z","2022-01-21T09:57:17Z","2022-01-31T18:27:50Z","2022-02-10T10:14:15Z","2022-04-19T06:21:04Z","2022-03-01T07:04:01Z","2021-11-15T22:12:56Z","2022-04-04T09:56:24Z"],[1762169,4159,1762169,2626,1762169,1762169,1762169,1762169,1762169,1762169,1762169,1762169,1762169,1762169,1762169],[172151,271,172151,32,172151,172151,172151,172151,172151,172151,172151,172151,172151,172151,172151],["boje","vremena","nobody","morskog","zagrebačka","napraviti","sniženje","umanjuje","pečeno","zamjenja","leži","samog","nikicdigital","istog","asortiman"]],"container":"<table class=\"display\">\n  <thead>\n    <tr>\n      <th>DATE<\/th>\n      <th>TIME<\/th>\n      <th>TITLE<\/th>\n      <th>FROM<\/th>\n      <th>AUTHOR<\/th>\n      <th>URL<\/th>\n      <th>URL_PHOTO<\/th>\n      <th>SOURCE_TYPE<\/th>\n      <th>GROUP_NAME<\/th>\n      <th>KEYWORD_NAME<\/th>\n      <th>FOUND_KEYWORDS<\/th>\n      <th>LANGUAGES<\/th>\n      <th>LOCATIONS<\/th>\n      <th>TAGS<\/th>\n      <th>MANUAL_SENTIMENT<\/th>\n      <th>AUTO_SENTIMENT<\/th>\n      <th>MENTION_SNIPPET<\/th>\n      <th>REACH<\/th>\n      <th>VIRALITY<\/th>\n      <th>ENGAGEMENT_RATE<\/th>\n      <th>INTERACTIONS<\/th>\n      <th>FOLLOWERS_COUNT<\/th>\n      <th>LIKE_COUNT<\/th>\n      <th>COMMENT_COUNT<\/th>\n      <th>SHARE_COUNT<\/th>\n      <th>TWEET_COUNT<\/th>\n      <th>LOVE_COUNT<\/th>\n      <th>WOW_COUNT<\/th>\n      <th>HAHA_COUNT<\/th>\n      <th>SAD_COUNT<\/th>\n      <th>ANGRY_COUNT<\/th>\n      <th>TOTAL_REACTIONS_COUNT<\/th>\n      <th>FAVORITE_COUNT<\/th>\n      <th>RETWEET_COUNT<\/th>\n      <th>VIEW_COUNT<\/th>\n      <th>DISLIKE_COUNT<\/th>\n      <th>COMMENTS_COUNT<\/th>\n      <th>LIKES<\/th>\n      <th>DISLIKES<\/th>\n      <th>COUNT<\/th>\n      <th>REPOST_COUNT<\/th>\n      <th>REDDIT_TYPE<\/th>\n      <th>REDDIT_SCORE<\/th>\n      <th>INFLUENCE_SCORE<\/th>\n      <th>TWEET_TYPE<\/th>\n      <th>TWEET_SOURCE_NAME<\/th>\n      <th>TWEET_SOURCE_URL<\/th>\n      <th>DATETIME<\/th>\n      <th>LIKE<\/th>\n      <th>COMMENT<\/th>\n      <th>word<\/th>\n    <\/tr>\n  <\/thead>\n<\/table>","options":{"pageLength":5,"scrollX":true,"columnDefs":[{"className":"dt-right","targets":[17,18,19,20,21,22,23,24,26,27,28,29,30,31,32,33,34,35,36,42,43,48,49]}],"order":[],"autoWidth":false,"orderClasses":false,"lengthMenu":[5,10,25,50,100]}},"evals":[],"jsHooks":[]}</script>
```


# ANALYSE


### BASIC FREQUENCIES

```r
## Most common words
printMD(insta_tokenTidy[,.N,by = word][order(-N),][N > 1500], big.mark=",")
```


------------------------
     word          N    
--------------- --------
    akcija       10,166 

     dana        5,617  

     kože        5,587  

    vrijeme      5,452  

     love        4,930  

    možete       4,918  

    croatia      4,762  

     moda        4,510  

     kožu        4,354  

     https       4,223  

    zagreb       4,191  

      kn         4,132  

    fashion      3,998  

    majice       3,766  

   hrvatska      3,713  

     preko       3,599  

     ovaj        3,454  

     danas       3,454  

    poklon       3,405  

     nove        3,334  

    ponuda       3,275  

      dan        3,174  

     malo        3,111  

 onlineprodaja   2,995  

    popust       2,977  

    cijena       2,905  

     brzo        2,812  

     radno       2,792  

    popusti      2,739  

   snizenje      2,685  

      dm         2,649  

    ponudi       2,563  

     lako        2,528  

      web        2,514  

     link        2,511  

   cijenama      2,495  

    sigurno      2,493  

     opisu       2,477  

    uskoro       2,461  

    gratis       2,444  

    profila      2,440  

    zaliha       2,416  

    beauty       2,405  

     osobu       2,326  

     našem       2,312  

    online       2,305  

   požurite      2,296  

     putem       2,290  

    odličan      2,286  

   skincare      2,276  

    uvijek       2,260  

     broj        2,241  

   poručite      2,184  

   instagram     2,178  

    godina       2,159  

     budi        2,140  

    haljine      2,135  

     krema       2,074  

    popusta      2,046  

    dostava      2,044  

   proizvode     2,031  

     dobro       2,026  

  pogodnosti     2,016  

    antiage      2,010  

   kvalitet      2,003  

      the        1,977  

     svaki       1,950  

     jakne       1,943  

  beautyqueen    1,934  

     dragu       1,923  

     važi        1,921  

     butik       1,914  

   obradujte     1,891  

     traje       1,888  

   isporuka      1,876  

   povoljno      1,864  

    zamjene      1,862  

    sjajnim      1,849  

     novo        1,838  

   kompleti      1,836  

   trenerke      1,832  

   pantalone     1,829  

    kaputi       1,811  

     style       1,810  

   mogucnost     1,804  

     kupuj       1,800  

    suknje       1,800  

   proizvoda     1,799  

     koži        1,799  

 zenskaodjeca    1,798  

   garderoba     1,798  

   dzemperi      1,796  

    protiv       1,793  

     nasoj       1,792  

   postarina     1,788  

   ocekujte      1,779  

   porudzbe      1,777  

    istjeka      1,774  

   kardigani     1,762  

     budva       1,761  

     shop        1,743  

     hlače       1,738  

     food        1,732  

     novi        1,726  

    naruči       1,714  

      and        1,710  

     nova        1,705  

     lica        1,690  

    dizajn       1,685  

     split       1,677  

    helanke      1,671  

     koža        1,671  

     bluza       1,660  

   instagood     1,643  

     model       1,634  

  montenegro     1,620  

     super       1,620  

   narudžbe      1,615  

   blejzeri      1,610  

    duxevi       1,609  

      sea        1,602  

      dio        1,599  

    minuta       1,585  

     ulje        1,556  

   besplatna     1,548  

    recept       1,546  

  photography    1,545  

     ulja        1,530  

     način       1,525  

     puno        1,520  

    sadrži       1,520  

   podgorica     1,514  

      cm         1,511  

      of         1,510  
------------------------

```r
## Vizualize most common words
insta_tokenTidy[,.N,by = word][N>2000][order(-N),][,word := reorder(word,N)] %>%
  ggplot(aes(word, N)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

```r
## Vizualize most common words over time
insta_tokenTidy[,DAY:=floor_date(DATE,"day")][,N:=.N,by=DAY][,gn:=sum(N)][
  word %in% c("akcija", "ponuda", "poklon", "gratis", "kože"),] %>%
  ggplot(., aes(DAY,  N / gn)) + 
   geom_point() +
   ggtitle("Učestalost korištenja riječi") +
   ylab("% ukupnih riječi") +
   geom_smooth() +
   facet_wrap(~ word, scales = "free_y") +
   scale_y_continuous(labels = scales::percent_format())+
   theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/unnamed-chunk-5-2.png)<!-- -->


```r
## Articles per influnecer
#printMD(insta[ ,.N,FROM][N>1500][order(-N)], big.mark=",")


## Articles per domain over time

insta_tokenTidy %>% 
   filter(word == "poklon" & FROM %in% c("elladvornik", "beautypharmacy_hr", "lucija_lugomer_official")) %>%
   mutate(WEEK = floor_date(DATE, "week")) %>%
   group_by(WEEK, FROM) %>%
   summarise(n = n()) %>%
   ungroup() %>%
   arrange(desc(n)) %>%
   ggplot(., aes(WEEK,  n)) + 
   geom_line() +
   ggtitle("Članci o poklonima na najvažnijim IG profilima") +
   ylab("Broj objava") +
   geom_smooth() +
   facet_wrap(~ FROM, scales = "free_y") +
   theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/dekriptivnoDom-1.png)<!-- -->


### SENTIMENT


```r
## Sentiment over time
vizualiziraj_sentiment <- function(dataset, frq = "week") {
dataset %>%
  inner_join( Crosentilex_sve, by = "word") %>%
  filter(!is.na(word)) %>%
  select(word, brija, DATE, sentiment) %>% 
  unique() %>%
  spread(. , brija, sentiment) %>%
  mutate(sentiment = POZ - NEG) %>%
  select(word, DATE, sentiment) %>% 
  group_by(word) %>% 
  mutate(count = n()) %>%
  arrange(desc(count)) %>%
  mutate( score = sentiment*count) %>%
  ungroup() %>%
  group_by(DATE) %>%
  arrange(desc(DATE)) -> sm
 
sm %>%
  select(DATE, score) %>%
  group_by(DATE = floor_date(DATE, frq)) %>%
  summarise(Dnevni_sent = sum(score, na.rm = TRUE)) %>%
  ggplot(., aes(DATE, Dnevni_sent)) +
  geom_bar(stat = "identity") + 
  ggtitle(paste0("Sentiment over time;freqency:", frq)) +
  ylab("SentimentScore") +
  theme_economist()-> gg_sentiment_kroz_vrijeme_qv
gg_sentiment_kroz_vrijeme_qv
}
vizualiziraj_sentiment(insta_tokenTidy,"week")
```

![](Lecture-7-Text-analysis_files/figure-html/sentimentTempus-1.png)<!-- -->




```r
## Sentiment 
doprinos_sentimentu <- function(dataset, no = n) {
dataset %>%
  inner_join(CroSentilex_Gold, by = "word") %>% 
  count(word, sentiment,sort = TRUE) %>% 
  group_by(sentiment) %>%
  top_n(no) %>%
  ungroup() %>%
  mutate(sentiment = case_when(sentiment == 0 ~ "NEUTRAL",
                                 sentiment == 1 ~ "NEGATIVE",
                                 sentiment == 2 ~ "POSITIVE")) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  ggtitle( "Sentiment") +
  labs( x = "Riječ", y = "Number of words") +
  facet_wrap(~ sentiment, scales = "free_y") +
  coord_flip() +
  theme_economist() -> gg_doprinos_sentimentu
  
 gg_doprinos_sentimentu
 
}
doprinos_sentimentu(insta_tokenTidy,15)
```

![](Lecture-7-Text-analysis_files/figure-html/doprinoSentimentu-1.png)<!-- -->



Simple WordCloud:


```r
## WordCloud(vulgaris)
insta_tokenTidy %>%
  anti_join(CroSentilex_Gold,by="word") %>% 
  count(word) %>% 
  arrange(desc(n)) %>%
  top_n(100) %>%
  with(wordcloud(word, n, max.words = 80)) 
```

![](Lecture-7-Text-analysis_files/figure-html/WCloud-1.png)<!-- -->

WordCloud sentimenta:


```r
## ComparisonCloud
insta_tokenTidy %>%
  inner_join(CroSentilex_Gold,by="word") %>% 
  count(word, sentiment) %>% 
  top_n(200) %>%
  mutate(sentiment = case_when(sentiment == 0 ~ "+/-",
                                 sentiment == 1 ~ "-",
                                 sentiment == 2 ~ "+")) %>%
  acast(word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("firebrick3", "deepskyblue3","darkslategray"),
                   max.words = 120)
```

![](Lecture-7-Text-analysis_files/figure-html/WCloutSent-1.png)<!-- -->


Negativity analysis of influencers:


```r
## Najnegativniji portali
wCount <- insta_tokenTidy %>% 
  group_by(FROM) %>%
  summarise(word = n())
CroSentilex_Gold_neg <- CroSentilex_Gold %>% filter(sentiment == 1)
CroSentilex_Gold_poz <- CroSentilex_Gold %>% filter(sentiment == 2)
insta_tokenTidy %>% 
  semi_join(CroSentilex_Gold_neg, by= "word") %>%
  group_by(FROM) %>% 
  summarise(negWords = n()) %>%
  left_join(wCount, by = "FROM") %>%
  mutate(negativnostIndex = (negWords/word)*100) %>%
  arrange(desc(negativnostIndex)) %>%
  printMD(., big.mark=",")
```


--------------------------------------------------------------------------
              FROM                negWords     word      negativnostIndex 
-------------------------------- ---------- ----------- ------------------
         apoteka_cvejic              2          68            2.941       

          erste_banka                2          68            2.941       

        patriciavodopija             1          45            2.222       

            psidacta                 1          45            2.222       

       galenpharmapoteke             1          47            2.128       

            reci.hr                  1          47            2.128       

        viktorija.radja              1          48            2.083       

         mirna_medstep               3          206           1.456       

          knjazrobert                3          217           1.382       

           dnevnikhr                142       10,748          1.321       

             avonba                  13        1,062          1.224       

          martina_boss               3          253           1.186       

       erstebankacrnagora            1          89            1.124       

          mandis_pharm               2          184           1.087       

         tatjana_juric               1          95            1.053       

          jutarnji.hr               286       27,623          1.035       

            combis_                  3          299           1.003       

          telegram.hr                4          403           0.9926      

           24sata.hr                 38        3,894          0.9759      

        ljekarna_marusic             22        2,274          0.9675      

           ivansaric                 3          323           0.9288      

          marko.mrkic                1          113           0.885       

         malajskitapir               3          342           0.8772      

           safariduha                12        1,369          0.8766      

          avonhrvatska               14        1,616          0.8663      

           mirnamaras                1          123           0.813       

           balkan_bet                2          250            0.8        

         ivicakostelic               4          506           0.7905      

     tarikfilipovicofficial          3          445           0.6742      

         maxfactor_bih               49        7,290          0.6722      

         belamakeuplada              1          154           0.6494      

          ecijaojdanic               6          925           0.6486      

          lukanizetic                4          618           0.6472      

          putoholicari               12        1,880          0.6383      

           usa_in_mne                1          161           0.6211      

         vecernji.list               97       15,892          0.6104      

           ninabljak                 10        1,657          0.6035      

          petrakurtela               5          836           0.5981      

         pamelaramljak               3          503           0.5964      

         ivana_miseric               1          175           0.5714      

        telemachcrnagora             2          360           0.5556      

       maxfactor_hrvatska            6         1,102          0.5445      

          eapoteka.rs                1          188           0.5319      

       ljekarnajelichrgic            4          765           0.5229      

            rkzagreb                 16        3,109          0.5146      

          josipa_lisac               5         1,007          0.4965      

          matea.miljan               1          207           0.4831      

         cvit_likarije               7         1,529          0.4578      

          ida_prester                6         1,330          0.4511      

       anamarasharmander             1          224           0.4464      

           mojbonbon                 4          905           0.442       

          nanulananula               4          908           0.4405      

           wwf_adria                 12        2,724          0.4405      

        mashinthebeauty              4          917           0.4362      

         discus70queen               2          489           0.409       

        ljupka_tanevska              7         1,715          0.4082      

          renatasopek                2          492           0.4065      

         apotekalaurus               2          496           0.4032      

         andreaandrassy              9         2,318          0.3883      

         mylittlezagreb              1          267           0.3745      

         ljekarna.pablo              12        3,235          0.3709      

           markodrcic                3          831           0.361       

        danijeladvornik              4         1,123          0.3562      

            skitnica                 19        5,404          0.3516      

         antonijablace               3          878           0.3417      

           lana_gojak                1          297           0.3367      

    ljekarnanaglicsostarcic          2          598           0.3344      

          slavkosobin                4         1,205          0.332       

           rtl.danas                 3          906           0.3311      

        apoteka_anapharm             4         1,233          0.3244      

          ivanablazoti               1          310           0.3226      

          iva___radic                1          311           0.3215      

         vandawinter148              1          311           0.3215      

           cerave_ba                 4         1,257          0.3182      

         pliva_hrvatska              4         1,259          0.3177      

            nivea_hr                 3          968           0.3099      

         moja_farmacia               8         2,601          0.3076      

         meridianbet_rs              1          326           0.3067      

         korana_marovic              11        3,597          0.3058      

        llums_cosmetics              1          334           0.2994      

           bojana.g.v                1          335           0.2985      

           click4chic                1          335           0.2985      

          thompson_hr                1          338           0.2959      

          davorgerbus                1          342           0.2924      

          mojaapoteka                1          343           0.2915      

         lilly_drogerie              1          354           0.2825      

         ornelavistica               3         1,071          0.2801      

          drmaxapoteka               2          718           0.2786      

             rtl_hr                  8         2,892          0.2766      

            severina                 1          364           0.2747      

          superapoteka               9         3,291          0.2735      

          pharmacy_bcc               11        4,077          0.2698      

        apoteke_b_pharm              6         2,226          0.2695      

           boombox.hr                1          375           0.2667      

          nivescelsius               1          378           0.2646      

          telemach.hr                13        4,958          0.2622      

         nevenarendeli               5         1,913          0.2614      

             iwaiva                  6         2,323          0.2583      

           damirkedzo                1          395           0.2532      

          maremikulic                3         1,192          0.2517      

          elladvornik                7         2,834          0.247       

             mtelcg                  1          411           0.2433      

          zoransprajc                2          878           0.2278      

          miffyandrea                6         2,729          0.2199      

              hana                   7         3,203          0.2185      

        marijanabatinic              4         1,833          0.2182      

         anonymous_user            5,291     2,432,632        0.2175      

          indiralevak                1          472           0.2119      

           teamiskon                 6         2,838          0.2114      

         bornabutijer5               3         1,431          0.2096      

       beautypharmacy_hr             76       38,753          0.1961      

         optima.telekom              5         2,589          0.1931      

 savez_elektronskih_sportova_cg      1          518           0.1931      

        slavica.de.jong              2         1,063          0.1881      

         ceravehrvatska              4         2,246          0.1781      

          uvijekgladna               3         1,691          0.1774      

            ormarija                 2         1,146          0.1745      

          slakipalaki                2         1,210          0.1653      

       ninasliskovicgoles            4         2,547          0.157       

         eljekarna24.hr              3         1,949          0.1539      

         lanatheklingor              3         1,971          0.1522      

        hrvatski.telekom             1          660           0.1515      

    lucija_lugomer_official          6         4,053          0.148       

     ljekarneprimafarmacia           3         2,052          0.1462      

       dashofblue_makeup             1          705           0.1418      

           anaradisic                2         1,411          0.1417      

          boltfood_hr                1          718           0.1393      

          lidijalesic                1          768           0.1302      

         a1hrvatskalife              3         2,476          0.1212      

         apoteka_monis               9         7,939          0.1134      

       ljekarnaonline.hr             3         2,858          0.105       

     ljekarne_vase_zdravlje          1          970           0.1031      

       ave_dulcis_tamara             2         2,067         0.09676      

        oriflame_croatia             2         2,295         0.08715      

           farmasibh                 1         1,229         0.08137      

           sarashine_                1         1,312         0.07622      

         tvojaljekarna               12       16,521         0.07263      

         domacica.sanja              2         2,819         0.07095      

         projekt_ilica               3         4,432         0.06769      

          mojaljekarna               1         1,524         0.06562      

         more_less_ines              2         3,121         0.06408      

        mali_muzej_cuda              2         4,145         0.04825      

       cromaris_hrvatska             1         2,156         0.04638      
--------------------------------------------------------------------------

...also positivity:


```r
## Najpozitivniji portali
CroSentilex_Gold_poz <- CroSentilex_Gold %>% filter(sentiment == 2)
insta_tokenTidy %>% 
  semi_join(CroSentilex_Gold_poz, by= "word") %>%
  group_by(FROM) %>% 
  summarise(pozWords = n()) %>%
  left_join(wCount, by = "FROM") %>%
  mutate(pozitivnostIndex = (pozWords/word)*100) %>%
  arrange(desc(pozitivnostIndex)) %>%
  printMD(., big.mark=",") 
```


---------------------------------------------------------------------
           FROM              pozWords     word      pozitivnostIndex 
--------------------------- ---------- ----------- ------------------
     vegafarmaapoteke           3          19            15.79       

       daniella.ods             2          15            13.33       

 imerovic_sandi_budivelik       2          16             12.5       

         nikailcic              2          19            10.53       

          postacg               3          35            8.571       

        ljekarna.hr             2          25              8         

      frankaofficial_           4          53            7.547       

      adrianadurdevic           6          84            7.143       

         chilli_di              6          87            6.897       

        safariduha              79        1,369          5.771       

      ljekarnepharmad           2          35            5.714       

       barbarakolar             1          18            5.556       

      prima_namjestaj           23         433           5.312       

  ljekarne_vase_zdravlje        51         970           5.258       

          jelka12               12         231           5.195       

       ivana_miseric            9          175           5.143       

      anabegictahiri            4          78            5.128       

        marko.tolja             17         339           5.015       

        ideasrbija              9          182           4.945       

      mylittlezagreb            13         267           4.869       

       mirna_medstep            10         206           4.854       

       katjagrudnik             1          21            4.762       

      generalisrbija            3          67            4.478       

        anaradisic              62        1,411          4.394       

         anaviglam              1          23            4.348       

      minea.official            4          92            4.348       

        a1hrvatska              7          162           4.321       

        tinsedlar88             4          95            4.211       

      hanii_official            2          48            4.167       

         lorainne7              1          24            4.167       

        mirnamaras              5          123           4.065       

        damirkedzo              16         395           4.051       

          mm.mia1               2          51            3.922       

       josipa_lisac             39        1,007          3.873       

         severina               14         364           3.846       

          combis_               11         299           3.679       

         meggle_hr              6          168           3.571       

   ericsson_nikola_tesla        13         365           3.562       

     mario_petrekovic           38        1,078          3.525       

         majasuput              11         313           3.514       

       discus70queen            17         489           3.476       

      sementa_rajhard           24         692           3.468       

      lipalek_apoteka           1          29            3.448       

       nivescelsius             13         378           3.439       

         ninabljak              56        1,657           3.38       

        lidlsrbija              8          242           3.306       

         dorica505              5          153           3.268       

       ornelavistica            35        1,071          3.268       

 triglav.osiguranje.srbija      1          31            3.226       

       maja_bajamic             17         536           3.172       

       petrakurtela             25         836            2.99       

      petra_miksikova           10         336           2.976       

       dejanlovren06            1          34            2.941       

        erste_banka             2          68            2.941       

        mojaapoteka             10         343           2.915       

        lutrija.hr              6          207           2.899       

      vandawinter148            9          311           2.894       

      lanatheklingor            57        1,971          2.892       

        five.agency             1          35            2.857       

       stitnjaca.hr             13         460           2.826       

        balkan_bet              7          250            2.8        

       bornabutijer5            40        1,431          2.795       

       avonhrvatska             45        1,616          2.785       

     telemachcrnagora           10         360           2.778       

        maremikulic             33        1,192          2.768       

      ljupka_tanevska           47        1,715          2.741       

        zoransprajc             24         878           2.733       

        anabacinger             11         404           2.723       

     veronika_rosandic          5          184           2.717       

        bojana.g.v              9          335           2.687       

          mtelcg                11         411           2.676       

         narod.hr               2          75            2.667       

        marko.mrkic             3          113           2.655       

        boltfood_hr             19         718           2.646       

        slakipalaki             32        1,210          2.645       

  antonija_stupar_jurkin        4          152           2.632       

      thecircuitmess            14         533           2.627       

       antonijablace            23         878            2.62       

       markogrubnic             10         383           2.611       

      rene_bitorajac            13         502            2.59       

         wwf_adria              70        2,724           2.57       

        indiralevak             12         472           2.542       

      slavica.de.jong           27        1,063           2.54       

       elaajerkovic             2          79            2.532       

         altabanka              1          41            2.439       

         rkzagreb               75        3,109          2.412       

        stazisweets             8          334           2.395       

       ecijaojdanic             22         925           2.378       

    maxfactor_hrvatska          26        1,102          2.359       

       nevenarendeli            45        1,913          2.352       

       andreafabricc            3          128           2.344       

       malajskitapir            8          342           2.339       

        ida_prester             31        1,330          2.331       

      croatiacontrol            1          43            2.326       

      mirjana_mikulec           18         785           2.293       

        jglobitelj              28        1,227          2.282       

        mr.moncina              13         570           2.281       

         24sata.hr              88        3,894           2.26       

          lalana                15         666           2.252       

        iva___radic             7          311           2.251       

    erstebankacrnagora          2          89            2.247       

    ninasliskovicgoles          57        2,547          2.238       

         phama.hr               9          403           2.233       

     anamarasharmander          5          224           2.232       

        inacrnagora             10         448           2.232       

       drmaxapoteka             16         718           2.228       

      ljekarna.pablo            72        3,235          2.226       

         brita0507              3          135           2.222       

        telemach.hr            110        4,958          2.219       

         rtl.danas              20         906           2.208       

       nanulananula             20         908           2.203       

       pamelaramljak            11         503           2.187       

     maxbetkladionice           11         510           2.157       

        elladvornik             61        2,834          2.152       

      a1hrvatskalife            53        2,476          2.141       

     dashofblue_makeup          15         705           2.128       

     galenpharmapoteke          1          47            2.128       

          reci.hr               1          47            2.128       

     apoteka_anapharm           26        1,233          2.109       

        august_xvi              13         623           2.087       

         dnevnikhr             223       10,748          2.075       

         farmasihr              35        1,706          2.052       

        renatasopek             10         492           2.033       

  tarikfilipovicofficial        9          445           2.022       

       projekt_ilica            89        4,432          2.008       

     oriflame_croatia           46        2,295          2.004       

      vern_university           13         652           1.994       

        slavkosobin             24        1,205          1.992       

         farmasirs              12         604           1.987       

      pliva_hrvatska            25        1,259          1.986       

      apoteke_b_pharm           44        2,226          1.977       

     hrvatski.telekom           13         660            1.97       

      danijeladvornik           22        1,123          1.959       

         farmasibh              24        1,229          1.953       

     srbotrade_apoteka          3          155           1.935       

     ljekarna_marusic           44        2,274          1.935       

      ceravehrvatska            43        2,246          1.915       

        sarashine_              25        1,312          1.905       

          mtelbh                7          368           1.902       

      optima.telekom            49        2,589          1.893       

       ljekarnatalan            13         689           1.887       

          avonba                20        1,062          1.883       

       vecernji.list           296       15,892          1.863       

           hana                 59        3,203          1.842       

      meridianbet_rs            6          326            1.84       

  ljekarnanaglicsostarcic       11         598           1.839       

    ljekarnajelichrgic          14         765            1.83       

     podravka_hrvatska          16         878           1.822       

        markodrcic              15         831           1.805       

        jutarnji.hr            497       27,623          1.799       

        lukanizetic             11         618            1.78       

       ivicakostelic            9          506           1.779       

        thompson_hr             6          338           1.775       

       cvit_likarije            27        1,529          1.766       

        davorgerbus             6          342           1.754       

        aboutyou_hr             5          286           1.748       

      mali_muzej_cuda           72        4,145          1.737       

          rtl_hr                50        2,892          1.729       

      oaza_zdravlja_            1          58            1.724       

         sasalozar              12         696           1.724       

     cromaris_hrvatska          37        2,156          1.716       

      mario.valentic            3          175           1.714       

      stjepanowskaja            2          117           1.709       

        centar.cesi             2          118           1.695       

        tportal.hr              6          354           1.695       

        miffyandrea             46        2,729          1.686       

        lana_gojak              5          297           1.684       

      danijel.subasic           2          120           1.667       

      dukat_hrvatska            8          480           1.667       

      more_less_ines            52        3,121          1.666       

 oriflamebosnaihercegovina      15         904           1.659       

       soccerbet.rs             2          123           1.626       

        boombox.hr              6          375            1.6        

        eapoteka.rs             3          188           1.596       

       martina_boss             4          253           1.581       

     mariahs_chambers           5          320           1.562       

         nivea_hr               15         968            1.55       

      anonymous_user          37,379    2,432,632        1.537       

  lucija_lugomer_official       61        4,053          1.505       

     ljekarnaonline.hr          43        2,858          1.505       

      eljekarna24.hr            29        1,949          1.488       

   ljekarneprimafarmacia        30        2,052          1.462       

       maxfactor_bih           106        7,290          1.454       

       katarinababan            11         763           1.442       

      marijanabatinic           26        1,833          1.418       

         zanamari               4          285           1.404       

       superapoteka             46        3,291          1.398       

      varteks.fashion           26        1,871           1.39       

        suzana_emes             6          435           1.379       

      mercator_centar           4          294           1.361       

       pharmacy_bcc             55        4,077          1.349       

        emaluketin              3          227           1.322       

       mojaljekarna             20        1,524          1.312       

          iwaiva                30        2,323          1.291       

     beautypharmacy_hr         491       38,753          1.267       

      andreaandrassy            29        2,318          1.251       

         ivansaric              4          323           1.238       

        marinamamic             1          81            1.235       

       koranagvozdic            5          408           1.225       

      korana_marovic            44        3,597          1.223       

      mashinthebeauty           11         917            1.2        

         cerave_ba              15        1,257          1.193       

 beslagicenis_jedini_pravi      1          84             1.19       

        mozzartbet              1          84             1.19       

        lidijalesic             9          768           1.172       

       apoteka_monis            91        7,939          1.146       

      lilly_drogerie            4          354            1.13       

        podravkacg              9          798           1.128       

         paulasik               6          545           1.101       

       mandis_pharm             2          184           1.087       

       putoholicari             20        1,880          1.064       

       tatjana_juric            1          95            1.053       

      oriflamesrbija            2          199           1.005       

      jelenamarinovic           7          699           1.001       

      dorinlucissima            1          100             1         

         skitnica               54        5,404          0.9993      

       ivanablazoti             3          310           0.9677      

         ormarija               11        1,146          0.9599      

      doris.stankovic           4          433           0.9238      

         teamiskon              26        2,838          0.9161      

        click4chic              3          335           0.8955      

       moja_farmacia            23        2,601          0.8843      

       wolt.croatia             5          590           0.8475      

       ceravesrbija             3          366           0.8197      

          tojevuk               1          122           0.8197      

       apotekalaurus            4          496           0.8065      

        bancaintesa             1          125            0.8        

       tvojaljekarna           130       16,521          0.7869      

      domacica.sanja            21        2,819          0.7449      

        telegram.hr             3          403           0.7444      

         glovo_hr               2          275           0.7273      

          vindija               33        4,675          0.7059      

      belamakeuplada            1          154           0.6494      

  obuca.metro.montenegro        50        7,919          0.6314      

     ljekarnelukacinzg          1          162           0.6173      

       teakravarscan            1          163           0.6135      

      llums_cosmetics           2          334           0.5988      

       uvijekgladna             10        1,691          0.5914      

         dm_srbija              1          171           0.5848      

         mojbonbon              5          905           0.5525      

      apotekaonlinea            1          182           0.5495      

     ave_dulcis_tamara          11        2,067          0.5322      

       gingerellica             2          380           0.5263      

      lucianoplazibat           1          190           0.5263      

        obuca.metro             7         1,399          0.5004      

       matea.miljan             1          207           0.4831      

        knjazrobert             1          217           0.4608      

   tomislavgustinfitness        1          282           0.3546      

     father_of_djordje          1          341           0.2933      

      obuca.metro_bih           5         4,894          0.1022      
---------------------------------------------------------------------


### TERM IMPORTANCE




```r
## Udio riječi po domenama
domenaWords <- insta_tokenTidy %>%
  filter(FROM %in% c("beautypharmacy_hr", "jutarnji.hr", "vecernji.list" )) %>% 
  count(FROM, word, sort = T)
  
ukupnoWords <- domenaWords %>%
  group_by(FROM) %>%
  summarise(totWords = sum(n))
domenaWords <- left_join(domenaWords, ukupnoWords)
# domenaWords %>% head(15)
# domenaWords %>% 
# ggplot(., aes(n/totWords, fill = domena)) +
#   geom_histogram(show.legend = FALSE) +
#   xlim(NA, 0.0009) +
#   facet_wrap(~domena, ncol = 2, scales = "free_y")
## Najbitnije riječi po domenma
idf <- domenaWords %>%
  bind_tf_idf(word, FROM, n)
idf %>% head(10)
```

```
##                  FROM         word   n totWords     tf  idf tf_idf
##  1:       jutarnji.hr     jutarnji 561    27623 0.0203 1.10 0.0223
##  2:       jutarnji.hr   hanzamedia 513    27623 0.0186 1.10 0.0204
##  3: beautypharmacy_hr         kože 510    38753 0.0132 1.10 0.0145
##  4: beautypharmacy_hr         kožu 465    38753 0.0120 0.41 0.0049
##  5:     vecernji.list         link 390    15892 0.0245 0.00 0.0000
##  6:     vecernji.list        tekst 390    15892 0.0245 1.10 0.0270
##  7:     vecernji.list vecernjilist 389    15892 0.0245 1.10 0.0269
##  8:     vecernji.list    linkinbio 333    15892 0.0210 1.10 0.0230
##  9: beautypharmacy_hr     skincare 319    38753 0.0082 1.10 0.0090
## 10: beautypharmacy_hr       beauty 315    38753 0.0081 1.10 0.0089
```

```r
# idf %>% 
#   select(-totWords) %>%
#   arrange(desc(tf_idf))
idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word)))) %>% 
  mutate(FROM = factor(FROM)) %>%
  group_by(FROM) %>% 
  top_n(15) %>% 
  ungroup() %>%
  ggplot(aes(word, tf_idf, fill = FROM)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~FROM, ncol = 2, scales = "free") +
  coord_flip() +
  theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/frekvencija-1.png)<!-- -->



### PHRASES


```r
insta_bigram <- instagram %>%
  unnest_tokens(bigram, FULL_TEXT, token = "ngrams", n = 2)
insta_bigram %>% head(10)
```

```
##           DATE     TIME
##  1: 2022-01-02 23:29:29
##  2: 2022-01-02 23:29:29
##  3: 2022-01-02 23:29:29
##  4: 2022-01-02 23:29:29
##  5: 2022-01-02 23:29:29
##  6: 2022-01-02 23:29:29
##  7: 2022-01-02 23:29:29
##  8: 2022-01-02 23:29:29
##  9: 2022-01-02 23:29:29
## 10: 2022-01-02 23:29:29
##                                                                                                                                                                                                                                                                           TITLE
##  1: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  2: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  3: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  4: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  5: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  6: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  7: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  8: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##  9: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
## 10: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection #hortensja #hortensias
##               FROM         AUTHOR                                      URL
##  1: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  2: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  3: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  4: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  5: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  6: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  7: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  8: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##  9: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
## 10: anonymous_user anonymous_user https://www.instagram.com/p/CYPpbBjMDG5/
##                                                                                                                                                                                                   URL_PHOTO
##  1: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  2: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  3: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  4: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  5: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  6: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  7: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  8: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##  9: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
## 10: https://mediatoolkit.com/img/50x50,sc,s-3IcNbqAFC3wQlzT1pOFXHhcc8Jeif6O3B3z7fCueWs/https://upload.wikimedia.org/wikipedia/commons/thumb/e/e7/Instagram_logo_2016.svg/1200px-Instagram_logo_2016.svg.png
##     SOURCE_TYPE GROUP_NAME KEYWORD_NAME FOUND_KEYWORDS LANGUAGES LOCATIONS TAGS
##  1:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  2:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  3:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  4:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  5:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  6:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  7:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  8:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##  9:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
## 10:   instagram       Luka         opće              I    hr, pl    HR, PL   NA
##     MANUAL_SENTIMENT AUTO_SENTIMENT
##  1:               NA        neutral
##  2:               NA        neutral
##  3:               NA        neutral
##  4:               NA        neutral
##  5:               NA        neutral
##  6:               NA        neutral
##  7:               NA        neutral
##  8:               NA        neutral
##  9:               NA        neutral
## 10:               NA        neutral
##                                                                                                                                                                                                                                          MENTION_SNIPPET
##  1: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  2: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  3: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  4: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  5: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  6: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  7: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  8: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##  9: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
## 10: I płatki . . . . . . . #naturephotography #nature #wogrodzie #macro #macrophotography #macromood #macronature #plants #plantlover #garden #nature_perfection #nature_brilliance #ogrodoweinspiracje #planstagram #macro_brilliance #macro_perfection
##       REACH VIRALITY ENGAGEMENT_RATE INTERACTIONS FOLLOWERS_COUNT LIKE_COUNT
##  1: 1.3e+07       NA              NA      1934320               0         12
##  2: 1.3e+07       NA              NA      1934320               0         12
##  3: 1.3e+07       NA              NA      1934320               0         12
##  4: 1.3e+07       NA              NA      1934320               0         12
##  5: 1.3e+07       NA              NA      1934320               0         12
##  6: 1.3e+07       NA              NA      1934320               0         12
##  7: 1.3e+07       NA              NA      1934320               0         12
##  8: 1.3e+07       NA              NA      1934320               0         12
##  9: 1.3e+07       NA              NA      1934320               0         12
## 10: 1.3e+07       NA              NA      1934320               0         12
##     COMMENT_COUNT SHARE_COUNT TWEET_COUNT LOVE_COUNT WOW_COUNT HAHA_COUNT
##  1:             0          NA          NA         NA        NA         NA
##  2:             0          NA          NA         NA        NA         NA
##  3:             0          NA          NA         NA        NA         NA
##  4:             0          NA          NA         NA        NA         NA
##  5:             0          NA          NA         NA        NA         NA
##  6:             0          NA          NA         NA        NA         NA
##  7:             0          NA          NA         NA        NA         NA
##  8:             0          NA          NA         NA        NA         NA
##  9:             0          NA          NA         NA        NA         NA
## 10:             0          NA          NA         NA        NA         NA
##     SAD_COUNT ANGRY_COUNT TOTAL_REACTIONS_COUNT FAVORITE_COUNT RETWEET_COUNT
##  1:        NA          NA                    NA             NA            NA
##  2:        NA          NA                    NA             NA            NA
##  3:        NA          NA                    NA             NA            NA
##  4:        NA          NA                    NA             NA            NA
##  5:        NA          NA                    NA             NA            NA
##  6:        NA          NA                    NA             NA            NA
##  7:        NA          NA                    NA             NA            NA
##  8:        NA          NA                    NA             NA            NA
##  9:        NA          NA                    NA             NA            NA
## 10:        NA          NA                    NA             NA            NA
##     VIEW_COUNT DISLIKE_COUNT COMMENTS_COUNT LIKES DISLIKES COUNT REPOST_COUNT
##  1:          0            NA             NA    NA       NA    NA           NA
##  2:          0            NA             NA    NA       NA    NA           NA
##  3:          0            NA             NA    NA       NA    NA           NA
##  4:          0            NA             NA    NA       NA    NA           NA
##  5:          0            NA             NA    NA       NA    NA           NA
##  6:          0            NA             NA    NA       NA    NA           NA
##  7:          0            NA             NA    NA       NA    NA           NA
##  8:          0            NA             NA    NA       NA    NA           NA
##  9:          0            NA             NA    NA       NA    NA           NA
## 10:          0            NA             NA    NA       NA    NA           NA
##     REDDIT_TYPE REDDIT_SCORE INFLUENCE_SCORE TWEET_TYPE TWEET_SOURCE_NAME
##  1:        <NA>           NA               1       <NA>              <NA>
##  2:        <NA>           NA               1       <NA>              <NA>
##  3:        <NA>           NA               1       <NA>              <NA>
##  4:        <NA>           NA               1       <NA>              <NA>
##  5:        <NA>           NA               1       <NA>              <NA>
##  6:        <NA>           NA               1       <NA>              <NA>
##  7:        <NA>           NA               1       <NA>              <NA>
##  8:        <NA>           NA               1       <NA>              <NA>
##  9:        <NA>           NA               1       <NA>              <NA>
## 10:        <NA>           NA               1       <NA>              <NA>
##     TWEET_SOURCE_URL            DATETIME    LIKE COMMENT
##  1:             <NA> 2022-01-02 23:29:29 1762169  172151
##  2:             <NA> 2022-01-02 23:29:29 1762169  172151
##  3:             <NA> 2022-01-02 23:29:29 1762169  172151
##  4:             <NA> 2022-01-02 23:29:29 1762169  172151
##  5:             <NA> 2022-01-02 23:29:29 1762169  172151
##  6:             <NA> 2022-01-02 23:29:29 1762169  172151
##  7:             <NA> 2022-01-02 23:29:29 1762169  172151
##  8:             <NA> 2022-01-02 23:29:29 1762169  172151
##  9:             <NA> 2022-01-02 23:29:29 1762169  172151
## 10:             <NA> 2022-01-02 23:29:29 1762169  172151
##                         bigram
##  1:                   i płatki
##  2:   płatki naturephotography
##  3:   naturephotography nature
##  4:           nature wogrodzie
##  5:            wogrodzie macro
##  6:     macro macrophotography
##  7: macrophotography macromood
##  8:      macromood macronature
##  9:         macronature plants
## 10:          plants plantlover
```

```r
insta_bigram %>%
  count(bigram, sort = T) %>%
  head(15) %>%
  printMD(., big.mark=",")
```


-------------------------
     bigram          n   
----------------- -------
      da se        3,071 

  radno vrijeme    2,649 

     za sve        2,498 

     u opisu       2,444 

      da je        2,266 

      i ti         2,060 

     4 dana        2,044 

       i u         2,003 

  opisu profila    2,000 

    sebe ili       1,927 

      je u         1,917 

 snizenje akcija   1,904 

   dragu osobu     1,898 

      od 2         1,886 

   požurite i      1,864 
-------------------------

```r
insta_bigram_sep <- insta_bigram %>%
  separate(bigram, c("word1","word2"), sep = " ")
insta_bigram_tidy <- insta_bigram_sep %>%
  filter(!word1 %in% stop_corpus$word) %>%
  filter(!word2 %in% stop_corpus$word) %>%
  mutate(word1 = gsub("\\d+", NA, word1)) %>%
  mutate(word2 = gsub("\\d+", NA, word2)) %>%
  mutate(word1 = gsub("^[a-zA-Z]$", NA, word1)) %>%
  mutate(word2 = gsub("^[a-zA-Z]$", NA, word2)) 
insta_bigram_tidy_bigram_counts <- insta_bigram_tidy %>% 
  count(word1, word2, sort = TRUE)

bigrams_united <- insta_bigram_tidy %>%
  unite(bigram, word1, word2, sep = " ")
#bigrams_united
bigrams_united %>% 
  count(FROM,bigram,sort = T) -> topicBigram
# Najvažniji bigrami po domenama
 bigram_tf_idf <- bigrams_united %>%
  count(FROM, bigram) %>%
  bind_tf_idf(bigram, FROM, n) %>%
  arrange(desc(tf_idf))
bigram_tf_idf %>%
  filter(FROM %in% c("elladvornik", "imerovic_sandi_budivelik", "elaajerkovic" )) %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram)))) %>% 
  group_by(FROM) %>% 
  top_n(7) %>% 
  ungroup() %>%
  ggplot(aes(bigram, tf_idf, fill = FROM)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~FROM, ncol = 2, scales = "free") +
  coord_flip() + 
  theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/nGRAMI-1.png)<!-- -->

```r
rm(posts,instagram,bigram_tf_idf,insta_bigram_sep,insta_bigram_tidy)
```

#### PHRASES CORRELATION




```r
insta_tokenTidy %>% 
#  filter(datum > "2020-02-20") %>%
  group_by(word) %>%
  filter(n() > 20) %>%
  filter(!is.na(word)) %>%
  pairwise_cor(word,DATE, sort = T) -> corsWords
#corsWords %>%
#  filter(item1 == "oporavak")
corsWords %>%
  filter(item1 %in% c("kupnja", "akcija", "sex", "poklon")) %>%
  group_by(item1) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(item2 = reorder(item2, correlation)) %>%
  ggplot(aes(item2, correlation)) +
  geom_bar(stat = "identity") +
  facet_wrap(~ item1, scales = "free") +
  coord_flip() + 
  theme_economist()
```

### TEMATIC ANALYSIS




```r
insta_tokenTidy %>%
  count(FROM, word, sort = TRUE) %>%
  cast_dtm(FROM, word,n) -> dtm
insta_LDA <- LDA(dtm, k = 4,  control = list(seed = 1234))
insta_LDA_tidy <- tidy(insta_LDA, matrix = "beta")
#newsCOVID_LDA_tidy
insta_terms <- insta_LDA_tidy %>%
  drop_na(.) %>%
  group_by(topic) %>%
  top_n(15, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#newsCOVID_terms
insta_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/TEME-1.png)<!-- -->




```r
# Bigrami 
topicBigram %>%
  cast_dtm(FROM, bigram,n) -> dtmB
insta_LDA <- LDA(dtmB, k = 4,  control = list(seed = 1234))
insta_LDA_tidy <- tidy(insta_LDA, matrix = "beta")
#newsCOVID_LDA_tidy
insta_terms <- insta_LDA_tidy %>%
  drop_na(.) %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
#newsCOVID_terms
insta_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered() + 
  theme_economist()
```

![](Lecture-7-Text-analysis_files/figure-html/TEMEbigram-1.png)<!-- -->















