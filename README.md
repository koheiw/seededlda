
# Fit seeded LDA using quanteda

A function to fit seeded LDA model for semi-supervised topic
classification using **topicmodels** and **quanteda**. The code is from
[*Making a topic dictionary for semi-supervised classification of the UN
speeches*](https://koheiw.net/wp-content/uploads/2019/06/Speech-classification-06-QTA-Dub.pdf).

``` r
require(quanteda)
require(topicmodels)
require(quanteda.seededlda)
```

The corpus and seed words in this example are from [*Conspiracist
propaganda: How Russia promotes anti-establishment
sentimentonline?*](https://koheiw.net/wp-content/uploads/2019/06/Sputnik-05-ECPR.pdf)

``` r
corp <- readRDS("data/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE)
dfmt <- dfm(toks) %>% 
    dfm_select("^[A-Za-z]+$", valuetype = "regex") %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
```

``` r
dict <- dictionary(file = "data/topics.yml")
print(dict)
## Dictionary object with 6 key entries.
## - [economy]:
##   - market*, money, bank*, stock*, bond*, industry, company, shop*
## - [politics]:
##   - parliament*, congress*, party leader*, party member*, voter*, lawmaker*, politician*
## - [society]:
##   - police, prison*, school*, hospital*
## - [diplomacy]:
##   - ambassador*, diplomat*, embassy, treaty
## - [military]:
##   - military, soldier*, air force, marine, navy, army
## - [nature]:
##   - water, wind, sand, forest, mountain, desert, animal, human
```

``` r
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
term <- terms(slda, 20)
colnames(term) <- c(names(dict), "else")
print(term)
##       economy     politics        society         diplomacy    military   
##  [1,] "company"   "parliament"    "police"        "diplomatic" "army"     
##  [2,] "money"     "congress"      "school"        "embassy"    "navy"     
##  [3,] "market"    "politicians"   "hospital"      "ambassador" "soldiers" 
##  [4,] "bank"      "parliamentary" "prison"        "treaty"     "marine"   
##  [5,] "industry"  "lawmakers"     "video"         "diplomat"   "sanctions"
##  [6,] "banks"     "voters"        "court"         "diplomats"  "uk"       
##  [7,] "markets"   "lawmaker"      "service"       "nuclear"    "eu"       
##  [8,] "banking"   "politician"    "women"         "korea"      "deal"     
##  [9,] "china"     "election"      "office"        "korean"     "iran"     
## [10,] "chinese"   "german"        "man"           "missile"    "meeting"  
## [11,] "economic"  "elections"     "department"    "air"        "agreement"
## [12,] "oil"       "team"          "found"         "show"       "nato"     
## [13,] "india"     "details"       "twitter"       "system"     "germany"  
## [14,] "billion"   "cup"           "investigation" "japan"      "union"    
## [15,] "project"   "follow"        "left"          "director"   "british"  
## [16,] "trade"     "vote"          "taken"         "kim"        "europe"   
## [17,] "indian"    "coalition"     "claimed"       "aircraft"   "house"    
## [18,] "financial" "referendum"    "children"      "systems"    "talks"    
## [19,] "business"  "democratic"    "justice"       "program"    "secretary"
## [20,] "companies" "june"          "swedish"       "missiles"   "putin"    
##       nature      else        
##  [1,] "human"     "going"     
##  [2,] "sand"      "much"      
##  [3,] "water"     "order"     
##  [4,] "syria"     "see"       
##  [5,] "syrian"    "help"      
##  [6,] "attack"    "false"     
##  [7,] "weapons"   "really"    
##  [8,] "city"      "come"      
##  [9,] "east"      "go"        
## [10,] "israel"    "whether"   
## [11,] "terrorist" "american"  
## [12,] "daesh"     "data"      
## [13,] "ministry"  "know"      
## [14,] "turkish"   "good"      
## [15,] "groups"    "different" 
## [16,] "turkey"    "facebook"  
## [17,] "border"    "put"       
## [18,] "saudi"     "university"
## [19,] "un"        "believe"   
## [20,] "iraq"      "research"
```

``` r
topic <- table(topics(slda))
names(topic) <- c(names(dict), "else")
print(topic)
##   economy  politics   society diplomacy  military    nature      else 
##        85        91       202       111       128       155       228
```
