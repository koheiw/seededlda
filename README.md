
# Fit seeded LDA using quanteda

A function to fit seeded LDA model for semi-supervised topic
classification using **topicmodels** and **quanteda**. The code is from
[*Making a topic dictionary for semi-supervised classification of the UN
speeches*](https://koheiw.net/wp-content/uploads/2019/06/Speech-classification-06-QTA-Dub.pdf).

## Install

``` r
install.packages("devtools")
devtools::install_github("koheiw/quanteda.seededlda")
```

## Example

The corpus and seed words in this example are from [*Conspiracist
propaganda: How Russia promotes anti-establishment
sentimentonline?*](https://koheiw.net/wp-content/uploads/2019/06/Sputnik-05-ECPR.pdf)

``` r
require(quanteda)
require(topicmodels)
require(quanteda.seededlda)
```

``` r
corp <- readRDS("inst/extdata/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE)
dfmt <- dfm(toks) %>% 
    dfm_select("^[A-Za-z]+$", valuetype = "regex") %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
```

``` r
dict <- dictionary(file = "inst/extdata/topics.yml")
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
##       economy    politics        society         diplomacy   
##  [1,] "company"  "parliament"    "police"        "diplomatic"
##  [2,] "money"    "congress"      "school"        "embassy"   
##  [3,] "market"   "politicians"   "hospital"      "ambassador"
##  [4,] "bank"     "parliamentary" "prison"        "treaty"    
##  [5,] "industry" "lawmakers"     "court"         "diplomat"  
##  [6,] "banks"    "voters"        "women"         "diplomats" 
##  [7,] "markets"  "lawmaker"      "rights"        "uk"        
##  [8,] "banking"  "politician"    "show"          "going"     
##  [9,] "missile"  "march"         "service"       "british"   
## [10,] "air"      "israel"        "office"        "really"    
## [11,] "video"    "june"          "man"           "false"     
## [12,] "aircraft" "press"         "found"         "come"      
## [13,] "monday"   "authorities"   "department"    "american"  
## [14,] "details"  "october"       "twitter"       "know"      
## [15,] "release"  "israeli"       "investigation" "facebook"  
## [16,] "force"    "vote"          "intelligence"  "team"      
## [17,] "systems"  "law"           "wrote"         "go"        
## [18,] "missiles" "refugees"      "campaign"      "good"      
## [19,] "system"   "jerusalem"     "justice"       "much"      
## [20,] "embed"    "independence"  "left"          "brexit"    
##       military       nature      else         
##  [1,] "army"         "human"     "china"      
##  [2,] "navy"         "sand"      "chinese"    
##  [3,] "soldiers"     "water"     "economic"   
##  [4,] "marine"       "nuclear"   "global"     
##  [5,] "syria"        "korea"     "far"        
##  [6,] "syrian"       "sanctions" "trade"      
##  [7,] "weapons"      "iran"      "oil"        
##  [8,] "attack"       "eu"        "project"    
##  [9,] "terrorist"    "korean"    "india"      
## [10,] "daesh"        "deal"      "billion"    
## [11,] "turkish"      "meeting"   "data"       
## [12,] "ministry"     "agreement" "help"       
## [13,] "groups"       "union"     "indian"     
## [14,] "turkey"       "germany"   "financial"  
## [15,] "iraq"         "election"  "high"       
## [16,] "saudi"        "leader"    "working"    
## [17,] "organization" "german"    "business"   
## [18,] "middle"       "relations" "development"
## [19,] "east"         "talks"     "companies"  
## [20,] "city"         "putin"     "research"
```

``` r
topic <- table(topics(slda))
names(topic) <- c(names(dict), "else")
print(topic)
##   economy  politics   society diplomacy  military    nature      else 
##        86        53       232       104       121       194       210
```
