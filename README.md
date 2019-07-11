
# Fit seeded-LDA in R

A package to fit seeded-LDA model for semi-supervised topic
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
corp <- readRDS("tests/data/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE)
dfmt <- dfm(toks) %>% 
    dfm_select("^[A-Za-z]+$", valuetype = "regex") %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
```

``` r
dict <- dictionary(file = "tests/data/topics.yml")
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
print(terms(slda, 20))
##       economy     politics        society         diplomacy    military   
##  [1,] "company"   "parliament"    "police"        "diplomatic" "army"     
##  [2,] "money"     "congress"      "school"        "embassy"    "navy"     
##  [3,] "market"    "politicians"   "hospital"      "ambassador" "soldiers" 
##  [4,] "bank"      "parliamentary" "prison"        "treaty"     "marine"   
##  [5,] "industry"  "voters"        "department"    "diplomat"   "missile"  
##  [6,] "banks"     "lawmakers"     "video"         "diplomats"  "air"      
##  [7,] "markets"   "lawmaker"      "court"         "nuclear"    "system"   
##  [8,] "banking"   "politician"    "service"       "korea"      "team"     
##  [9,] "china"     "uk"            "women"         "sanctions"  "june"     
## [10,] "chinese"   "eu"            "found"         "iran"       "research" 
## [11,] "economic"  "germany"       "twitter"       "deal"       "force"    
## [12,] "project"   "british"       "investigation" "korean"     "aircraft" 
## [13,] "trade"     "german"        "man"           "meeting"    "power"    
## [14,] "india"     "europe"        "authorities"   "agreement"  "cup"      
## [15,] "oil"       "march"         "members"       "program"    "sea"      
## [16,] "global"    "union"         "children"      "putin"      "using"    
## [17,] "billion"   "elections"     "newspaper"     "secretary"  "high"     
## [18,] "business"  "brexit"        "left"          "director"   "systems"  
## [19,] "companies" "law"           "justice"       "japan"      "based"    
## [20,] "indian"    "vote"          "claimed"       "visit"      "potential"
##       nature      other         
##  [1,] "human"     "syria"       
##  [2,] "sand"      "syrian"      
##  [3,] "water"     "nato"        
##  [4,] "going"     "city"        
##  [5,] "much"      "east"        
##  [6,] "really"    "ministry"    
##  [7,] "see"       "israel"      
##  [8,] "false"     "terrorist"   
##  [9,] "show"      "relations"   
## [10,] "come"      "daesh"       
## [11,] "know"      "un"          
## [12,] "go"        "weapons"     
## [13,] "good"      "turkish"     
## [14,] "facebook"  "turkey"      
## [15,] "change"    "attack"      
## [16,] "american"  "move"        
## [17,] "put"       "border"      
## [18,] "might"     "organization"
## [19,] "something" "council"     
## [20,] "means"     "nations"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
## diplomacy   economy  military    nature     other  politics   society 
##       100        88       130       167       221        86       208
```
