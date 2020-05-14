
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
##       economy    politics        society       diplomacy      military     
##  [1,] "company"  "parliament"    "police"      "diplomatic"   "army"       
##  [2,] "money"    "congress"      "school"      "embassy"      "navy"       
##  [3,] "market"   "politicians"   "hospital"    "ambassador"   "soldiers"   
##  [4,] "bank"     "parliamentary" "prison"      "treaty"       "marine"     
##  [5,] "industry" "lawmakers"     "court"       "diplomat"     "china"      
##  [6,] "banks"    "voters"        "women"       "diplomats"    "chinese"    
##  [7,] "markets"  "lawmaker"      "service"     "sanctions"    "economic"   
##  [8,] "banking"  "politician"    "rights"      "meeting"      "system"     
##  [9,] "video"    "eu"            "man"         "department"   "global"     
## [10,] "show"     "germany"       "office"      "press"        "trade"      
## [11,] "twitter"  "german"        "found"       "israel"       "oil"        
## [12,] "details"  "election"      "law"         "program"      "india"      
## [13,] "follow"   "europe"        "left"        "putin"        "billion"    
## [14,] "talk"     "elections"     "children"    "house"        "project"    
## [15,] "users"    "union"         "authorities" "secretary"    "data"       
## [16,] "means"    "french"        "taken"       "agency"       "development"
## [17,] "post"     "team"          "claimed"     "intelligence" "indian"     
## [18,] "friday"   "vote"          "newspaper"   "wednesday"    "financial"  
## [19,] "black"    "cup"           "swedish"     "measures"     "research"   
## [20,] "joined"   "bill"          "home"        "alleged"      "companies"  
##       nature      other      
##  [1,] "human"     "uk"       
##  [2,] "sand"      "going"    
##  [3,] "water"     "much"     
##  [4,] "syria"     "british"  
##  [5,] "nuclear"   "really"   
##  [6,] "syrian"    "see"      
##  [7,] "korea"     "might"    
##  [8,] "iran"      "come"     
##  [9,] "weapons"   "campaign" 
## [10,] "korean"    "know"     
## [11,] "missile"   "american" 
## [12,] "air"       "expressed"
## [13,] "deal"      "wrote"    
## [14,] "agreement" "whether"  
## [15,] "ministry"  "good"     
## [16,] "relations" "facebook" 
## [17,] "nato"      "position" 
## [18,] "un"        "change"   
## [19,] "daesh"     "problem"  
## [20,] "terrorist" "help"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
## diplomacy   economy  military    nature     other  politics   society 
##        91        73       139       208       233        67       189
```
