
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
##       economy     politics        society         diplomacy     
##  [1,] "company"   "parliament"    "police"        "embassy"     
##  [2,] "money"     "congress"      "school"        "diplomatic"  
##  [3,] "market"    "politicians"   "hospital"      "ambassador"  
##  [4,] "bank"      "parliamentary" "prison"        "treaty"      
##  [5,] "industry"  "lawmakers"     "department"    "diplomat"    
##  [6,] "banks"     "voters"        "court"         "diplomats"   
##  [7,] "markets"   "lawmaker"      "women"         "syria"       
##  [8,] "banking"   "politician"    "rights"        "syrian"      
##  [9,] "china"     "video"         "man"           "terrorist"   
## [10,] "chinese"   "twitter"       "found"         "daesh"       
## [11,] "economic"  "details"       "members"       "turkish"     
## [12,] "oil"       "team"          "investigation" "turkey"      
## [13,] "trade"     "june"          "children"      "saudi"       
## [14,] "project"   "cup"           "january"       "iraq"        
## [15,] "billion"   "post"          "left"          "conflict"    
## [16,] "india"     "follow"        "newspaper"     "organization"
## [17,] "indian"    "monday"        "claimed"       "weapons"     
## [18,] "global"    "amid"          "justice"       "east"        
## [19,] "financial" "american"      "swedish"       "french"      
## [20,] "companies" "referendum"    "case"          "middle"      
##       military   nature      else       
##  [1,] "army"     "human"     "uk"       
##  [2,] "navy"     "sand"      "going"    
##  [3,] "soldiers" "water"     "much"     
##  [4,] "marine"   "nuclear"   "british"  
##  [5,] "missile"  "korea"     "help"     
##  [6,] "air"      "sanctions" "right"    
##  [7,] "force"    "iran"      "really"   
##  [8,] "nato"     "eu"        "see"      
##  [9,] "city"     "deal"      "false"    
## [10,] "ministry" "korean"    "come"     
## [11,] "israel"   "meeting"   "know"     
## [12,] "agency"   "agreement" "working"  
## [13,] "border"   "union"     "expressed"
## [14,] "area"     "program"   "might"    
## [15,] "weapons"  "germany"   "show"     
## [16,] "system"   "election"  "good"     
## [17,] "november" "relations" "whether"  
## [18,] "aircraft" "german"    "different"
## [19,] "israeli"  "talks"     "change"   
## [20,] "japan"    "secretary" "believe"
```

``` r
topic <- table(topics(slda))
names(topic) <- c(names(dict), "else")
print(topic)
##   economy  politics   society diplomacy  military    nature      else 
##        83        93       188        87       141       161       247
```
