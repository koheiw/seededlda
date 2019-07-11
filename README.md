
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
term <- terms(slda, 20)
colnames(term) <- c(names(dict), "else")
print(term)
##       economy     politics        society         diplomacy    
##  [1,] "company"   "parliament"    "police"        "diplomatic" 
##  [2,] "money"     "congress"      "school"        "embassy"    
##  [3,] "market"    "politicians"   "hospital"      "ambassador" 
##  [4,] "bank"      "parliamentary" "prison"        "treaty"     
##  [5,] "industry"  "lawmakers"     "department"    "diplomat"   
##  [6,] "banks"     "voters"        "court"         "diplomats"  
##  [7,] "markets"   "lawmaker"      "service"       "sanctions"  
##  [8,] "banking"   "politician"    "women"         "iran"       
##  [9,] "economic"  "video"         "rights"        "eu"         
## [10,] "trade"     "june"          "found"         "deal"       
## [11,] "oil"       "team"          "authorities"   "agreement"  
## [12,] "billion"   "details"       "man"           "meeting"    
## [13,] "global"    "monday"        "investigation" "union"      
## [14,] "financial" "cup"           "office"        "israel"     
## [15,] "business"  "twitter"       "west"          "secretary"  
## [16,] "companies" "follow"        "left"          "putin"      
## [17,] "gas"       "embed"         "children"      "council"    
## [18,] "economy"   "american"      "claimed"       "relations"  
## [19,] "energy"    "referendum"    "january"       "cooperation"
## [20,] "interest"  "march"         "members"       "peace"      
##       military       nature        else          
##  [1,] "army"         "human"       "uk"          
##  [2,] "navy"         "sand"        "going"       
##  [3,] "soldiers"     "water"       "much"        
##  [4,] "marine"       "china"       "election"    
##  [5,] "syria"        "nuclear"     "british"     
##  [6,] "syrian"       "korea"       "really"      
##  [7,] "weapons"      "chinese"     "campaign"    
##  [8,] "nato"         "korean"      "false"       
##  [9,] "attack"       "missile"     "come"        
## [10,] "german"       "program"     "show"        
## [11,] "terrorist"    "system"      "see"         
## [12,] "daesh"        "india"       "whether"     
## [13,] "turkish"      "air"         "know"        
## [14,] "turkey"       "development" "right"       
## [15,] "organization" "indian"      "elections"   
## [16,] "saudi"        "research"    "good"        
## [17,] "iraq"         "recently"    "facebook"    
## [18,] "groups"       "kim"         "problem"     
## [19,] "middle"       "japan"       "presidential"
## [20,] "french"       "technology"  "today"
```

``` r
topic <- table(topics(slda))
names(topic) <- c(names(dict), "else")
print(topic)
##   economy  politics   society diplomacy  military    nature      else 
##        57       105       192       113       135       159       239
```
