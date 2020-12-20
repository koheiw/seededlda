
# Semisupervised LDA for theory-driven text analysis

**NOTICE:** This R package is renamed from **quanteda.seededlda** to
**seededlda** for CRAN submission.

**seededlda** is an R package that implements the seeded-LDA for
semisupervised topic modeling using **quanteda**. The seeded-LDA model
was proposed by [Lu et
al. (2010)](https://dl.acm.org/citation.cfm?id=2119585). Until version
0.3, that packages has been a simple wrapper around the **topicmodels**
package, but the LDA estimator is newly implemented in C++ using the
[GibbsLDA++](http://gibbslda.sourceforge.net/) library to be submitted
to CRAN in August 202. The author believes this package implements the
seeded-LDA model more closely to the original proposal.

Please see [*Theory-Driven Analysis of Large Corpora: Semisupervised
Topic Classification of the UN
Speeches*](https://journals.sagepub.com/doi/full/10.1177/0894439320907027)
for the overview of semisupervised topic classification techniques and
their advantages in social science research.

[**keyATM**](https://github.com/keyATM/keyATM) is the latest addition to
the semisupervised topic models. The users of seeded-LDA are also
encouraged to use that package.

## Install

``` r
install.packages("devtools")
devtools::install_github("koheiw/seededlda") 
```

## Example

The corpus and seed words in this example are from [*Conspiracist
propaganda: How Russia promotes anti-establishment sentiment
online?*](https://koheiw.net/wp-content/uploads/2019/06/Sputnik-05-ECPR.pdf).

``` r
require(quanteda)
require(seededlda) # changed from quanteda.seededlda to seededlda
```

Users of seeded-LDA has to construct a small dictionary of keywords
(seed words) to define the desired topics.

``` r
dict <- dictionary(file = "tests/data/topics.yml")
print(dict)
## Dictionary object with 5 key entries.
## - [economy]:
##   - market*, money, bank*, stock*, bond*, industry, company, shop*
## - [politics]:
##   - parliament*, congress*, white house, party leader*, party member*, voter*, lawmaker*, politician*
## - [society]:
##   - police, prison*, school*, hospital*
## - [diplomacy]:
##   - ambassador*, diplomat*, embassy, treaty
## - [military]:
##   - military, soldier*, terrorist*, air force, marine, navy, army
```

``` r
corp <- readRDS("tests/data/data_corpus_sputnik.RDS")
toks <- tokens(corp) %>%
        tokens_select("^[A-Za-z]+$", valuetype = "regex", min_nchar = 2) %>% 
        tokens_compound(dict) # multi-word expressions
dfmt <- dfm(toks) %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.2, docfreq_type = "prop")
```

Many of the top terms of the seeded-LDA are seed words but related topic
words are also identified. The result includes “other” as a junk topic
because `residual = TRUE` .

``` r
set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
print(terms(slda, 20))
##       economy    politics        society         diplomacy    military    
##  [1,] "company"  "parliament"    "police"        "embassy"    "army"      
##  [2,] "money"    "congress"      "school"        "diplomatic" "terrorist" 
##  [3,] "market"   "white_house"   "hospital"      "ambassador" "navy"      
##  [4,] "bank"     "politicians"   "prison"        "treaty"     "terrorists"
##  [5,] "industry" "parliamentary" "media"         "diplomat"   "air_force" 
##  [6,] "banks"    "lawmakers"     "reported"      "diplomats"  "soldiers"  
##  [7,] "markets"  "voters"        "information"   "north"      "marine"    
##  [8,] "banking"  "lawmaker"      "local"         "trump"      "syria"     
##  [9,] "china"    "politician"    "video"         "nuclear"    "defense"   
## [10,] "chinese"  "uk"            "women"         "korea"      "syrian"    
## [11,] "percent"  "european"      "found"         "south"      "forces"    
## [12,] "year"     "minister"      "public"        "sanctions"  "weapons"   
## [13,] "economic" "eu"            "investigation" "iran"       "nato"      
## [14,] "trade"    "party"         "news"          "korean"     "israel"    
## [15,] "oil"      "political"     "court"         "foreign"    "daesh"     
## [16,] "project"  "prime"         "report"        "security"   "turkish"   
## [17,] "billion"  "german"        "group"         "meeting"    "turkey"    
## [18,] "india"    "germany"       "department"    "relations"  "air"       
## [19,] "million"  "british"       "children"      "donald"     "iraq"      
## [20,] "system"   "world"         "man"           "moscow"     "saudi"     
##       other   
##  [1,] "like"  
##  [2,] "now"   
##  [3,] "just"  
##  [4,] "even"  
##  [5,] "think" 
##  [6,] "trump" 
##  [7,] "way"   
##  [8,] "going" 
##  [9,] "many"  
## [10,] "years" 
## [11,] "say"   
## [12,] "want"  
## [13,] "really"
## [14,] "back"  
## [15,] "made"  
## [16,] "get"   
## [17,] "world" 
## [18,] "come"  
## [19,] "need"  
## [20,] "much"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
##   economy  politics   society diplomacy  military     other 
##       137       166       248       140       145       164
```
