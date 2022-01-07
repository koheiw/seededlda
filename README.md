
# Seeded-LDA for semisupervised topic modeling

<!-- badges: start -->

[![CRAN
Version](https://www.r-pkg.org/badges/version/seededlda)](https://CRAN.R-project.org/package=seededlda)
[![Downloads](https://cranlogs.r-pkg.org/badges/seededlda)](https://CRAN.R-project.org/package=seededlda)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/seededlda?color=orange)](https://CRAN.R-project.org/package=seededlda)
[![R build
status](https://github.com/koheiw/seededlda/workflows/R-CMD-check/badge.svg)](https://github.com/koheiw/seededlda/actions)
[![codecov](https://codecov.io/gh/koheiw/seededlda/branch/master/graph/badge.svg)](https://codecov.io/gh/koheiw/seededlda)
<!-- badges: end -->

**seededlda** is an R package that implements the seeded-LDA for
semisupervised topic modeling using **quanteda**. The seeded-LDA model
was proposed by [Lu et
al. (2010)](https://dl.acm.org/citation.cfm?id=2119585). Until version
0.3, that packages has been a simple wrapper around the **topicmodels**
package, but the LDA estimator is newly implemented in C++ using the
[GibbsLDA++](http://gibbslda.sourceforge.net/) library to be submitted
to CRAN in August 2020. The author believes this package implements the
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
require(seededlda)
```

Users of seeded-LDA must provided a small dictionary of keywords (seed
words) to define the desired topics.

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
toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE) %>%
        tokens_select(min_nchar = 2) %>% 
        tokens_compound(dict) # for multi-word expressions
dfmt <- dfm(toks) %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.2, docfreq_type = "prop")
```

Many of the top terms of the seeded-LDA are seed words but related topic
words are also identified. The result includes “other” as a junk topic
because `residual = TRUE`.

``` r
set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
print(terms(slda, 20))
##       economy     politics        society           diplomacy   
##  [1,] "company"   "parliament"    "police"          "diplomatic"
##  [2,] "money"     "congress"      "school"          "embassy"   
##  [3,] "market"    "white_house"   "hospital"        "ambassador"
##  [4,] "bank"      "politicians"   "prison"          "treaty"    
##  [5,] "industry"  "parliamentary" "schools"         "diplomat"  
##  [6,] "banks"     "lawmakers"     "pic.twitter.com" "diplomats" 
##  [7,] "markets"   "voters"        "media"           "like"      
##  [8,] "banking"   "lawmaker"      "reported"        "just"      
##  [9,] "stock"     "politician"    "local"           "now"       
## [10,] "stockholm" "minister"      "information"     "think"     
## [11,] "china"     "european"      "video"           "even"      
## [12,] "percent"   "sanctions"     "public"          "trump"     
## [13,] "chinese"   "eu"            "social"          "going"     
## [14,] "economic"  "political"     "court"           "made"      
## [15,] "india"     "party"         "women"           "years"     
## [16,] "year"      "foreign"       "man"             "way"       
## [17,] "oil"       "prime"         "report"          "say"       
## [18,] "project"   "union"         "found"           "want"      
## [19,] "billion"   "moscow"        "investigation"   "many"      
## [20,] "million"   "trump"         "department"      "really"    
##       military        other      
##  [1,] "army"          "north"    
##  [2,] "terrorist"     "nuclear"  
##  [3,] "navy"          "korea"    
##  [4,] "terrorists"    "south"    
##  [5,] "air_force"     "iran"     
##  [6,] "soldiers"      "trump"    
##  [7,] "marine"        "korean"   
##  [8,] "soldier"       "world"    
##  [9,] "defense"       "israel"   
## [10,] "syria"         "deal"     
## [11,] "syrian"        "saudi"    
## [12,] "forces"        "kim"      
## [13,] "security"      "show"     
## [14,] "nato"          "israeli"  
## [15,] "weapons"       "agreement"
## [16,] "daesh"         "program"  
## [17,] "turkish"       "cup"      
## [18,] "turkey"        "trump's"  
## [19,] "international" "japan"    
## [20,] "group"         "peace"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
##   economy  politics   society diplomacy  military     other 
##       136       181       262       158       144       119
```

## Examples

Please read the following papers for how to apply seeded-LDA in social
science research:

Curini, Luigi and Vignoli, Valerio. 2021. [Committed Moderates and
Uncommitted Extremists: Ideological Leaning and Parties’ Narratives on
Military Interventions in Italy](https://doi.org/10.1093/fpa/orab016),
*Foreign Policy Analysis*.
