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

**NOTICE:** This R package is renamed from **quanteda.seededlda** to
**seededlda** for CRAN submission.

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
because `residual = TRUE` .

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
##  [7,] "markets"   "voters"        "media"           "north"        
##  [8,] "banking"   "lawmaker"      "information"     "nuclear"      
##  [9,] "stock"     "politician"    "reported"        "defense"      
## [10,] "stockholm" "european"      "local"           "korea"        
## [11,] "china"     "minister"      "video"           "south"        
## [12,] "chinese"   "eu"            "women"           "trump"        
## [13,] "percent"   "party"         "department"      "korean"       
## [14,] "year"      "uk"            "found"           "missile"      
## [15,] "india"     "sanctions"     "investigation"   "moscow"       
## [16,] "oil"       "political"     "social"          "meeting"      
## [17,] "countries" "prime"         "public"          "security"     
## [18,] "economic"  "union"         "court"           "nato"         
## [19,] "billion"   "germany"       "several"         "foreign"      
## [20,] "trade"     "election"      "took"            "international"
##       military     other     
##  [1,] "army"       "trump"   
##  [2,] "terrorist"  "just"    
##  [3,] "navy"       "like"    
##  [4,] "terrorists" "world"   
##  [5,] "soldiers"   "think"   
##  [6,] "air_force"  "now"     
##  [7,] "marine"     "even"    
##  [8,] "soldier"    "going"   
##  [9,] "syria"      "get"     
## [10,] "syrian"     "american"
## [11,] "iran"       "made"    
## [12,] "forces"     "say"     
## [13,] "israel"     "way"     
## [14,] "group"      "want"    
## [15,] "daesh"      "really"  
## [16,] "turkish"    "show"    
## [17,] "turkey"     "come"    
## [18,] "region"     "make"    
## [19,] "security"   "know"    
## [20,] "war"        "back"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
##   economy  politics   society diplomacy  military     other 
##       140       160       243       134       121       202
```

## Examples

Please read the following papers for how to use Seeded LDA in social
science research:

Curini, Luigi and Vignoli, Valerio. 2021. [Committed Moderates and
Uncommitted Extremists: Ideological Leaning and Parties’ Narratives on
Military Interventions in Italy](https://doi.org/10.1093/fpa/orab016),
*Foreign Policy Analysis*.
