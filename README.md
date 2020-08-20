
# Semisupervided LDA for theory-driven text analysis

**NOTICE** This R package is renamed from **quanteda.seededlda** to
**seededlda** for CRAN submission.

A package implements seeded-LDA for semisupervised topic modeling using
**quanteda** and GibbsLDA++. This seeded-LDA model was proposed by [Lu
et al. (2010)](https://dl.acm.org/citation.cfm?id=2119585). Before this
package was submitted to CRAN on August 2020 (until version 0.3), it was
wrapper around the **topicmodels** package, but the LDA estimator is
newly implemented in C++ using the
[GibbsLDA++](http://gibbslda.sourceforge.net/) library by the author.
The author believes this package implements the seeded-LDA model more
closely to the original proposal.

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

``` r
corp <- readRDS("tests/data/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE)
dfmt <- dfm(toks) %>% 
    dfm_select("^[A-Za-z]+$", valuetype = "regex") %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
```

Users of seeded-LDA has to construct a small dictionary of keywords
(seed words) to define the desired topics.

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
set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = FALSE)
print(terms(slda, 20))
##       economy     politics        society         diplomacy    military  
##  [1,] "company"   "parliament"    "police"        "diplomatic" "army"    
##  [2,] "money"     "congress"      "school"        "embassy"    "navy"    
##  [3,] "market"    "politicians"   "hospital"      "ambassador" "soldiers"
##  [4,] "bank"      "parliamentary" "prison"        "treaty"     "marine"  
##  [5,] "industry"  "lawmakers"     "court"         "diplomat"   "korea"   
##  [6,] "banks"     "voters"        "women"         "diplomats"  "nuclear" 
##  [7,] "markets"   "lawmaker"      "man"           "syria"      "korean"  
##  [8,] "banking"   "politician"    "found"         "syrian"     "missile" 
##  [9,] "china"     "eu"            "investigation" "iran"       "air"     
## [10,] "chinese"   "uk"            "children"      "israel"     "nato"    
## [11,] "economic"  "british"       "video"         "daesh"      "japan"   
## [12,] "project"   "election"      "service"       "turkish"    "force"   
## [13,] "trade"     "germany"       "department"    "terrorist"  "kim"     
## [14,] "india"     "german"        "swedish"       "turkey"     "program" 
## [15,] "billion"   "elections"     "newspaper"     "weapons"    "aircraft"
## [16,] "oil"       "union"         "claimed"       "saudi"      "weapons" 
## [17,] "global"    "french"        "letter"        "relations"  "ministry"
## [18,] "indian"    "brexit"        "incident"      "conflict"   "missiles"
## [19,] "financial" "presidential"  "sweden"        "deal"       "system"  
## [20,] "business"  "leader"        "law"           "iraq"       "systems" 
##       nature     
##  [1,] "human"    
##  [2,] "sand"     
##  [3,] "water"    
##  [4,] "going"    
##  [5,] "really"   
##  [6,] "see"      
##  [7,] "much"     
##  [8,] "know"     
##  [9,] "come"     
## [10,] "facebook" 
## [11,] "something"
## [12,] "good"     
## [13,] "whether"  
## [14,] "help"     
## [15,] "change"   
## [16,] "go"       
## [17,] "right"    
## [18,] "important"
## [19,] "seen"     
## [20,] "far"
```

Many of the top terms of seeded-LDA models are seed words but other
topic words are also identified.

``` r
topic <- table(topics(slda))
print(topic)
## 
## diplomacy   economy  military    nature  politics   society 
##       163       127       140       174       169       227
```
