
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
corp <- readRDS("tests/data/data_corpus_sputnik.RDS")
toks <- tokens(corp, remove_punct = TRUE) %>% 
    tokens_compound(dict) # for "air force"
dfmt <- dfm(toks) %>% 
    dfm_select("^[A-Za-z]+$", valuetype = "regex") %>% 
    dfm_remove(stopwords('en')) %>% 
    dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile", 
             max_docfreq = 0.2, docfreq_type = "prop")
```

Many of the top terms of the seeded-LDA are seed words but other topic
words are also identified.

``` r
set.seed(1234)
slda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
print(terms(slda, 20))
##       economy     politics        society    diplomacy       military  
##  [1,] "company"   "parliament"    "police"   "diplomatic"    "army"    
##  [2,] "money"     "congress"      "school"   "embassy"       "navy"    
##  [3,] "market"    "politicians"   "hospital" "ambassador"    "soldiers"
##  [4,] "bank"      "parliamentary" "prison"   "treaty"        "marine"  
##  [5,] "industry"  "lawmakers"     "reported" "diplomat"      "north"   
##  [6,] "banks"     "voters"        "local"    "diplomats"     "defense" 
##  [7,] "markets"   "lawmaker"      "video"    "syria"         "korea"   
##  [8,] "banking"   "politician"    "media"    "syrian"        "south"   
##  [9,] "china"     "european"      "women"    "iran"          "nuclear" 
## [10,] "chinese"   "minister"      "court"    "israel"        "korean"  
## [11,] "percent"   "eu"            "found"    "security"      "missile" 
## [12,] "economic"  "party"         "man"      "weapons"       "nato"    
## [13,] "countries" "uk"            "service"  "daesh"         "air"     
## [14,] "year"      "prime"         "several"  "terrorist"     "forces"  
## [15,] "india"     "british"       "children" "turkish"       "japan"   
## [16,] "project"   "german"        "another"  "turkey"        "security"
## [17,] "oil"       "political"     "swedish"  "group"         "kim"     
## [18,] "billion"   "union"         "public"   "forces"        "aircraft"
## [19,] "trade"     "world"         "years"    "international" "missiles"
## [20,] "million"   "germany"       "million"  "un"            "meeting" 
##       nature         other   
##  [1,] "human"        "now"   
##  [2,] "sand"         "like"  
##  [3,] "water"        "even"  
##  [4,] "trump"        "think" 
##  [5,] "donald"       "just"  
##  [6,] "house"        "going" 
##  [7,] "information"  "many"  
##  [8,] "report"       "say"   
##  [9,] "department"   "way"   
## [10,] "program"      "want"  
## [11,] "washington"   "world" 
## [12,] "former"       "really"
## [13,] "media"        "years" 
## [14,] "white"        "see"   
## [15,] "show"         "get"   
## [16,] "intelligence" "know"  
## [17,] "details"      "need"  
## [18,] "news"         "come"  
## [19,] "foreign"      "well"  
## [20,] "campaign"     "back"
```

``` r
topic <- table(topics(slda))
print(topic)
## 
## diplomacy   economy  military    nature     other  politics   society 
##       150       125       121       132       147       126       199
```
