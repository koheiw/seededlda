
# seededlda: the package for semi-supervised topic modeling

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
semi-supervised topic modeling using **quanteda**. The seeded-LDA model
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
for the overview of semi-supervised topic classification techniques and
their advantages in social science research.

[**keyATM**](https://github.com/keyATM/keyATM) is the latest addition to
the semi-supervised topic models. The users of seeded-LDA are also
encouraged to use that package.

## Installation

From CRAN:

``` r
install.packages("seededlda")
```

From Github:

``` r
devtools::install_github("koheiw/seededlda")
```

## Examples

Please visit the package website for examples:

- [Introduction](https://koheiw.github.io/seededlda/articles/pkgdown/basic.html):
  basic functions of the package
- [Distributed
  LDA](https://koheiw.github.io/seededlda/articles/pkgdown/distributed.html):
  topic modeling with parallel computing
- [Seeded
  LDA](https://koheiw.github.io/seededlda/articles/pkgdown/seeded.html):
  semi-supervised topic modeling
- [Sequential
  LDA](https://koheiw.github.io/seededlda/articles/pkgdown/sequential.html):
  sentence-level topic modeling

Please read the following papers on the algorithms.

- Watanabe, K., & Baturo, A. (2023). Seeded Sequential LDA: A
  Semi-Supervised Algorithm for Topic-Specific Analysis of Sentences.
  Social Science Computer Review.
  <https://doi.org/10.1177/08944393231178605>
- Watanabe, K. (2023). Speed Up Topic Modeling: Distributed Computing
  and Convergence Detection for LDA, [working
  paper](https://blog.koheiw.net/wp-content/uploads/2023/05/Distributed-LDA-02.pdf).

## Other publications

Please read the following papers for how to apply seeded-LDA in social
science research:

Curini, Luigi and Vignoli, Valerio. 2021. [Committed Moderates and
Uncommitted Extremists: Ideological Leaning and Parties’ Narratives on
Military Interventions in Italy](https://doi.org/10.1093/fpa/orab016),
*Foreign Policy Analysis*.
