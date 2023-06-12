
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

**seededlda** is an R package that implements Seeded LDA (Latent
Dirichlet Allocation) for semi-supervised topic modeling based on
**quanteda**. Initially, the package was a simple wrapper around the
**topicmodels** package, but it was fully rewritten in C++ using the
[GibbsLDA++ library](http://gibbslda.sourceforge.net/) and submitted to
CRAN as version 0.5 in 2020. The package was further developed to add
the sequential classification (Sequential LDA) and parallel computing
(Distributed LDA) capabilities and released as version 1.0 in 2023.

[**keyATM**](https://github.com/keyATM/keyATM) is the latest addition to
the semi-supervised topic models. The users of Seeded LDA are also
encouraged to download that package.

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

- Curini, L., & Vignoli, V. (2021). Committed Moderates and Uncommitted
  Extremists: Ideological Leaning and Parties’ Narratives on Military
  Interventions in Italy. Foreign Policy Analysis, 17(3), 1–20.
  <https://doi.org/10.1093/fpa/orab016>
