## Changes in v1.4.0

- Add `adjust_alpha` as an experimental argument to optimize `alpha` automatically.
- Add `update_model` to update terms of existing models to classify documents with unseen words more accurately.

## Changes in v1.3.2

- Improve the way to convert std::vector to arma::mat.

## Changes in v1.3.1

- Fix C++ code for Armadillo v14.

## Changes in v1.3.0

- Add `perplexity()` to compute perplexity scores of fitted LDA models.
- Improve documentation.

## Changes in v1.2.1

- Fix tests on systems when the TBB library is unavailable.

## Changes in v1.2.0

- The RcppParallel package is no longer required as the TBB library in the operating system (Linux and MacOS) or Rtools (Windows) is used.
- Linux and MacOS must have the TBB library to enable parallel computing before installing this package from the source.

## Changes in v1.1.1

* Allow `alpha` and `beta` to be a vector for asymmetric Dirichlet priors.

## Changes in v1.1.0

* Remove `uniform` to simplify the computation of seed word weights.
* Add `levels` argument to better handle hierarchical dictionaries.

## Changes in v1.0.1

* Fix the error when `textmodel_seqlda()` is called.
* Save values in the Array object in double to avoid rounding error (#60).

## Changes in v1.0.0

* Add `auto_iter` to `textmodel_seededlda()` and `textmodel_lda()` to stop Gibbs sampling automatically before `max_iter` is reached.
* Add `batch_size` to `textmodel_seededlda()` and `textmodel_lda()` to enable the distributed LDA algorithm for parallel computing.

## Changes in v0.9.0

* Add the gamma parameter to `textmodel_seededlda()` and `textmodel_lda()` for sequential classification.
* Add `textmodel_seqlda()` as as short cut for `textmodel_lda(gamma = 0.5)`.
* Improve the calculation of weights for seed words.
* Add the `regularize` argument to `divergence()` for the regularized topic divergence measure.

## Changes in v0.8.4

* Fix for deprecation in Matrix 1.5-4.

## Changes in v0.8.3

* Add `data_corpus_moviereviews` to the package to reduce dependency.

## Changes in v0.8.2

* Add `min_prob` and `select` to `topics()` for greater flexibility
* Change the divergence measure from Kullback-Leibler to Jensen-Shannon.
* Add `weighted`, `min_size`, `select` to `divergence()` for regularized topic divergence scores.

## Changes in v0.8.1

* Change `textmodel_seededlda()` to set positive integer values to `residual`.
* Fix a bug in `textmodel_seededlda()` that ignores n-grams when `concatenator` is not "_".
* Change `topics()` to return document names.
* Add `divergence()` to optimize the number of topics or the seed words (#26).

## Changes in v0.8.0

* Add the `model` argument to `textmodel_lda()` to replace `predict()`.

## Changes in v0.7.0

* Change the `textmodel_seededlda` object to save dictionary and related settings (#18)

## Changes in v0.6.0

* Add `predict()` to identify topics of unseen documents (#9)
* Allow selecting seed words based on their frequencies using `dfm_trim()` in `textmodel_seededlda()` via `...` (#8)

## Changes in v0.5.1

* Change `topics()` to return factor with NA for empty documents
* Fix a bug in initializing LDA that leads to incorrect phi (#4 and #6)

## Changes in v0.5

* Implement original LDA estimator using the LDAGibbs++ library
