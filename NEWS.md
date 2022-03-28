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
