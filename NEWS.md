# Changes in v0.7.0

* Change the `textmodel_seededlda` object to save dictionary and related settings (#18)

# Changes in v0.6.0

* Add `predict()` to identify topics of unseen documents (#9)
* Allow selecting seed words based on their frequencies using `dfm_trim()` in `textmodel_seededlda()` via `...` (#8)

# Changes in v0.5.1

* Change `topics()` to return factor with NA for empty documents
* Fix a bug in initializing LDA that leads to incorrect phi (#4 and #6)

# Changes in v0.5

* Implement original LDA estimator using the LDAGibbs++ library
