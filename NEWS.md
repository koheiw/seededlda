# Changes in v0.6.0

* Adds `predict()` to identify topics of unseen documents (#9)
* Allows selecting seed words based on their frequencies using `dfm_trim()` in `textmodel_seededlda()` via `...` (#8)

# Changes in v0.5.1

* topics() now returns factor with NA for empty documents
* Fix a bug in initializing LDA that leads to incorrect phi (#4 and #6)

# Changes in v0.5

* Implement original LDA estimator using the LDAGibbs++ library
