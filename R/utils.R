#' Optimize the number of topics
#'
#' These functions help users to find the optimal number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @details
#' `divergence()` computes the average Kullback–Leibler distance between all the
#' pairs of topic vectors in `x$phi`. The divergence score maximizes when the
#' chosen number of topic `k` is optimal (Deveaud et al., 2014).
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#' @export
divergence <- function(x) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x) {
    div <- proxyC::dist(x$phi, method = "kullback")
    diag(div) <- NA
    Matrix::mean(div, na.rm = TRUE)
}
