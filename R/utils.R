#' [experimental] Optimize the number of topics
#'
#' These functions help users to find the optimal number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @param ... not used.
#' @details `divergence()` computes the Kullback–Leibler distance between all
#' the pairs of topic vectors in `x$phi`. The average divergence score should
#' maximize when the chosen number of topic `k` is optimal (Deveaud et al.,
#' 2014).
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#' @export
divergence <- function(x, ...) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x, ..., weighted = FALSE) {
    div <- proxyC::dist(x$phi, method = "kullback")
    if (weighted)
        div <- div * tcrossprod((colSums(x$words) / sum(x$words)))
    diag(div) <- NA
    return(div)
}

