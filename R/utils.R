#' Optimize the number of topics
#'
#' `divergence()` computes the regularized topic divergence to find the optimal
#' number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()].
#' @param regularize if `TRUE`, compute the regularized divergence.
#' @param min_size the minimum size of topics for regularized topic divergence.
#'   Ignored when `regularize = FALSE`.
#' @param select names of topics for which the divergence is computed.
#' @details `divergence()` computes the average Jensen-Shannon divergence
#'   between all the pairs of topic vectors in `x$phi`. The divergence score
#'   maximizes when the chosen number of topic `k` is optimal (Deveaud et al.,
#'   2014). The regularized divergence penalizes topics smaller than `min_size`
#'   to avoid fragmentation (Watanabe & Baturo, forthcoming).
#' @seealso [sizes]
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#'
#'   Watanabe, Kohei & Baturo, Alexander. (forthcoming). "Seeded Sequential LDA:
#'   A Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#'   *Social Science Computer Review*.
#' @export
divergence <- function(x, regularize = TRUE, min_size = 0.01, select = NULL) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x, regularize = TRUE, min_size = 0.01,
                                     select = NULL) {

    regularize <- check_logical(regularize)
    min_size <- check_double(min_size, min = 0, max = 1)

    if (is.null(select)) {
        l <- rep(TRUE, nrow(x$phi))
    } else {
        select <- check_character(select, min_len = 2, max_len = nrow(x$phi), strict = TRUE)
        if (any(!select %in% rownames(x$phi)))
            stop("Selected topics must be in the model", call. = FALSE)
        l <- rownames(x$phi) %in% select
    }

    div <- proxyC::dist(x$phi, method = "jensen")
    diag(div) <- NA
    if (regularize) {
        p <- colSums(x$words) / sum(x$words)
    } else {
        min_size <- 0
        p <- rep(1 / ncol(x$word), ncol(x$word))
    }
    w <- tcrossprod(p[l]) - (min_size ^ 2)
    sum(as.matrix(div[l, l]) * w, na.rm = TRUE) + (min_size ^ 2)
}


#' Compute the sizes of topics
#'
#' Compute the sizes of topics as the proportions of topic words in the corpus.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @export
sizes <- function(x) {
    UseMethod("sizes")
}
#' @export
sizes.textmodel_lda <- function(x) {
    p <- colSums(x$words) / sum(x$words)
    names(p) <- rownames(x$phi) # TODO: consider set colnames to words
    return(p)
}
