#' \[Experimental\] Compute the divergence of topics
#'
#' Compute the divergence of topics. This can be used to search the optimal
#' number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()].
#' @param weighted if `TRUE` weight the divergence scores by the sizes of
#'   topics.
#' @param min_prop the minimum size of topics that can increase the average
#'   divergence. Ignored when `weighted = FALSE`.
#' @param select names of topics for which the divergence is computed.
#' @details `divergence()` computes the average Jensen-Shannon divergence
#'   between all the pairs of topic vectors in `x$phi`. The divergence score
#'   maximizes when the chosen number of topic `k` is optimal (Deveaud et al.,
#'   2014).
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#' @export
divergence <- function(x, weighted = TRUE, min_prop = 0.01, select = NULL) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x, weighted = TRUE, min_prop = 0.01, select = NULL) {

    weighted <- check_logical(weighted)
    min_prop <- check_double(min_prop, min = 0, max = 1)

    if (is.null(select)) {
        l <- rep(TRUE, nrow(x$phi))
    } else {
        select <- check_character(select, max_len = ncol(x$phi), strict = TRUE)
        if (any(!select %in% rownames(x$phi)))
            stop("Selected topics must be in the model", call. = FALSE)
        l <- rownames(x$phi) %in% select
    }
    if (sum(l) < 2)
        stop("At least two topics must be selected", call. = FALSE)

    div <- proxyC::dist(x$phi, method = "jensen")
    diag(div) <- NA
    if (weighted) {
        p <- colSums(x$words) / sum(x$words)
    } else {
        min_prop <- 0
        p <- rep(1 / ncol(x$word), ncol(x$word))
    }
    w <- tcrossprod(p[l]) - (min_prop ^ 2)
    sum(div[l, l] * w, na.rm = TRUE) + (min_prop ^ 2)
}


#' \[experimental\] Compute the sizes of topics
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
