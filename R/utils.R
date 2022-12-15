#' \[Experimental\] Compute the divergence of topics
#'
#' Compute the divergence of topics. This can be used to search the optimal
#' number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()].
#' @param weighted if `TRUE` weight the divergence scores by the sizes of
#'   topics.
#' @param min_size the minimum size of topics that can increase the average
#'   divergence. Ignored when `weighted = FALSE`.
#' @param select names of topics for which the divergence is computed.
#' @details `divergence()` computes the average Jensen-Shannon divergence
#'   between all the pairs of topic vectors in `x$phi`. The divergence score
#'   maximizes when the chosen number of topic `k` is optimal (Deveaud et al.,
#'   2014).
#' @seealso [sizes]
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#' @export
divergence <- function(x, weighted = TRUE, min_size = 0.01, select = NULL) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x, weighted = TRUE, min_size = 0.01,
                                     select = NULL) {

    weighted <- check_logical(weighted)
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
    if (weighted) {
        p <- colSums(x$words) / sum(x$words)
    } else {
        min_size <- 0
        p <- rep(1 / ncol(x$word), ncol(x$word))
    }
    w <- tcrossprod(p[l]) - (min_size ^ 2)
    sum(div[l, l] * w, na.rm = TRUE) + (min_size ^ 2)
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

#' @importFrom quanteda dfm_weight as.dfm
get_entropy <- function(x, base = 2) {
    #x <- t(x)
    x <- dfm_weight(as.dfm(x), "prop")
    x <- as(x, "dgTMatrix")
    result <- sapply(split(x@x, factor(x@i + 1L, levels = seq_len(nrow(x)))),
                            function(y) sum(y * log(y, base)) * -1)
    names(result) <- rownames(x)
    return(result)
}

#' @importFrom quanteda is.dfm
get_weight <- function(x, y) {
    if (!is.dfm(x) || !is.dfm(y))
        stop('x and y have to be dfm')
    y <- dfm_weight(y, scheme = "boolean")
    cp <- Matrix::crossprod(x, y)
    return(1 - get_entropy(cp, nrow(cp)))
}
