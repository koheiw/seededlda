#' Fit seeded LDA with dictionary
#' @param x \code{dfm}
#' @param dictionary a dictionary
#' @param weight pseudo-count given to words in dictionary to adjust prior
#'   probability of topics over words
#' @param residual if \code{TRUE} a residual topic will be added to user-defined
#'   topics for documents that do not fall into any of them
#' @param ... additional arguments to specify how to interpret the dictionary
#' @import topicmodels
#' @export
textmodel_seededlda <- function(x, dictionary, weight = 500, residual = FALSE, ...) {
    dtm <- convert(x, "topicmodels")
    y <- tfm(x, dictionary, weight = weight, residual = residual, ...)
    dtm_seed <- quanteda:::dfm2dtm(y, omit_empty = FALSE)
    result <- list(
        lda = LDA(
            dtm,
            k = dtm_seed$nrow,
            method = "Gibbs",
            seedwords = dtm_seed
        ),
        dictionary = dictionary,
        topics = rownames(dtm_seed),
        documents = docnames(x)
    )
    class(result) <- "textmodel_seededlda"
    return(result)
}

#' Internal function to construct topic-feature matrix
#' @import Matrix quanteda
#' @noRd
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 500, scheme = c("absolute", "relative"),
                residual = TRUE) {

    valuetype <- match.arg(valuetype)
    scheme <- match.arg(scheme)

    id_key <- id_feat <- integer()
    for (i in seq_along(dictionary)) {
        f <- featnames(dfm_select(x, dictionary[i]))
        id_key <- c(id_key, rep(i, length(f)))
        id_feat <- c(id_feat, match(f, featnames(x)))
    }
    weight <- rep(weight, length(id_feat))
    key <- names(dictionary)
    if (residual)
        key <- c(key, "other")
    if (scheme == "relative")
        weight <- weight * colSums(x)[id_feat]
    result <- Matrix::sparseMatrix(
            i = id_key,
            j = id_feat,
            x = weight,
            dims = c(length(key), nfeat(x)),
            dimnames = list(key, featnames(x))
        )
    as.dfm(result)
}

#' Extract most likely topics
#' @export
#' @param x a fitted seeded LDA model
topics <- function(x) {
    UseMethod("topics")
}

#' @export
topics.textmodel_seededlda <- function(x) {
    i <- topicmodels::topics(x$lda)
    i <- i[x$documents]
    topic <- x$topics[i]
    return(topic)
}

#' Extract most likely terms
#' @param x a fitted seeded LDA model
#' @param k number of terms to be extracted
#' @importFrom topicmodels terms
#' @export
terms <- function(x, k = 10) {
    UseMethod("terms")
}

#' @export
terms.textmodel_seededlda <- function(x, k = 10) {
    term <- topicmodels::terms(x$lda, k)
    if (k == 1) {
        names(term) <- x$topics
    } else {
        colnames(term) <- x$topics
    }
    return(term)
}

