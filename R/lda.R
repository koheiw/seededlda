#' @rdname textmodel_seededlda
#' @param x the dfm on which the model will be fit
#' @param k the number of topics
#' @param max_iter the maximum number of iteration in Gibbs sampling.
#' @param verbose logical; if `TRUE` print diagnostic information during
#'   fitting.
#' @param alpha the hyper parameter for topic-document distribution
#' @param beta the hyper parameter for topic-word distribution
#' @param model a fitted LDA model; if provided, `textmodel_lda()` inherits
#'   parameters from an existing model. See details.
#' @details To predict topics of new documents, first, fit a new model with
#'   new documents `x` and an existing LDA `model`; second, apply `topics()` to the
#'   new model. `model` takes objects created either by [textmodel_lda()] or
#'   [textmodel_seededlda()].
#'
#' @keywords textmodel
#' @seealso [topicmodels][topicmodels::LDA]
#' @export
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL,
    model = NULL, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL,
    model = NULL, verbose = quanteda_options("verbose")
) {

    if (!is.null(model)) {
        if (!is.textmodel_lda(model))
            stop("model must be a fitted textmodel_lda")
        x <- dfm_match(x, colnames(model$phi))
        k <- model$k
        label <- rownames(model$phi)
        alpha <- model$alpha
        beta <- model$beta
        words <- model$words
        warning("k, alpha and beta values are overitten by the fitted model", call. = FALSE)
    } else {
        label <- paste0("topic", seq_len(k))
        words <- NULL
    }
    lda(x, k, label, max_iter, alpha, beta, NULL, words, verbose)
}

#' @importFrom methods as
#' @import quanteda
#' @useDynLib seededlda, .registration = TRUE
lda <- function(x, k, label, max_iter, alpha, beta, seeds, words, verbose) {

    k <- as.integer(k)
    max_iter <- as.integer(max_iter)
    if (is.null(alpha)) {
        alpha <- -1.0 # default value will be set in C++
    } else {
        alpha <- as.double(alpha)
    }
    if (is.null(beta)) {
        beta <- -1.0 # default value will be set in C++
    } else {
        beta <- as.double(beta)
    }
    verbose <- as.logical(verbose)

    if (k < 1)
        stop("k must be larger than zero")

    if (is.null(seeds))
        seeds <- as(Matrix::Matrix(0, nrow = nfeat(x), ncol = k), "dgCMatrix")
    if (is.null(words))
        words <- as(Matrix::Matrix(0, nrow = nfeat(x), ncol = k), "dgCMatrix")

    random <- sample.int(.Machine$integer.max, 1) # seed for random number generation
    result <- cpp_lda(x, k, max_iter, alpha, beta, seeds, words, random, verbose)

    dimnames(result$phi) <- list(label, colnames(x))
    dimnames(result$theta) <- list(rownames(x), label)
    result$data <- x
    result$call <- match.call()
    class(result) <- c("textmodel_lda", "textmodel", "list")
    return(result)
}

is.textmodel_lda <- function(x) {
    "textmodel_lda" %in% class(x)
}
