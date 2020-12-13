#' @rdname textmodel_seededlda
#' @param x the dfm on which the model will be fit
#' @param k the number of topics
#' @param max_iter the maximum number of iteration in Gibbs sampling.
#' @param verbose logical; if `TRUE` print diagnostic information during
#'   fitting.
#' @param alpha the hyper parameter for topic-document distribution
#' @param beta the hyper parameter for topic-word distribution
#' @keywords textmodel experimental
#' @seealso [topicmodels][topicmodels::LDA]
#' @export
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {

    label <- paste0("topic", seq_len(k))
    lda(x, k, label, max_iter, alpha, beta, NULL, verbose)
}

#' @importFrom methods as
#' @import quanteda
#' @useDynLib seededlda, .registration = TRUE
lda <- function(x, k, label, max_iter, alpha, beta, seeds, verbose) {

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
        seeds <- as(Matrix::Matrix(0, nrow = nfeat(x), ncol = k), "dgCMatrix") # empty seed word matrix

    seed <- sample.int(.Machine$integer.max, 1) # seed for random number generation
    result <- cpp_lda(x, k, max_iter, alpha, beta, seeds, seed, verbose)

    dimnames(result$phi) <- list(label, colnames(x))
    dimnames(result$theta) <- list(rownames(x), label)
    result$x <- x
    result$max_iter <- max_iter
    result$call <- match.call()
    class(result) <- c("textmodel_lda", "textmodel", "list")
    return(result)
}
