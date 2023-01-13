#' @rdname textmodel_seededlda
#' @param x the dfm on which the model will be fit
#' @param k the number of topics; determined automatically by the number of keys
#'   in `dictionary` in `textmodel_seededlda()`.
#' @param max_iter the maximum number of iteration in Gibbs sampling.
#' @param verbose logical; if `TRUE` print diagnostic information during
#'   fitting.
#' @param alpha the value to smooth topic-document distribution.
#' @param beta the value to smooth topic-word distribution.
#' @param gamma a parameter to determine change of topics between sentences or
#'   paragraphs. When `gamma > 0`, Gibbs sampling of topics for the current
#'   document is affected by the previous document's topics.
#' @param model a fitted LDA model; if provided, `textmodel_lda()` inherits
#'   parameters from an existing model. See details.
#' @details To predict topics of new documents (i.e. out-of-sample), first,
#'   create a new LDA model from a existing LDA model passed to `model` in
#'   `textmodel_lda()`; second, apply [topics()] to the new model. The `model`
#'   argument takes objects created either by `textmodel_lda()` or
#'   `textmodel_seededlda()`.
#' @return `textmodel_seededlda()` and `textmodel_lda()` returns a list of model
#'   parameters. `theta` is the distribution of topics over documents; `phi` is
#'   the distribution of words over topics. `alpha` and `beta` are the small
#'   constant added to the frequency of words to estimate `theta` and `phi`,
#'   respectively, in Gibbs sampling. Other elements in the list subject to
#'   change.
#' @keywords textmodel
#' @seealso [topicmodels][topicmodels::LDA]
#' @export
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, alpha = 0.5, beta = 0.1, gamma = 0,
    model = NULL, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, alpha = 0.5, beta = 0.1, gamma = 0,
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
        if (!is.null(model$gamma)) {
            gamma <- model$gamma
        } else {
            gamma <- 0
        }
        words <- model$words
        warning("k, alpha and beta values are overwriten by the fitted model", call. = FALSE)
    } else {
        label <- paste0("topic", seq_len(k))
        words <- NULL
    }
    lda(x, k, label, max_iter, alpha, beta, gamma, NULL, words, verbose)
}

#' @importFrom methods as
#' @import quanteda
#' @useDynLib seededlda, .registration = TRUE
lda <- function(x, k, label, max_iter, alpha, beta, gamma, seeds, words, verbose) {

    k <- check_integer(k, min = 1, max = 1000)
    alpha <- check_double(alpha, min = 0)
    beta <- check_double(beta, min = 0)
    gamma <- check_double(gamma, min = 0, max = 1)
    verbose <- check_logical(verbose)
    max_iter <- check_integer(max_iter)

    if (is.null(seeds))
        seeds <- Matrix::Matrix(0, nrow = nfeat(x), ncol = k)
    if (is.null(words))
        words <- Matrix::Matrix(0, nrow = nfeat(x), ncol = k)

    first <- !duplicated(docid(x))
    if (all(first) && gamma)
        warning("gamma has no effect when docid are all unique.", call. = FALSE, immediate. = TRUE)

    random <- sample.int(.Machine$integer.max, 1) # seed for random number generation
    result <- cpp_lda(x, k, max_iter, alpha, beta, gamma,
                      as(seeds, "dgCMatrix"), as(words, "dgCMatrix"),
                      first, random, verbose)

    dimnames(result$words) <- list(colnames(x), label)
    dimnames(result$phi) <- list(label, colnames(x))
    dimnames(result$theta) <- list(rownames(x), label)
    result$data <- x
    result$call <- match.call(sys.function(-2), call = sys.call(-2))
    result$version <- utils::packageVersion("seededlda")
    class(result) <- c("textmodel_lda", "textmodel", "list")
    return(result)
}

is.textmodel_lda <- function(x) {
    "textmodel_lda" %in% class(x)
}
