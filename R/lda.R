#' Unsupervised Latent Dirichlet allocation
#'
#' Implements unsupervised Latent Dirichlet allocation (LDA). Users can run
#' Seeded LDA by setting `gamma > 0`.
#'
#' @param x the dfm on which the model will be fit.
#' @param k the number of topics.
#' @param max_iter the maximum number of iteration in Gibbs sampling.
#' @param auto_iter if `TRUE`, stops Gibbs sampling on convergence before
#'   reaching `max_iter`. See details.
#' @param batch_size split the corpus into the smaller batches (specified in
#'   proportion) for distributed computing; it is disabled when a batch include
#'   all the documents `batch_size = 1.0`. See details.
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
#'
#'   If `batch_size < 1.0`, the corpus is partitioned into sub-corpora of
#'   `ndoc(x) * batch_size` documents for Gibbs sampling in sub-processes with
#'   synchronization of parameters in every 10 iteration. Parallel processing is
#'   more efficient when `batch_size` is small (e.g. 0.01) but the sub-corpora
#'   should be sufficiently large to produce results similar to those from
#'   serial processing. The algorithm is the Approximate Distributed LDA
#'   proposed by  Newman et al. (2009)
#'
#'   If `auto_iter = TRUE`, the iteration stops even before `max_iter` when
#'   `delta <= 0`. `delta` is computed to measure the changes in the number of
#'   words whose topics are updated by the Gibbs sampler in every 100 iteration
#'   as shown in the verbose message.
#'
#' @return `textmodel_seededlda()` and `textmodel_lda()` returns a list of model
#'   parameters. `theta` is the distribution of topics over documents; `phi` is
#'   the distribution of words over topics. `alpha` and `beta` are the small
#'   constant added to the frequency of words to estimate `theta` and `phi`,
#'   respectively, in Gibbs sampling. Other elements in the list subject to
#'   change.
#' @references
#'
#' Newman, D., Asuncion, A., Smyth, P., & Welling, M. (2009). Distributed
#' Algorithms for Topic Models. The Journal of Machine Learning Research, 10,
#' 1801–1828.
#' @keywords textmodel
#' @seealso [topicmodels][topicmodels::LDA]
#' @export
#' @examples
#' \donttest{
#' require(seededlda)
#' require(quanteda)
#'
#' corp <- head(data_corpus_moviereviews, 500)
#' toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
#' dfmt <- dfm(toks) %>%
#'     dfm_remove(stopwords('en'), min_nchar = 2) %>%
#'     dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile",
#'              max_docfreq = 0.1, docfreq_type = "prop")
#'
#' lda <- textmodel_lda(head(dfmt, 450), 6)
#' terms(lda)
#' topics(lda)
#' lda2 <- textmodel_lda(tail(dfmt, 50), model = lda) # new documents
#' topics(lda2)
#' }
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, gamma = 0,
    model = NULL, batch_size = 1.0, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, gamma = 0,
    model = NULL, batch_size = 1.0, verbose = quanteda_options("verbose")
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
    lda(x, k, label, max_iter, auto_iter, alpha, beta, gamma, NULL, words, batch_size, verbose)
}

is.textmodel_lda <- function(x) {
    "textmodel_lda" %in% class(x)
}


#' @importFrom methods as
#' @import quanteda
#' @useDynLib seededlda, .registration = TRUE
lda <- function(x, k, label, max_iter, auto_iter, alpha, beta, gamma,
                seeds, words, batch_size, verbose) {

    k <- check_integer(k, min = 1, max = 1000)
    max_iter <- check_integer(max_iter, min = 100)
    auto_iter <- check_logical(auto_iter, strict = TRUE)
    alpha <- check_double(alpha, min = 0)
    beta <- check_double(beta, min = 0)
    gamma <- check_double(gamma, min = 0, max = 1)
    batch_size <- check_double(batch_size, min = 0, max = 1)
    verbose <- check_logical(verbose, strict = TRUE)

    if (is.null(seeds))
        seeds <- Matrix::Matrix(0, nrow = nfeat(x), ncol = k)
    if (is.null(words))
        words <- Matrix::Matrix(0, nrow = nfeat(x), ncol = k)
    if (auto_iter) {
        min_delta <- 0.0
    } else {
        min_delta <- -1.0
    }

    first <- !duplicated(docid(x))
    if (all(first) && gamma)
        warning("gamma has no effect when docid are all unique.", call. = FALSE, immediate. = TRUE)
    if (batch_size == 0)
        stop("batch_size musht be larger than 0", call. = FALSE)
    random <- sample.int(.Machine$integer.max, 1) # seed for random number generation
    batch <- ceiling(ndoc(x) * batch_size)
    thread <- check_integer(getOption("seededlda_threads", -1))

    result <- cpp_lda(x, k, max_iter, min_delta, alpha, beta, gamma,
                      as(seeds, "dgCMatrix"), as(words, "dgCMatrix"),
                      first, random, batch, verbose, thread)

    dimnames(result$words) <- list(colnames(x), label)
    dimnames(result$phi) <- list(label, colnames(x))
    dimnames(result$theta) <- list(rownames(x), label)
    result$data <- x
    result$batch_size <- batch_size
    result$call <- match.call(sys.function(-2), call = sys.call(-2))
    result$version <- utils::packageVersion("seededlda")
    class(result) <- c("textmodel_lda", "textmodel", "list")
    return(result)
}
