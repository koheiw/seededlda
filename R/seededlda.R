#' Semisupervised Latent Dirichlet allocation
#'
#' `textmodel_seededlda()` implements semisupervised Latent Dirichlet allocation
#' (seeded-LDA). The estimator's code adopted from the GibbsLDA++ library
#' (Xuan-Hieu Phan, 2007). `textmodel_seededlda()` allows identification of
#' pre-defined topics by semisupervised learning with a seed word dictionary.
#' @param dictionary a [quanteda::dictionary()] with seed words that define
#'   topics.
#' @param residual if \code{TRUE} a residual topic (or "garbage topic") will be
#'   added to user-defined topics.
#' @param weight pseudo count given to seed words as a proportion of total
#'   number of words in `x`.
#' @param valuetype see [quanteda::valuetype]
#' @param case_insensitive see [quanteda::valuetype]
#' @param ... passed to [quanteda::dfm_trim] to restrict seed words based on
#'   their term or document frequency. This is useful when glob patterns in the
#'   dictionary match too many words.
#' @references Lu, Bin et al. (2011). [Multi-aspect Sentiment Analysis with
#'   Topic Models](https://dl.acm.org/doi/10.5555/2117693.2119585). *Proceedings
#'   of the 2011 IEEE 11th International Conference on Data Mining Workshops*.
#'
#'   Watanabe, Kohei & Zhou, Yuan (2020). [Theory-Driven Analysis of Large
#'   Corpora: Semisupervised Topic Classification of the UN
#'   Speeches](https://doi.org/10.1177/0894439320907027). *Social Science
#'   Computer Review*.
#'
#' @examples
#' \dontrun{
#' require(quanteda)
#'
#' data("data_corpus_moviereviews", package = "quanteda.textmodels")
#' corp <- head(data_corpus_moviereviews, 500)
#' toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
#' dfmt <- dfm(toks) %>%
#'     dfm_remove(stopwords('en'), min_nchar = 2) %>%
#'     dfm_trim(min_termfreq = 0.90, termfreq_type = "quantile",
#'              max_docfreq = 0.1, docfreq_type = "prop")
#'
#' # unsupervised LDA
#' lda <- textmodel_lda(head(dfmt, 450), 6)
#' terms(lda)
#' topics(lda)
#' predict(lda, newdata = tail(dfmt, 50))
#'
#' # semisupervised LDA
#' dict <- dictionary(list(people = c("family", "couple", "kids"),
#'                         space = c("alien", "planet", "space"),
#'                         moster = c("monster*", "ghost*", "zombie*"),
#'                         war = c("war", "soldier*", "tanks"),
#'                         crime = c("crime*", "murder", "killer")))
#' slda <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 100)
#' terms(slda)
#' topics(slda)
#' }
#' @export
textmodel_seededlda <- function(
    x, dictionary,
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = FALSE, weight = 0.01,
    max_iter = 2000, alpha = NULL, beta = NULL,
    ..., verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_seededlda")
}

#' @export
textmodel_seededlda.dfm <- function(
    x, dictionary,
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = FALSE, weight = 0.01,
    max_iter = 2000, alpha = NULL, beta = NULL,
    ..., verbose = quanteda_options("verbose")
) {

    seeds <- t(tfm(x, dictionary, weight = weight, residual = residual, ..., verbose = verbose))
    if (!identical(colnames(x), rownames(seeds)))
        stop("seeds must have the same features")
    k <- ncol(seeds)
    label <- colnames(seeds)
    lda(x, k, label, max_iter, alpha, beta, seeds, NULL, verbose, ...)
}

#' Print method for a LDA model
#' @param x for print method, the object to be printed
#' @param ... unused
#' @method print textmodel_lda
#' @keywords internal textmodel
#' @export
print.textmodel_lda <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Topics: ", x$k, "; ",
        ndoc(x$data), " documents; ",
        nfeat(x$data), " features.",
        "\n",
        sep = "")
}

#' Extract most likely terms
#' @param x a fitted LDA model
#' @param n number of terms to be extracted
#' @export
terms <- function(x, n = 10) {
    UseMethod("terms")
}
#' @export
#' @method terms textmodel_lda
#' @importFrom utils head
terms.textmodel_lda <- function(x, n = 10) {
    apply(x$phi, 1, function(x, y, z) head(y[order(x, decreasing = TRUE), drop = FALSE], z),
          colnames(x$phi), n)
}

#' Extract most likely topics
#' @export
#' @param x a fitted LDA model
topics <- function(x) {
    UseMethod("topics")
}
#' @export
#' @method topics textmodel_lda
topics.textmodel_lda <- function(x) {
    result <- factor(max.col(x$theta), labels = colnames(x$theta),
                     levels = seq_len(ncol(x$theta)))
    result[rowSums(x$data) == 0] <- NA
    return(result)
}

#' Internal function to construct topic-feature matrix
#' @noRd
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = TRUE,
                ...,
                verbose = quanteda_options("verbose")) {

    valuetype <- match.arg(valuetype)

    if (!quanteda::is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    if (weight < 0)
        stop("weight must be pisitive a value")

    key <- names(dictionary)
    feat <- featnames(x)
    count <- floor(sum(x)) * weight
    x <- dfm_trim(x, ..., verbose = verbose)
    x <- as.dfm(rbind(colSums(x)))
    result <- Matrix::Matrix(nrow = 0, ncol = length(feat), sparse = TRUE)
    for (i in seq_along(dictionary)) {
        temp <- dfm_select(x, pattern = dictionary[i])
        temp <- dfm_match(temp, features = feat) > 0
        result <- rbind(result, temp)
    }
    if (residual) {
        key <- c(key, "other")
        result <- rbind(result, Matrix::Matrix(0, nrow = 1, ncol = length(feat), sparse = TRUE))
    }
    result <- result * count
    dimnames(result) <- list(key, feat)
    return(result)
}
