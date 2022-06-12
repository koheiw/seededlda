#' Semisupervised Latent Dirichlet allocation
#'
#' `textmodel_seededlda()` implements semisupervised Latent Dirichlet allocation
#' (seeded-LDA). The estimator's code adopted from the GibbsLDA++ library
#' (Xuan-Hieu Phan, 2007). `textmodel_seededlda()` allows users to specify
#' topics using a seed word dictionary.
#' @param dictionary a [quanteda::dictionary()] with seed words that define
#'   topics.
#' @param residual the number of undefined topics. They are named "other" by
#'   default, but it can be changed via `base::options(slda_residual_name)`.
#' @param weight pseudo count given to seed words as a proportion of total
#'   number of words in `x`.
#' @param valuetype see [quanteda::valuetype]
#' @param case_insensitive see [quanteda::valuetype]
#' @param ... passed to [quanteda::dfm_trim] to restrict seed words based on
#'   their term or document frequency. This is useful when glob patterns in the
#'   dictionary match too many words.
#' @references Lu, Bin et al. (2011). "Multi-aspect Sentiment Analysis with
#'   Topic Models". doi:10.5555/2117693.2119585. *Proceedings of the 2011 IEEE
#'   11th International Conference on Data Mining Workshops*.
#'
#'   Watanabe, Kohei & Zhou, Yuan (2020). "Theory-Driven Analysis of Large
#'   Corpora: Semisupervised Topic Classification of the UN Speeches".
#'   doi:10.1177/0894439320907027. *Social Science Computer Review*.
#'
#' @examples
#' \donttest{
#' require(seededlda)
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
#' lda2 <- textmodel_lda(tail(dfmt, 50), model = lda) # new documents
#' topics(lda2)
#'
#' # semisupervised LDA
#' dict <- dictionary(list(people = c("family", "couple", "kids"),
#'                         space = c("alien", "planet", "space"),
#'                         moster = c("monster*", "ghost*", "zombie*"),
#'                         war = c("war", "soldier*", "tanks"),
#'                         crime = c("crime*", "murder", "killer")))
#' slda <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10)
#' terms(slda)
#' topics(slda)
#'
#' }
#' @export
textmodel_seededlda <- function(
    x, dictionary,
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = 0, sort = FALSE, weight = 0.01,
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
    residual = 0, sort = FALSE, weight = 0.01,
    max_iter = 2000, alpha = NULL, beta = NULL,
    ..., verbose = quanteda_options("verbose")
) {
    residual <- as.integer(residual)
    seeds <- t(tfm(x, dictionary, weight = weight, residual = residual, ..., verbose = verbose))
    if (!identical(colnames(x), rownames(seeds)))
        stop("seeds must have the same features")
    k <- ncol(seeds)
    label <- colnames(seeds)

    result <- lda(x, k, label, sort, max_iter, alpha, beta, seeds, NULL, verbose)
    result$dictionary <- dictionary
    result$valuetype <- valuetype
    result$case_insensitive <- case_insensitive
    result$residual <- residual
    result$weight <- weight
    return(result)
}

#' Print method for a LDA model
#' @param x for print method, the object to be printed
#' @param ... unused
#' @method print textmodel_lda
#' @keywords internal
#' @export
print.textmodel_lda <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n", prettyNum(x$k, big.mark = ","), " topics; ",
        prettyNum(ndoc(x$data), big.mark = ","), " documents; ",
        prettyNum(nfeat(x$data), big.mark = ","), " features.",
        "\n", sep = "")
}

#' Extract most likely terms
#'
#' `terms()` returns the most likely terms, or words, for topics based on the `phi` parameter.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @param n number of terms to be extracted
#' @details Users can access the original matrix `x$phi` for likelihood scores.
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
#'
#' `topics()` returns the most likely topics for documents based on the `theta` parameter.
#' @export
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @details Users can access the original matrix `x$theta` for likelihood
#'   scores; run `max.col(x$theta)` to obtain the same result as `topics(x)`.
topics <- function(x) {
    UseMethod("topics")
}
#' @export
#' @method topics textmodel_lda
topics.textmodel_lda <- function(x) {
    result <- factor(max.col(x$theta), labels = colnames(x$theta),
                     levels = seq_len(ncol(x$theta)))
    names(result) <- rownames(x$theta)
    result[rowSums(x$data) == 0] <- NA
    return(result)
}

#' Internal function to construct topic-feature matrix
#' @noRd
#' @importFrom Matrix Matrix
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = 1,
                weight_scheme = c("all", "topic", "word"),
                ...,
                verbose = quanteda_options("verbose")) {

    residual <- as.integer(residual)
    valuetype <- match.arg(valuetype)
    weight_scheme <- match.arg(weight_scheme)

    if (!quanteda::is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    if (weight < 0)
        stop("weight must be pisitive a value")

    key <- names(dictionary)
    feat <- featnames(x)
    total <- sum(x)
    x <- dfm_trim(x, ..., verbose = verbose)
    x <- dfm_group(x, rep("text", ndoc(x)))
    result <- Matrix(nrow = 0, ncol = length(feat), sparse = TRUE)
    for (i in seq_along(dictionary)) {
        temp <- dfm_select(x, pattern = dictionary[i])
        if (weight_scheme == "all") {
            temp <- (dfm_match(temp, features = feat) > 0) * total * weight
        } else if (weight_scheme == "topic") {
            temp <- (dfm_match(temp, features = feat) > 0) * sum(temp) * weight
        } else {
            temp <- dfm_match(temp, features = feat) * weight
        }
        result <- rbind(result, as(temp, "dgCMatrix"))
    }
    if (residual > 0) {
        label <- getOption("slda_residual_name", "other")
        if (residual == 1) {
            key <- c(key, label)
        } else {
            key <- c(key, paste0(label, seq_len(residual)))
        }
        result <- rbind(result, Matrix(0, nrow = residual, ncol = length(feat), sparse = TRUE))
    }
    dimnames(result) <- list(key, feat)
    return(result)
}
