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
#' @param weight determines the size of pseudo counts given to seed words.
#' @param uniform if `FALSE`, adjusts the weights of seed words to make their
#'   total amount equal across topics.
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
    residual = 0, weight = 0.01, uniform = TRUE,
    max_iter = 2000, alpha = NULL, beta = NULL, gamma = 0,
    ..., verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_seededlda")
}

#' @export
textmodel_seededlda.dfm <- function(
    x, dictionary,
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = 0, weight = 0.01, uniform = TRUE,
    max_iter = 2000, alpha = NULL, beta = NULL, gamma = 0,
    ..., verbose = quanteda_options("verbose")
) {

    residual <- check_integer(residual, min_len = 1, max_len = 1, min = 0)
    seeds <- t(tfm(x, dictionary, weight = weight, residual = residual, uniform = uniform,
                   ..., verbose = verbose))
    if (!identical(colnames(x), rownames(seeds)))
        stop("seeds must have the same features")
    k <- ncol(seeds)
    label <- colnames(seeds)

    result <- lda(x, k, label, max_iter, alpha, beta, gamma, seeds, NULL, verbose)
    result$dictionary <- dictionary
    result$valuetype <- valuetype
    result$case_insensitive <- case_insensitive
    result$seeds <- seeds
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
#' `terms()` returns the most likely terms, or words, for topics based on the
#' `phi` parameter.
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

    apply(x$phi, 1, function(x, y, z) {
        head(y[order(x, decreasing = TRUE), drop = FALSE], z)
    }, colnames(x$phi), n)
}

#' Extract most likely topics
#'
#' `topics()` returns the most likely topics for documents based on the `theta`
#' parameter.
#' @export
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @param select returns the selected topic with the
#'   highest probability; specify by the names of columns in `x$theta`.
#' @param min_prob ignores topics if their probability is lower than this value.
#' @details Users can access the original matrix `x$theta` for likelihood
#'   scores; run `max.col(x$theta)` to obtain the same result as `topics(x)`.
topics <- function(x, min_prob = 0, select = NULL) {
    UseMethod("topics")
}
#' @export
#' @method topics textmodel_lda
topics.textmodel_lda <- function(x, min_prob = 0, select = NULL) {

    min_prob <- check_double(min_prob, min = 0, max = 1)
    if (is.null(select)) {
        j <- rep(TRUE, ncol(x$theta))
    } else {
        select <- check_character(select, min_len = 2, max_len = ncol(x$theta), strict = TRUE)
        if (any(!select %in% colnames(x$theta)))
            stop("Selected topics must be in the model", call. = FALSE)
        j <- colnames(x$theta) %in% select
    }

    theta <- x$theta[, j, drop = FALSE]
    k <- max.col(theta)
    if (min_prob > 0) {
        l <- theta[cbind(seq_along(k), k)] <= min_prob
    } else {
        l <- rep(FALSE, length(k))
    }
    result <- factor(k, labels = colnames(theta),
                     levels = seq_len(ncol(theta)))
    names(result) <- rownames(theta)
    result[rowSums(x$data) == 0 | l] <- NA
    return(result)
}

#' Internal function to construct topic-feature matrix
#' @noRd
#' @importFrom Matrix Matrix rowSums
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = 1,
                uniform = TRUE,
                old = FALSE,
                ...,
                verbose = quanteda_options("verbose")) {

    valuetype <- match.arg(valuetype)
    residual <- as.integer(residual)

    if (!quanteda::is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")

    key <- names(dictionary)
    feat <- featnames(x)

    x <- dfm_trim(x, ..., verbose = verbose)
    x <- dfm_group(x, rep("text", ndoc(x)))
    result <- Matrix(nrow = 0, ncol = length(feat), sparse = TRUE)
    for (i in seq_along(dictionary)) {
        temp <- dfm_select(x, pattern = dictionary[i])
        temp <- dfm_match(temp, features = feat)
        result <- rbind(result, as(temp, "dgCMatrix"))
    }

    if (length(weight) == 1)
        weight <- rep(weight, nrow(result))

    weight <- check_double(weight, min_len = nrow(result), max_len = nrow(result),
                           min = 0, max = 1)

    s <- sum(result)
    if (!old) {
        if (s > 0) {
            if (uniform) {
                p <- rep(1, nrow(result))
            } else {
                p <- (rowSums(result) / s) * nrow(result) # row profile
            }
            q <- colSums(result) / s # column profile
            w <- tcrossprod(p, q)
            weight <- weight * 100 # for compatibility with pre v0.9
            result <- (result > 0) * w * s * weight
        }
    } else {
        result <- (result > 0) * s * weight
    }
    # s <- colSums(result)
    # result <- result > 0
    # result <- t(t(result) * (s * weight))
    #result <- result * weight

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
    result <- as(result, "dgCMatrix")
    return(result)
}
