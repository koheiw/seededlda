#' Semisupervised Latent Dirichlet allocation
#'
#' Implements semisupervised Latent Dirichlet allocation
#' (Seeded LDA). `textmodel_seededlda()` allows users to specify
#' topics using a seed word dictionary. Users can run Seeded Sequential LDA by
#' setting `gamma > 0`.
#' @inheritParams textmodel_lda
#' @param dictionary a [quanteda::dictionary()] with seed words that define
#'   topics.
#' @param levels levels of entities in a hierarchical dictionary to be used as
#'   seed words. See also [quanteda::dictionary].
#' @param residual the number of undefined topics. They are named "other" by
#'   default, but it can be changed via `base::options(seededlda_residual_name)`.
#' @param weight determines the size of pseudo counts given to matched seed words.
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
#'   Watanabe, Kohei & Baturo, Alexander. (2023). "Seeded Sequential LDA:
#'   A Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#'   doi:10.1177/08944393231178605. *Social Science Computer Review*.
#' @seealso [keyATM][keyATM::keyATM]
#' @examples
#' \donttest{
#' require(seededlda)
#' require(quanteda)
#'
#' corp <- head(data_corpus_moviereviews, 500)
#' toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
#' dfmt <- dfm(toks) %>%
#'     dfm_remove(stopwords("en"), min_nchar = 2) %>%
#'     dfm_trim(max_docfreq = 0.1, docfreq_type = "prop")
#'
#' dict <- dictionary(list(people = c("family", "couple", "kids"),
#'                         space = c("alien", "planet", "space"),
#'                         moster = c("monster*", "ghost*", "zombie*"),
#'                         war = c("war", "soldier*", "tanks"),
#'                         crime = c("crime*", "murder", "killer")))
#' lda_seed <- textmodel_seededlda(dfmt, dict, residual = TRUE, min_termfreq = 10,
#'                                 max_iter = 500)
#' terms(lda_seed)
#' topics(lda_seed)
#' }
#' @export
textmodel_seededlda <- function(
    x, dictionary, levels = 1,
    valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
    residual = 0, weight = 0.01, max_iter = 2000, auto_iter = FALSE,
    alpha = 0.5, beta = 0.1, gamma = 0, batch_size = 1.0,
    ..., verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_seededlda")
}

#' @export
textmodel_seededlda.dfm <- function(
    x, dictionary, levels = 1,
    valuetype = c("glob", "regex", "fixed"), case_insensitive = TRUE,
    residual = 0, weight = 0.01, max_iter = 2000, auto_iter = FALSE,
    alpha = 0.5, beta = 0.1, gamma = 0, batch_size = 1.0,
    ..., verbose = quanteda_options("verbose")
) {

    residual <- check_integer(residual, min_len = 1, max_len = 1, min = 0)
    weight <- check_double(weight, min_len = 0, max_len = Inf, min = 0, max = 1)
    levels <- check_integer(levels, min_len = 1, max_len = 100, min = 1)
    seeds <- tfm(x, dictionary, levels = levels, weight = weight, residual = residual,
                 ..., verbose = verbose)
    if (!identical(colnames(x), colnames(seeds)))
        stop("seeds must have the same features")
    k <- nrow(seeds)
    label <- rownames(seeds)

    result <- lda(x, k, label, max_iter, auto_iter, alpha, beta, gamma, t(seeds), NULL, batch_size, verbose)
    result$dictionary <- dictionary
    result$valuetype <- valuetype
    result$case_insensitive <- case_insensitive
    result$seeds <- seeds # TODO: change to omega?
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
        select <- check_character(select, min_len = 1, max_len = ncol(x$theta), strict = TRUE)
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
#' @importFrom Matrix Matrix
tfm <- function(x, dictionary, levels = 1,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = 1,
                old = FALSE,
                ...,
                verbose = quanteda_options("verbose")) {

    valuetype <- match.arg(valuetype)
    residual <- as.integer(residual)

    if (!quanteda::is.dictionary(dictionary))
        stop("dictionary must be a dictionary object", call. = FALSE)

    dict <- flatten_dictionary(dictionary, levels)
    key <- names(dict)
    feat <- featnames(x)
    len <- length(key)
    total <- sum(x)

    if (length(weight) == 1) {
        weight <- rep(weight, len)
    } else {
        if (length(weight) != len)
            stop("The length of weight must be 1 or equal to dictionary", call. = FALSE)
    }

    x <- dfm_trim(x, ..., verbose = FALSE)
    x <- dfm_group(x, rep("text", ndoc(x)))
    y <- Matrix(nrow = 0, ncol = length(feat), sparse = TRUE)
    for (i in seq_along(dict)) {
        temp <- dfm_select(x, pattern = dict[i], verbose = FALSE)
        temp <- dfm_match(temp, features = feat)
        y <- rbind(y, as(temp, "dgCMatrix"))
    }
    rownames(y) <- key

    if (old) {
    	result <- (y > 0) * total * weight
    } else {
    	result <- y * weight * 100
    }

    if (residual > 0) {
        label <- getOption("seededlda_residual_name", "other")
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
