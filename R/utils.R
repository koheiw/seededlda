#' Optimize the number of topics for LDA
#'
#' `divergence()` computes the regularized topic divergence scores to help users
#' to find the optimal number of topics for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()].
#' @param min_size the minimum size of topics for regularized topic divergence.
#'   Ignored when `regularize = FALSE`.
#' @param select names of topics for which the divergence is computed.
#' @param regularize if `TRUE`, returns the regularized divergence.
#' @param newdata if provided, `theta` and `phi` are estimated through fresh
#'   Gibbs sampling.
#' @param ... additional arguments passed to [textmodel_lda].
#' @details `divergence()` computes the average Jensen-Shannon divergence
#'   between all the pairs of topic vectors in `x$phi`. The divergence score
#'   maximizes when the chosen number of topic `k` is optimal (Deveaud et al.,
#'   2014). The regularized divergence penalizes topics smaller than `min_size`
#'   to avoid fragmentation (Watanabe & Baturo, forthcoming).
#' @returns Returns a singple numeric value.
#' @seealso [perplexity]
#' @references Deveaud, Romain et al. (2014). "Accurate and Effective Latent
#'   Concept Modeling for Ad Hoc Information Retrieval".
#'   doi:10.3166/DN.17.1.61-84. *Document Numérique*.
#'
#'   Watanabe, Kohei & Baturo, Alexander. (2023). "Seeded Sequential LDA: A
#'   Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#'   doi:10.1177/08944393231178605. *Social Science Computer Review*.
#' @export
divergence <- function(x, min_size = 0.01, select = NULL,
                       regularize = TRUE, newdata = NULL, ...) {
    UseMethod("divergence")
}

#' @importFrom proxyC dist
#' @export
divergence.textmodel_lda <- function(x, min_size = 0.01, select = NULL,
                                     regularize = TRUE, newdata = NULL, ...) {

    min_size <- check_double(min_size, min = 0, max = 1)
    select <- check_character(select, min_len = 2, max_len = nrow(x$phi),
                              strict = TRUE, allow_null = TRUE)
    regularize <- check_logical(regularize, strict = TRUE)

    if (!is.null(newdata)) {
    	suppressWarnings({
    		x <- textmodel_lda(newdata, model = x, ...)
    	})
    }

    if (is.null(select)) {
        l <- rep(TRUE, nrow(x$phi))
    } else {
        if (any(!select %in% rownames(x$phi)))
            stop("Selected topics must be in the model", call. = FALSE)
        l <- rownames(x$phi) %in% select
    }

    div <- proxyC::dist(x$phi, method = "jensen")
    diag(div) <- NA
    if (regularize) {
        p <- colSums(x$words) / sum(x$words)
    } else {
        min_size <- 0
        p <- rep(1 / ncol(x$word), ncol(x$word))
    }
    w <- tcrossprod(p[l]) - (min_size ^ 2)
    sum(as.matrix(div[l, l]) * w, na.rm = TRUE) + (min_size ^ 2)
}

#' Optimize the hyper-parameters for LDA
#'
#' `perplexity()` computes the perplexity score to help users to chose the
#' optimal values of hyper-parameters for LDA.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()].
#' @param newdata if provided, `theta` and `phi` are estimated through fresh
#'   Gibbs sampling.
#' @param ... additional arguments passed to [textmodel_lda].
#' @returns Returns a singple numeric value.
#' @details `perplexity()` predicts the distribution of words in the dfm based
#'   on `x$alpha` and `x$gamma` and then compute the sum of disparity between their
#'   predicted and observed frequencies. The perplexity score minimizes when the
#'   chosen values of hyper-parameters such as `k`, `alpha` and `gamma` are
#'   optimal.
#' @seealso [divergence]
#' @export
perplexity <- function(x, newdata = NULL, ...) {
	UseMethod("perplexity")
}

#' @export
perplexity.textmodel_lda <- function(x, newdata = NULL, ...) {
	if (!is.null(newdata)) {
		suppressWarnings({
			x <- textmodel_lda(newdata, model = x, ...)
		})
	}
	#exp(-sum(log(x$theta %*% x$phi[,featnames(x$data)]) * x$data) / sum(x$data))
	mat <- as(x$data, "TsparseMatrix")
	exp(-sum(log(colSums(x$phi[,mat@j + 1] * t(x$theta)[,mat@i + 1])) * mat@x) / sum(mat@x))
}


#' Compute the sizes of topics
#'
#' Compute the sizes of topics as the proportions of topic words in the corpus.
#' @param x a LDA model fitted by [textmodel_seededlda()] or [textmodel_lda()]
#' @returns a numeric vector in the same lengths as `k`.
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

#' Create a dictionary from topic terms
#'
#' `as.dictionary()` creates a [quanteda::dictionary] object from top topic terms.
#' @param x a model fitted by [textmodel_lda()].
#' @export
#' @param n the number of terms in the dictionary for each topic.
#' @param separator the character in between multi-word dictionary values.
#' @param ... not used.
#' @return Returns a [quanteda::dictionary] object.
#' @method as.dictionary textmodel_lda
as.dictionary.textmodel_lda <- function(x, n = 10, separator = NULL, ...) {
	if (is.null(separator)) {
		if (is.null(x$concatenator)) {
			# for v1.4.2 or earlier
			separator <- meta(x$data, field = "concatenator", type = "object")
		} else {
			separator <- x$concatenator
		}
	}
	lis <- as.list(as.data.frame.matrix(terms(x, n = n)))
	dictionary(lis, separator = separator)
}

get_threads <- function() {

	# respect other settings
	default <- c("tbb" = as.integer(Sys.getenv("RCPP_PARALLEL_NUM_THREADS")),
				 "omp" = as.integer(Sys.getenv("OMP_THREAD_LIMIT")),
				 "max" = cpp_get_max_thread())
	default <- unname(min(default, na.rm = TRUE))
	suppressWarnings({
		value <- as.integer(getOption("seededlda_threads", default))
	})
	if (length(value) != 1 || is.na(value)) {
		stop("seededlda_threads must be an integer")
	}
	return(value)
}

#' Get information on TBB library
#' @keywords internal
#' @export
info_tbb <- function() {
	list("enabled" = cpp_tbb_enabled(),
		 "max_threads" = cpp_get_max_thread())
}
