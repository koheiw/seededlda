#' Sequential Latent Dirichlet allocation
#'
#' Sequential LDA considered the words in previous and current documents in
#' inferring topics. `textmodel_seqlda()` is a shortcut function for Sequential
#' LDA, equivalent to `textmodel_lda(gamma = 0.5)`. Seeded Sequential LDA is
#' `textmodel_seededlda(gamma = 0.5)`.
#' @inheritParams textmodel_lda
#' @export
#' @references
#'
#' Du, Lan et al. (2012). "Sequential Latent Dirichlet Allocation".
#' doi.org/10.1007/s10115-011-0425-1. *Knowledge and Information Systems*.
#'
#' Watanabe, Kohei & Baturo, Alexander. (forthcoming). "Seeded Sequential LDA:
#' A Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#'
textmodel_seqlda <- function(
        x, k = 10, max_iter = 2000, alpha = 0.5, beta = 0.1,
        model = NULL, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_sequentiallda")
}
#' @export
textmodel_seqlda.dfm <- function(
        x, k = 10, max_iter = 2000, alpha = 0.5, beta = 0.1,
        model = NULL, verbose = quanteda_options("verbose")
) {
    textmodel_lda(x, k = k, max_iter = max_iter, alpha = alpha, beta = beta,
                  gamma = 0.5, model = model, verbose = verbose)
}
