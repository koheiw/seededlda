#' Sequential Latent Dirichlet allocation
#'
#' Implements Sequential Latent Dirichlet allocation (Sequential LDA).
#' `textmodel_seqlda()` allows the users to classify sentences of texts. It
#' considers the topics of previous document in inferring the topics of currency
#' document. `textmodel_seqlda()` is a shortcut equivalent to
#' `textmodel_lda(gamma = 0.5)`. Seeded Sequential LDA is
#' `textmodel_seededlda(gamma = 0.5)`.
#' @inheritParams textmodel_lda
#' @export
#' @references
#'
#' Du, Lan et al. (2012). "Sequential Latent Dirichlet Allocation".
#' doi.org/10.1007/s10115-011-0425-1. *Knowledge and Information Systems*.
#'
#' Watanabe, Kohei & Baturo, Alexander. (forthcoming). "Seeded Sequential LDA: A
#' Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#' *Social Science Computer Review*.
#'
textmodel_seqlda <- function(
        x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, batch_size = 1.0,
        model = NULL, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_sequentiallda")
}
#' @export
textmodel_seqlda.dfm <- function(
        x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, batch_size = 1.0,
        model = NULL, verbose = quanteda_options("verbose")
) {
    textmodel_lda(x, k = k, max_iter = max_iter, auto_iter = auto_iter, alpha = alpha, beta = beta,
                  gamma = 0.5, batch_size = batch_size, model = model, verbose = verbose)
}
