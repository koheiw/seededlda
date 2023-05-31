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
#'   Du, Lan et al. (2012). "Sequential Latent Dirichlet Allocation".
#'   doi.org/10.1007/s10115-011-0425-1. *Knowledge and Information Systems*.
#'
#'   Watanabe, Kohei & Baturo, Alexander. (2023). "Seeded Sequential LDA:
#'   A Semi-supervised Algorithm for Topic-specific Analysis of Sentences".
#'   doi:10.1177/08944393231178605. *Social Science Computer Review*.
#' @examples
#' \donttest{
#' require(seededlda)
#' require(quanteda)
#'
#' corp <- head(data_corpus_moviereviews, 500) %>%
#'     corpus_reshape()
#' toks <- tokens(corp, remove_punct = TRUE, remove_symbols = TRUE, remove_number = TRUE)
#' dfmt <- dfm(toks) %>%
#'     dfm_remove(stopwords("en"), min_nchar = 2) %>%
#'     dfm_trim(max_docfreq = 0.01, docfreq_type = "prop")
#'
#' lda_seq <- textmodel_seqlda(dfmt, k = 6, max_iter = 500) # 6 topics
#' terms(lda_seq)
#' topics(lda_seq)
#' }
textmodel_seqlda <- function(
        x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, batch_size = 1.0,
        model = NULL, verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_seqlda")
}
#' @export
textmodel_seqlda.dfm <- function(
        x, k = 10, max_iter = 2000, auto_iter = FALSE, alpha = 0.5, beta = 0.1, batch_size = 1.0,
        model = NULL, verbose = quanteda_options("verbose")
) {
    textmodel_lda(x, k = k, max_iter = max_iter, auto_iter = auto_iter, alpha = alpha, beta = beta,
                  gamma = 0.5, batch_size = batch_size, model = model, verbose = verbose)
}
