#' Prediction method for textmodel_lda
#'
#' Predicts topics of documents with a fitted LDA model. Prediction is performed
#' by a Gibbs sampling with words allocated to topics in the fitted LDA. The
#' result becomes different from `topics()` even for the same documents because
#' `predict()` triggers additional iterations.
#' @param object a fitted LDA textmodel
#' @param newdata dfm on which prediction should be made
#' @param ... not used
#' @inherit textmodel_seededlda
#' @export
predict.textmodel_lda <- function(object, newdata = NULL,
                                  max_iter = 2000,
                                  verbose = quanteda_options("verbose"), ...) {

    max_iter <- as.integer(max_iter)
    if (!is.null(newdata)) {
        data <- newdata
    } else {
        data <- object$data
    }

    data <- dfm_match(data, colnames(object$phi))
    label <- rownames(object$phi)
    temp <- lda(data, object$k, label, max_iter, object$alpha,
                object$beta, NULL, object$words, verbose, ...)
    result <- topics(temp)
    names(result) <- docnames(data)
    return(result)
}
