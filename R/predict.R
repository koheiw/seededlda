#' @export
predict.textmodel_lda <- function(x, newdata = NULL, max_iter = 2000, verbose = quanteda_options("verbose")) {

    max_iter <- as.integer(max_iter)
    if (!is.null(newdata)) {
        data <- newdata
    } else {
        data <- x$data
    }

    data <- dfm_match(data, colnames(x$phi))
    label <- rownames(x$phi)
    temp <- lda(data, x$k, label, max_iter, x$alpha, x$beta, NULL, x$words, verbose)
    result <- topics(temp)
    return(result)
}
