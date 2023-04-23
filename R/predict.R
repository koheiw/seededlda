#' @noRd
#' @param object a fitted LDA textmodel.
#' @param ... not used
#' @keywords internal
#' @export
predict.textmodel_lda <- function(object, ...) {
    .Defunct(msg = "predict() is deprecated; use the model argument in textmodel_lda() to predict topics of new documents")
}
