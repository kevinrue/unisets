#' Package Settings
#'
#' @rdname INTERNAL_get_showHeadLines
#' @aliases get_showHeadLines get_showTailLines
#'
#' @return An integer scalar indicating the number of top and bottom lines that `show` methods should display.
#'
#' @author Kevin Rue-Albrecht, inspired from the `S4Vectors` package.
get_showHeadLines <- function() { 5L }

#' @rdname INTERNAL_get_showHeadLines
get_showTailLines <- function() { 5L }

#' Compact view of `BaseSets` objects as a character matrix.
#'
#' @rdname INTERNAL_make_naked_matrix_from_BaseSets
#'
#' @param x An object of class inheriting from [`BaseSets-class`].
#'
#' @importFrom S4Vectors showAsCell classNameForDisplay
#'
#' @note Adapted from `S4Vectors:::.make_naked_matrix_from_Hits`.
.make_naked_matrix_from_BaseSets <- function(x) {
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    x_len <- length(x)
    x_mcols <- mcols(relations(x))
    x_nmc <- if (is.null(x_mcols)) 0L else ncol(x_mcols)
    ans <- cbind(
        from = ids(elementData(x))[from(relations(x))],
        to = ids(setData(x))[to(relations(x))]
    )
    colnames(ans) <- c("element", "set")

    if (x_nmc > 0L) {
        tmp <- do.call(data.frame, c(lapply(x_mcols, showAsCell),
                                     list(check.names=FALSE)))
        ans <- cbind(ans, `|`=rep.int("|", x_len), as.matrix(tmp))
    }

    ans
}
