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

#' @importFrom utils head tail
#' @importFrom BiocGenerics nrow ncol
#' @importFrom S4Vectors showAsCell classNameForDisplay
.showRelationsAsDataFrame <- function(x) {
    # Settings
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    nm <- nrow(x)
    nc <- ncol(x)
    ne <- length(unique(as.character(x$element)))
    ns <- length(unique(as.character(x$set)))

    # Display
    if (nm > 0) {
        if (nm <= (nhead + ntail + 1L)) {
            out <- as.matrix(format(as.data.frame(lapply(x,
                showAsCell), optional = TRUE)))
        }
        else {
            out <- rbind(as.matrix(format(as.data.frame(lapply(x,
                function(x) showAsCell(head(x, nhead))), optional = TRUE))),
                rbind(rep.int("...", nc)), as.matrix(format(as.data.frame(lapply(x,
                  function(x) showAsCell(tail(x, ntail))), optional = TRUE))))
        }
        classinfo <- matrix(unlist(lapply(x, function(x) {
            paste0("<", classNameForDisplay(x)[1], ">")
        }), use.names = FALSE), nrow = 1, dimnames = list("",
            colnames(out)))
        out <- rbind(classinfo, out)
        print(out, quote = FALSE, right = TRUE)
    }
}
