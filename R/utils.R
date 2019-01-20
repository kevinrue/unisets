#' Package Settings
#'
#' @rdname INTERNAL_get_showHeadLines
#' @aliases get_showHeadLines get_showTailLines
#'
#' @return An integer scalar indicating the number of top and bottom lines that `show` methods should display.
get_showHeadLines <- function() { 5L }

#' @rdname INTERNAL_get_showHeadLines
get_showTailLines <- function() { 5L }

#' @importFrom utils head tail
#' @importFrom BiocGenerics nrow ncol
#' @importFrom S4Vectors showAsCell classNameForDisplay
.showSetAsTable <- function(class, x) {
    # Settings
    nhead <- get_showHeadLines()
    ntail <- get_showTailLines()
    nm <- nrow(x)
    nc <- ncol(x)
    ne <- length(unique(x$element))
    ns <- length(unique(x$set))

    # Display
    cat(
        class, " with ",
        nm, ifelse(nm == 1, " relation", " relations"), " between ",
        ne, ifelse(ne == 1, " element", " elements"), " and ",
        ns, ifelse(ns == 1, " set\n", " sets\n"),
        sep = "")
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
