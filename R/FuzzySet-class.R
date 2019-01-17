#' @importFrom BiocGenerics cbind eval
NULL

setMethod("subset", "FuzzySet", function(x, ...)  {
    .local <- function (x, subset, select, drop = FALSE, ...) {
        table <- cbind(x@map, membership=x@membership)

        i <- eval(substitute(subset), table)
        map <- x@map[i, ]
        membership <- x@membership[i]

        elementData <- x@elementData[unique(map$element), ]
        setData <- x@setData[unique(map$set), ]

        FuzzySet(map, elementData, setData, membership=membership)
    }
    .local(x, ...)
})

setMethod("show", "FuzzySet", function(object) {
    # Format the object
    x <- object@map
    x[["membership"]] <- object@membership
    x[["elementData"]] <- object@elementData[x$element, ]
    x[["setData"]] <- object@setData[x$set, ]

    .showSetAsTable(class(object), x)
})

#' @param x An object that inherits from `FuzzySet`.
#'
#' @rdname FuzzySet-class
#' @aliases as.list.FuzzySet
#' @importFrom methods as
#' @export
as.list.FuzzySet <- function(x, ...) {
    as(x, "list")
}
