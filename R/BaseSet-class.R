#' @importFrom BiocGenerics eval
NULL

setMethod("subset", "BaseSets", function(x, ...)  {
    .local <- function (x, subset, select, drop = FALSE, ...) {
        table <- x@map

        i <- eval(substitute(subset), table)
        map <- x@map[i, ]

        elementData <- x@elementData[unique(map$element), ]
        setData <- x@setData[unique(map$set), ]

        BaseSets(map, elementData, setData)
    }
    .local(x, ...)
})

setMethod("show", "BaseSets", function(object) {
    # Format the object
    x <- object@map
    x[["elementData"]] <- object@elementData[x$element, ]
    x[["setData"]] <- object@setData[x$set, ]

    .showSetAsTable(class(object), x)
})

setAs("BaseSets", "list", function(from) {
    split(from@map$element, from@map$set)
})

#' @param ... Additional arguments passed to and from methods.
#'
#' @rdname BaseSets-class
#' @aliases as.list.BaseSets as.list
#' @importFrom methods as
#' @export
as.list.BaseSets <- function(x, ...) {
    as(x, "list")
}

#' @param x An object that inherits from `BaseSets`.
#'
#' @rdname BaseSets-class
#' @aliases setLengths,BaseSets-method
#' @importFrom methods as
setMethod("setLengths", "BaseSets", function(x) {
    x <- as(x, "list")
    lengths(x)
})

#' @rdname BaseSets-class
#' @aliases elementLengths,BaseSets-method
#' @importFrom methods as
setMethod("elementLengths", "BaseSets", function(x) {
    x <- split(x@map$set, x@map$element)
    lengths(x)
})
