#' @importFrom BiocGenerics eval
NULL

setMethod("subset", "BaseSet", function(x, ...)  {
    .local <- function (x, subset, select, drop = FALSE, ...) {
        table <- x@map

        i <- eval(substitute(subset), table)
        map <- x@map[i, ]

        elementData <- x@elementData[unique(map$element), ]
        setData <- x@setData[unique(map$set), ]

        BaseSet(map, elementData, setData)
    }
    .local(x, ...)
})

setMethod("show", "BaseSet", function(object) {
    # Format the object
    x <- object@map
    x[["elementData"]] <- object@elementData[x$element, ]
    x[["setData"]] <- object@setData[x$set, ]

    .showSetAsTable(class(object), x)
})

setAs("BaseSet", "list", function(from) {
    split(from@map$element, from@map$set)
})

#' @param ... Additional arguments passed to and from methods.
#'
#' @rdname BaseSet-class
#' @aliases as.list.BaseSet as.list
#' @importFrom methods as
#' @export
as.list.BaseSet <- function(x, ...) {
    as(x, "list")
}

#' @param x An object that inherits from `BaseSet`.
#'
#' @rdname BaseSet-class
#' @aliases setLengths,BaseSet-method
#' @importFrom methods as
setMethod("setLengths", "BaseSet", function(x) {
    x <- as(x, "list")
    lengths(x)
})

#' @rdname BaseSet-class
#' @aliases elementLengths,BaseSet-method
#' @importFrom methods as
setMethod("elementLengths", "BaseSet", function(x) {
    x <- split(x@map$set, x@map$element)
    lengths(x)
})
