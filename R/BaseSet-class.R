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

#' @rdname BaseSet-class
#' @aliases show,BaseSet-method
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

#' @rdname BaseSet-class
#' @aliases as.list.BaseSet as.list
#' @importFrom methods as
as.list.BaseSet <- function(object) {
    as(object, "list")
}

#' @param object An object that inherits from `BaseSet`.
#'
#' @rdname BaseSet-class
#' @aliases setLengths,BaseSet-method
#' @importFrom methods as
setMethod("setLengths", "BaseSet", function(object) {
    x <- as(object, "list")
    lengths(x)
})

#' @rdname BaseSet-class
#' @aliases elementLengths,BaseSet-method
#' @importFrom methods as
setMethod("elementLengths", "BaseSet", function(object) {
    x <- split(object@map$set, object@map$element)
    lengths(x)
})
