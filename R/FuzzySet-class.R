#' @importFrom BiocGenerics cbind eval
NULL

setMethod("subset", "FuzzySets", function(x, ...)  {
    .local <- function (x, subset, select, drop=FALSE, ...) {
        table <- cbind(x@map, membership=x@membership)

        i <- eval(substitute(subset), table)
        map <- x@map[i, ]
        membership <- x@membership[i]

        elementData <- x@elementData[unique(map$element), ]
        setData <- x@setData[unique(map$set), ]

        FuzzySets(map, elementData, setData, membership=membership)
    }
    .local(x, ...)
})

setMethod("show", "FuzzySets", function(object) {
    # Format the object
    x <- object@map
    x[["membership"]] <- object@membership
    x[["elementData"]] <- object@elementData[x$element, ]
    x[["setData"]] <- object@setData[x$set, ]

    .showSetAsTable(class(object), x)
})

#' @param x An object that inherits from `FuzzySets`.
#'
#' @rdname FuzzySets-class
#' @aliases as.list.FuzzySets
#' @importFrom methods as
#' @export
as.list.FuzzySets <- function(x, ...) {
    as(x, "list")
}

#' @importFrom reshape2 acast
setAs("FuzzySets", "matrix", function(from) {
    x <- cbind(as.data.frame(from@map), value=from@membership)
    out <- reshape2::acast(x, element ~ set, value.var="value", fill=0)
    out
})

#' @rdname FuzzySets-class
#' @aliases as.matrix.FuzzySets
#' @importFrom methods as
#' @export
as.matrix.FuzzySets <- function(x, ...)  {
    as(x, "matrix")
}

#' @importFrom reshape2 melt
#' @importFrom S4Vectors DataFrame
setAs("matrix", "FuzzySets", function(from) {
    storage.mode(from) <- "double"
    x <- melt(from, varnames=c("element", "set"), as.is=TRUE)
    x <- x[!is.na(x$value), ]
    map <- x[, c("element", "set")]
    map <- DataFrame(map)
    membership <- x$value
    FuzzySets(map, membership=membership)
})

#' @aliases as.FuzzySets.matrix as.FuzzySets
#' @importFrom methods as
as.FuzzySets.matrix <- function(x, ...)  {
    as(x, "FuzzySets")
}
