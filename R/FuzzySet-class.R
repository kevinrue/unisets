#' @importFrom BiocGenerics cbind eval
NULL

setMethod("subset", "FuzzySets", function(x, ...)  {
    .local <- function (x, subset, select, drop = FALSE, ...) {
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
