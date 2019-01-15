
#' Validity Method for BaseSet Objects
#'
#' @rdname INTERNAL_valid_BaseSet
#'
#' @param object An object that inherits from `BaseSet`.
#'
#' @return If the object is valid, \code{TRUE};
#' otherwise, a character vector describing all the validity failures encountered.
#' @importFrom methods slot
.valid.BaseSet <- function(object){

    errors <- c()

    x <- slot(object, "map")
    if (!identical(colnames(x), c("element", "set"))) {
        error <- 'colnames(object@map) must be c("element", "set")'
        errors <- c(errors, error)
    }

    if (length(errors > 0)){
        return(errors)
    }

    return(TRUE)
}

#' BaseSet objects
#'
#' The `BaseSet` class implements a minimal container to describe distinct objects that make up sets.
#'
#' @slot map DataFrame.
#'
#' @return A `BaseSet` object.
#' @export BaseSet
#' @exportClass BaseSet
#' @importClassesFrom S4Vectors DataFrame
#' @importFrom S4Vectors DataFrame
#'
#' @examples
#' bs <- BaseSet()
BaseSet <- setClass(
    "BaseSet",
    slots=c(map="DataFrame"),
    prototype=list(
        map=DataFrame(
            element=character(0),
            set=character(0)
            )
        ),
    validity=.valid.BaseSet
    )

