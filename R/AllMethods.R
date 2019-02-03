
# BaseSets ----

#' Methods for `BaseSets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`BaseSets`][BaseSets-class] class.
#' In the usage below, `x` represents an object of class inheriting from [`BaseSets`][BaseSets-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name BaseSets-methods
#' @rdname BaseSets-methods
#'
#' @param x An object of class inheriting from [`BaseSets`][BaseSets-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @seealso [`BaseSets-class`].
#'
#' @examples
#' # Setup ----
#'
#' example("BaseSets-class", echo=FALSE)
NULL

# FuzzyHits ----

#' Methods for `FuzzyHits` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzyHits`][FuzzyHits-class] class.
#' The `FuzzyHits` class directly extends [`Hits`][`Hits-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `x` represents an object of class inheriting from [`FuzzyHits`][FuzzyHits-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#'
#' @param x An object of class inheriting from [FuzzyHits][FuzzyHits-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @seealso [`FuzzyHits-class`], [FuzzySets-class].
#'
#' @examples
#' # Setup ----
#'
#' example("FuzzyHits-class", echo=FALSE)
NULL

# IdVector ----

#' Methods for `IdVector` objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`IdVector`][IdVector-class] class.
#' The `IdVector` class directly extends [`Vector`][`Vector-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `x` represents an object of class inheriting from [`IdVector`][IdVector-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name IdVector-methods
#' @rdname IdVector-methods
#'
#' @param x An object of class inheriting from [IdVector][IdVector-class].
#' @param value An object of a class specified in the S4 method signature.
#'
#' @seealso [`IdVector-class`], [`Vector`][Vector-class], [`Vector-setops`].
#'
#' @examples
#' # Setup ----
#'
#' example("IdVector-class", echo=FALSE)
NULL

# FuzzySets ----

#' Methods for `FuzzySets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzySets`][FuzzySets-class] class.
#' The `FuzzySets` class directly extends [`BaseSets`][`BaseSets-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `x` represents an object of class inheriting from [`FuzzySets`][FuzzySets-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzySets-methods
#' @rdname FuzzySets-methods
#'
#' @param x An object of class inheriting from [`FuzzySets`][FuzzySets-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @seealso [`FuzzySets-class`], [`BaseSets-methods`].
#'
#' @examples
#' # Setup ----
#'
#' example("FuzzySets-class", echo=FALSE)
NULL
