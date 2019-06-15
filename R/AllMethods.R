
# Sets ----

#' Methods for `Sets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`Sets-class`] class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`Sets-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name Sets-methods
#' @rdname Sets-methods
#'
#' @param object,x An object of class inheriting from [`Sets-class`].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @importMethodsFrom methods show
#' @importMethodsFrom S4Vectors union c
#' @importMethodsFrom BiocGenerics duplicated
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`Sets-class`].
NULL

# FuzzyHits ----

#' Methods for `FuzzyHits` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzyHits-class`] class.
#' The `FuzzyHits` class directly extends [`Hits-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` represents an object of class inheriting from [`FuzzyHits-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#'
#' @param object An object of class inheriting from [`FuzzyHits-class`].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`FuzzyHits-class`], [`FuzzySets-class`].
NULL

# IdVector ----

#' Methods for `IdVector` objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`IdVector-class`] class.
#' The `IdVector` class directly extends [`Vector-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`IdVector-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name IdVector-methods
#' @rdname IdVector-methods
#'
#' @param object,x An object of class inheriting from [`IdVector-class`].
#' @param value An object of a class specified in the S4 method signature.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`IdVector-class`], [`Vector-class`], [`Vector-setops`].
NULL

# FuzzySets ----

#' Methods for `FuzzySets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzySets-class`] class.
#' The `FuzzySets` class directly extends [`Sets-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`FuzzySets-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzySets-methods
#' @rdname FuzzySets-methods
#'
#' @param object,x An object of class inheriting from [`FuzzySets-class`].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`FuzzySets-class`], [`Sets-methods`].
NULL

# GOHits ----

#' Methods for `GOHits` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`GOHits-class`] class.
#' The `GOHits` class directly extends [`Hits-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` represents an object of class inheriting from [`GOHits-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name GOHits-methods
#' @rdname GOHits-methods
#'
#' @param object An object of class inheriting from [`GOHits-class`].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`GOHits-class`], [`Hits-class`].
#'
#' @examples
#' # Controlled vocabulary ----
#'
#' GOEvidenceCodes
#' GOOntologyCodes
NULL

# GOSets ----

#' Methods for `GOSets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`GOSets-class`] class.
#' The `GOSets` class directly extends [`Sets-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` represents an object of class inheriting from [`GOSets-class`],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name GOSets-methods
#' @rdname GOSets-methods
#'
#' @param object An object of class inheriting from [`GOSets-class`].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`GOSets-class`], [`Sets-methods`].
NULL
