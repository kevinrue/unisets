
# BaseSets ----

#' Methods for `BaseSets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`BaseSets`][BaseSets-class] class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`BaseSets`][BaseSets-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name BaseSets-methods
#' @rdname BaseSets-methods
#'
#' @param object,x An object of class inheriting from [`BaseSets`][BaseSets-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`BaseSets-class`].
NULL

# FuzzyHits ----

#' Methods for `FuzzyHits` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzyHits`][FuzzyHits-class] class.
#' The `FuzzyHits` class directly extends [`Hits`][`Hits-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` represents an object of class inheriting from [`FuzzyHits`][FuzzyHits-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzyHits-methods
#' @rdname FuzzyHits-methods
#'
#' @param object An object of class inheriting from [FuzzyHits][FuzzyHits-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`FuzzyHits-class`], [FuzzySets-class].
NULL

# IdVector ----

#' Methods for `IdVector` objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`IdVector`][IdVector-class] class.
#' The `IdVector` class directly extends [`Vector`][`Vector-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`IdVector`][IdVector-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name IdVector-methods
#' @rdname IdVector-methods
#'
#' @param object,x An object of class inheriting from [IdVector][IdVector-class].
#' @param value An object of a class specified in the S4 method signature.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`IdVector-class`], [`Vector`][Vector-class], [`Vector-setops`].
NULL

# FuzzySets ----

#' Methods for `FuzzySets` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`FuzzySets`][FuzzySets-class] class.
#' The `FuzzySets` class directly extends [`BaseSets`][`BaseSets-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`FuzzySets`][FuzzySets-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name FuzzySets-methods
#' @rdname FuzzySets-methods
#'
#' @param object,x An object of class inheriting from [`FuzzySets`][FuzzySets-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`FuzzySets-class`], [`BaseSets-methods`].
NULL

# GOHits ----

#' Methods for `GOHits` Objects
#'
#' This page documents the S4 generics and methods defined for objects inheriting of the [`GOHits`][GOHits-class] class.
#' The `GOHits` class directly extends [`Hits`][`Hits-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` represents an object of class inheriting from [`GOHits`][GOHits-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name GOHits-methods
#' @rdname GOHits-methods
#'
#' @param object An object of class inheriting from [GOHits][GOHits-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`GOHits-class`], [GOHits-class].
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
#' This page documents the S4 generics and methods defined for objects inheriting of the [`GOSets`][GOSets-class] class.
#' The `GOSets` class directly extends [`BaseSets`][`BaseSets-class`] and thus inherits of all methods defined for the parent class.
#' In the usage below, `object` and `x` represent an object of class inheriting from [`GOSets`][GOSets-class],
#' and `value` is an object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @name GOSets-methods
#' @rdname GOSets-methods
#'
#' @param object,x An object of class inheriting from [`GOSets`][GOSets-class].
#' @param value An object of a class specified in the S4 method signature or as outlined in 'Accessors'.
#'
#' @author Kevin Rue-Albrecht
#'
#' @seealso [`GOSets-class`], [`BaseSets-methods`].
NULL
