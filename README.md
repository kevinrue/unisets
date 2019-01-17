[![Travis build status](https://travis-ci.org/kevinrue/unisets.svg?branch=master)](https://travis-ci.org/kevinrue/unisets)
[![Coverage status](https://codecov.io/gh/kevinrue/unisets/branch/master/graph/badge.svg)](https://codecov.io/github/kevinrue/unisets?branch=master)

# unisets

The goal of unisets is to provide a collection of classes to store gene sets.

## Installation

You can install the released version of unisets from [GitHub](https://github.com/kevinrue/unisets) with:

``` r
devtools::install_github("kevinrue/unisets")
```

## Example

This is a basic example which shows you how to create a `BaseSets` object, to store simple associations between genes and sets:

``` r
library(unisets)
gene_lists <- list(
    geneset1 = c("A", "B"),
    geneset2 = c("B", "C", "D")
)
mapping_table <- DataFrame(
    element = unlist(gene_lists),
    set     = rep(names(gene_lists), lengths(gene_lists))
)
gene_data <- DataFrame(
    row.names = c("A", "B", "C", "D"),
    stat1     = c( 1,   2,   3,   4 ),
    info1     = c("a", "b", "c", "d")
)
set_data <- DataFrame(
    row.names = c("geneset1", "geneset2"),
    stat1     = c(      100,        200 ),
    info1     = c(     "abc",      "def")
)
base_set <- BaseSets(mapping_table, gene_data, set_data)
base_set
```

```
BaseSets with 5 mappings between 4 elements and 2 sets
      element         set elementData     setData
  <character> <character> <DataFrame> <DataFrame>
1           A    geneset1         1:a     100:abc
2           B    geneset1         2:b     100:abc
3           B    geneset2         2:b     200:def
4           C    geneset2         3:c     200:def
5           D    geneset2         4:d     200:def
```

More sophisticated classes are available to store additional information (e.g. `FuzzySets`).

``` r
membership <- round(runif(nrow(mapping_table)), 2)
fuzzy_set <- FuzzySets(mapping_table, gene_data, set_data, membership = membership)
fuzzy_set
```

```
FuzzySets with 5 mappings between 4 elements and 2 sets
      element         set membership elementData     setData
  <character> <character>  <numeric> <DataFrame> <DataFrame>
1           A    geneset1        0.7         1:a     100:abc
2           B    geneset1       0.99         2:b     100:abc
3           B    geneset2       0.77         2:b     200:def
4           C    geneset2       0.34         3:c     200:def
5           D    geneset2       0.51         4:d     200:def
```
