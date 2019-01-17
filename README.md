[![Travis build status](https://travis-ci.org/kevinrue/uniset.svg?branch=master)](https://travis-ci.org/kevinrue/uniset)
[![Coverage status](https://codecov.io/gh/kevinrue/uniset/branch/master/graph/badge.svg)](https://codecov.io/github/kevinrue/uniset?branch=master)

# uniset

The goal of uniset is to provide a collection of classes to store gene sets.

## Installation

You can install the released version of uniset from [GitHub](https://github.com/kevinrue/uniset) with:

``` r
devtools::install_github("kevinrue/uniset")
```

## Example

This is a basic example which shows you how to create a `BaseSets` object, to store simple associations between genes and sets:

``` r
library(uniset)
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
membership <- runif(nrow(mapping_table))
fuzzy_set <- FuzzySets(mapping_table, gene_data, set_data, membership = membership)
fuzzy_set
```

```
FuzzySets with 5 mappings between 4 elements and 2 sets
      element         set        membership elementData     setData
  <character> <character>         <numeric> <DataFrame> <DataFrame>
1           A    geneset1 0.570553105324507         1:a     100:abc
2           B    geneset1 0.539594167144969         2:b     100:abc
3           B    geneset2 0.342863601865247         2:b     200:def
4           C    geneset2 0.501708829076961         3:c     200:def
5           D    geneset2 0.776408678852022         4:d     200:def
```
