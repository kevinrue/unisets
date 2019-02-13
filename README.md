[![Travis build status](https://travis-ci.org/kevinrue/unisets.svg?branch=master)](https://travis-ci.org/kevinrue/unisets)
[![Coverage status](https://codecov.io/gh/kevinrue/unisets/branch/master/graph/badge.svg)](https://codecov.io/github/kevinrue/unisets?branch=master)

# unisets

The goal of the [_unisets_](https://github.com/kevinrue/unisets) package is to provide a collection of S4 classes to store relationships between elements and sets, with a particular emphasis on gene sets.

## Installation

You can install the released version of unisets from [GitHub](https://github.com/kevinrue/unisets) with:

``` r
devtools::install_github("kevinrue/unisets")
```

To compile the vignette as well, please use the following command:

``` r
devtools::install_github("kevinrue/unisets", build_opts = c("--no-resave-data", "--no-manual"))
```

## Example

This is a basic example which shows you how to create a `BaseSets` object, to store simple associations between genes and sets:

``` r
library(unisets)
gene_lists <- list(
    geneset1 = c("A", "B"),
    geneset2 = c("B", "C", "D")
)
relations_table <- DataFrame(
    element = unlist(gene_lists),
    set     = rep(names(gene_lists), lengths(gene_lists)),
    extra1  = rep(c("ABC", "DEF"), c(3L, 2L)),
    extra2  = seq(0, 1, length.out = 5L)
)
gene_data <- IdVector(c("A", "B", "C", "D"))
elementMetadata(gene_data) <- DataFrame(
    stat1     = c( 1,   2,   3,   4 ),
    info1     = c("a", "b", "c", "d")
)
set_data <- IdVector(c("geneset1", "geneset2"))
elementMetadata(set_data) <- DataFrame(
    stat1     = c( 100,   200 ),
    info1     = c("abc", "def")
)
base_set <- BaseSets(relations_table, gene_data, set_data)
base_set
```

```
BaseSets with 5 relations between 4 elements and 2 sets
     element        set relationData elementData     setData
  <IdVector> <IdVector>  <DataFrame> <DataFrame> <DataFrame>
1          A   geneset1        ABC:0         1:a     100:abc
2          B   geneset1     ABC:0.25         2:b     100:abc
3          B   geneset2      ABC:0.5         2:b     200:def
4          C   geneset2     DEF:0.75         3:c     200:def
5          D   geneset2        DEF:1         4:d     200:def
```
