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
sets_list <- list(
    set1 = c("A", "B"),
    set2 = c("B", "C", "D")
)
relations_table <- DataFrame(
    element = unlist(sets_list),
    set     = rep(names(sets_list), lengths(sets_list)),
    extra1  = rep(c("ABC", "DEF"), c(3L, 2L)),
    extra2  = seq(0, 1, length.out = 5L)
)
element_data <- IdVector(c("A", "B", "C", "D"))
elementMetadata(element_data) <- DataFrame(
    stat1     = c( 1,   2,   3,   4 ),
    info1     = c("a", "b", "c", "d")
)
set_data <- IdVector(c("set1", "set2"))
elementMetadata(set_data) <- DataFrame(
    stat1     = c( 100,   200 ),
    info1     = c("abc", "def")
)
base_set <- BaseSets(relations_table, element_data, set_data)
base_set
```

```
BaseSets with 5 relations between 4 elements and 2 sets
       element        set |      extra1    extra2
    <IdVector> <IdVector> | <character> <numeric>
[1]          A       set1 |         ABC         0
[2]          B       set1 |         ABC      0.25
[3]          B       set2 |         ABC       0.5
[4]          C       set2 |         DEF      0.75
[5]          D       set2 |         DEF         1

@elementData
IdVector of length 4 with 4 unique identifiers
Ids: A, B, C, D
Metadata: stat1, info1 (2 columns)

@setData
IdVector of length 2 with 2 unique identifiers
Ids: set1, set2
Metadata: stat1, info1 (2 columns)
```
