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

This is a basic example which shows you how to create a `BaseSet` object, to store simple associations between genes and sets:

``` r
library(uniset)
gene_lists <- list(
    geneset1 = c("A", "B"),
    geneset2 = c("B", "C", "D")
)
gene_table <- DataFrame(
    element = unlist(gene_lists),
    set     = rep(names(gene_lists), lengths(gene_lists))
)
base_set <- BaseSet(gene_table)
base_set
```

```
BaseSet with 5 mappings between 4 elements and 2 sets
      element         set elementData     setData
  <character> <character> <DataFrame> <DataFrame>
1           A    geneset1                        
2           B    geneset1                        
3           B    geneset2                        
4           C    geneset2                        
5           D    geneset2   
```

More sophisticated classes are available to store additional information (e.g. `FuzzySet`).

``` r
membership <- runif(nrow(gene_table))
fuzzy_set <- FuzzySet(gene_table, membership = membership)
fuzzy_set
```

```
FuzzySet with 5 mappings between 4 elements and 2 sets
      element         set           membership elementData     setData
  <character> <character>            <numeric> <DataFrame> <DataFrame>
1           A    geneset1     0.95690098265186                        
2           B    geneset1    0.907051113899797                        
3           B    geneset2 6.23832456767559e-05                        
4           C    geneset2    0.878892858279869                        
5           D    geneset2    0.284040126018226   
```
