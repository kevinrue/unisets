# unisets 0.99.0

* Added a `NEWS.md` file to track changes to the package.
* Added classes `BaseSets` and `FuzzySets`.
* Added getters and setters to slots.
* Added method `subset` for classes `BaseSets` and `FuzzySets`.
* Added method `show` for classes `BaseSets` and `FuzzySets`.
* Added `elementIds`, `setIds`, `length`, `nElements`, `nSets`, `setLengths`
    and `elementLengths` methods for classes `BaseSets` and `FuzzySets`.
* Added `as.list` and `as.matrix` export methods for classes derived from `BaseSets`.
* Added `as.BaseSets` and `as.FuzzySets` import methods for class `matrix`.
* Added `as.BaseSets` import methods for classes `matrix` and `Go3AnnDbBimap`.
* Added `as` method to coerce class `BaseSets` to `FuzzySets`.
* Added introduction vignette.
* Added vignette describing integration with Bioconductor `org.*.db` packages.
