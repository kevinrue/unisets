# unisets 0.99.0

* Added a `NEWS.md` file to track changes to the package.
* Added class `IdVector`.
* Added accessors `ids` for class `IdVector`.
* Added methods `duplicated`, `unique`, and `union` for class `IdVector`.
* Added classes `EntrezIdVector` and `EnsemblIdVector`.
* Added class `Sets`.
* Added accessors `relations`, `elementInfo`, and `setInfo` for class `Sets`.
* Added methods `length`, `nElements`, `nSets`, `setLengths` and `elementLengths` for class `Sets`.
* Added `show` method for class `Sets` matching the `Hits` class
* Added method `subset` for class `Sets`.
* Added method `c` for class `Sets`.
* Added classes `FuzzyHits` and `FuzzySets`.
* Added accessor `membership` for class `FuzzySets`.
* Added classes `GOHits` and `GOSets`.
* Added accessors `evidence` and `ontology` for class `GOSets`.
* Added methods `import` and `export` for GMT file format.
* Added method `import` for `Go3AnnDbBimap` objects.
* Added method `as` to `matrix` and `list`.
* Added method `as` from `matrix` and `list`.
* Added method `as` from `Sets` to `FuzzySets`.
* Added introduction vignette.
* Added vignette describing integration with Bioconductor `org.*.db` packages.
