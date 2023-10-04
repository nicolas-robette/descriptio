# descriptio 1.2

## changes in existing functions

* `assoc.catcont()` : new items in the results (summary statistics, test-values)
* `assoc.twocat()` : new item in the results (p-values  of adjusted standardized residuals)
* `ggassoc_marimekko()` : y axis labels are now horizontal
* `condesc()` and `catdesc()` : labels of the results have been renamed ; dec argument is replaced by digits and simplified ; permutation p-values can be provided for variables and categories



# descriptio 1.1 [CRAN]

## new functions

* `weighted.cor2()` : weighted correlations between the columns of a data frame
* `weighted.cov()` : weighted covariance
* `weighted.cov2()` : weighted covariances between the columns of a data frame

## minor changes

* `ggpattern` package moved from Imports to Suggests



# descriptio 1.0 [CRAN]

Many functions in `descriptio` are imported from `GDAtools` (1.8), with some changes and improvements, among which the main ones are :

- Every function handles weights
- Every function handles NA values
- A few function and argument name changes


