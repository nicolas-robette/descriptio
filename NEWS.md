# descriptio 1.4

## new functions

* `crosstab()` : Displays pretty 2, 3 or 4-way cross-tabulations, from possibly weighted data, and with the opportunity to color the cells of the table according to a local measure of association (phi coefficients, standardized residuals or PEM). 
* `cattab()` : Bivariate statistics between a categorical variable and a set of variables
* `contab()` : Bivariate statistics between a continuous variable and a set of variables
* `regtab()` : Univariate and multivariate regressions and their average marginal effects side-by-side
* `weighted.cramer()` : Cramer's V measure of association between two (possibly weighted) categorical variables
* `stdres.table()` : Standardized and adjusted residuals of a (possibly weighted) contingency table

## minor changes

* all functions concerned : changed default NA level from "NA" to "NAs"
* all functions concerned : when na.rm = FALSE, a level for NAs is added only if there are NAs
* all functions concerned : empty levels are not automatically removed

## bug fix

* `profiles()` : bug fix when stat = "cprop" and mar = TRUE 



# descriptio 1.3 [CRAN]

## new functions

* `assoc.xx()` : Bivariate association measures between pairs of variables
* `assoc.twocat.by()` : Groupwise version of `assoc.twocat()`
* `assoc.twocont.by()` : Groupwise version of `assoc.twocont()`
* `assoc.catcont.by()` : Groupwise version of `assoc.catcont()`
* `profiles()` : Profiles by level of a categorical variable



# descriptio 1.2 [CRAN]

## changes in existing functions

* `assoc.catcont()` : new items in the results (summary statistics, test-values)
* `assoc.twocat()` : new item in the results (p-values of adjusted standardized residuals)
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

