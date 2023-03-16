# **descriptio** <img src="man/figures/descriptio.png" height=140px width=120px alt="" align="right" />

## Descriptive Statistical Analysis

<!-- badges: start -->
[![R-CMD-check](https://github.com/nicolas-robette/descriptio/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/nicolas-robette/descriptio/actions/workflows/R-CMD-check.yaml)
  [![](https://www.r-pkg.org/badges/version/descriptio?color=blue)](https://cran.r-project.org/package=descriptio)
  [![](http://cranlogs.r-pkg.org/badges/last-month/descriptio?color=orange)](https://cran.r-project.org/package=descriptio)
<!-- badges: end -->

[`descriptio`](https://nicolas-robette.github.io/descriptio/) provides functions for the description of statistical associations between two variables :

* measures of local and global association between variables (phi, Cramer's V, point-biserial correlation, eta-squared, Goodman & Kruskal tau, PEM, etc.),
* graphical representations of the associations between two variables (using `ggplot2`),
* weighted statistics,
* permutation tests.


## Documentation

Please visit [https://nicolas-robette.github.io/descriptio/](https://nicolas-robette.github.io/descriptio/) for documentation


## Installation

Execute the following code within `R`:

``` r
if (!require(devtools)){
    install.packages('devtools')
    library(devtools)
}
install_github("nicolas-robette/descriptio")
```


## Citation

To cite `descriptio` in publications, use :

Robette N. (2023), *`descriptio` : Descriptive Analysis of Associations between two Variables in `R`*, version 1.1, https://nicolas-robette.github.io/descriptio/


## References

Agresti, A. (2007). *An Introduction to Categorical Data Analysis*, 2nd ed. New York: John Wiley & Sons.

Cibois P., 1993, « Le PEM, pourcentage de l'ecart maximum : un indice de liaison entre modalites d'un tableau de contingence », *Bulletin de Methodologie Sociologique*, 40, pp 43-63, [http://cibois.pagesperso-orange.fr/bms93.pdf]

Rakotomalala R., « Comprendre la taille d'effet (effect size) », [http://eric.univ-lyon2.fr/~ricco/cours/slides/effect_size.pdf]
