
# accelEE

<!-- badges: start -->
<!-- badges: end -->

`accelEE` is a package to simplify prediction of energy expenditure from
accelerometer data. The following methods are currently supported:

* Hibbing two-regression models (see [Hibbing et al. (2018)](https://pubmed.ncbi.nlm.nih.gov/29271847/)
and the [TwoRegression package](https://github.com/paulhibbing/TwoRegression))
* Linear Hildebrand (see [2014](https://pubmed.ncbi.nlm.nih.gov/24887173/)
  and [2017](https://pubmed.ncbi.nlm.nih.gov/27878845/) publications by Hildebrand
  et al.)
* Non-Linear Hildebrand (see [Ellingson et al. (2017)](https://pubmed.ncbi.nlm.nih.gov/28481750/))
* Montoye neural networks (see [Montoye et al. (2017)](https://www.tandfonline.com/doi/abs/10.1080/1091367X.2017.1337638?journalCode=hmpe20))
* Sojourn methods of [Lyden et al. (2014)](https://pubmed.ncbi.nlm.nih.gov/23860415/) and
[Ellingson et al. (2016)](https://pubmed.ncbi.nlm.nih.gov/27015380/)
* Staudenmayer linear model and random forest (see [Staudenmayer et al. (2015)](https://pubmed.ncbi.nlm.nih.gov/26112238/))

## Installation

You can install the development version of accelEE from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paulhibbing/accelEE")
```
