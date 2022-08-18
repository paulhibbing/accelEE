
# accelEE

<!-- badges: start -->
<!-- badges: end -->

`accelEE` is a package to simplify prediction of energy expenditure from
accelerometer data. The initial goal is to support predictions using the
following methods:

* Linear Hildebrand (see [2014](https://journals.lww.com/00005768-201409000-00017)
  and [2017](https://onlinelibrary.wiley.com/doi/full/10.1111/sms.12795?casa_token=AwyG_FHPWcAAAAAA%3ADIO8s4EFVNUhOpyhEpmw9A7ccEZqvxARYjqSc6yD7prUEpEEECzPTMVmEOJkHem8y7YnpXrAkCoaicnc) publications by Hildebrand
  et al.)
* Non-Linear Hildebrand (see [Ellingson et al. (2017)](https://iopscience.iop.org/article/10.1088/1361-6579/aa6d00/meta))
* Staudenmayer linear model and random forest (see [Staudenmayer et al. (2015)](https://journals.physiology.org/doi/full/10.1152/japplphysiol.00026.2015))
* Montoye neural networks (see [Montoye et al. (2017)](https://www.tandfonline.com/doi/abs/10.1080/1091367X.2017.1337638?journalCode=hmpe20))
* Hibbing two-regression models (see [Hibbing et al. (2018)](https://journals.lww.com/acsm-msse/Fulltext/2018/05000/Estimating_Energy_Expenditure_with_ActiGraph_GT9X.24.aspx)
and the [TwoRegression package](https://github.com/paulhibbing/TwoRegression))

## Installation

You can install the development version of accelEE from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("paulhibbing/accelEE")
```
