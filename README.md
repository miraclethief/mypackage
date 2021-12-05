# mypackage
  <!-- badges: start -->
  [![R-CMD-check](https://github.com/miraclethief/hw4/workflows/R-CMD-check/badge.svg)](https://github.com/miraclethief/hw4/actions)
  <!-- badges: end -->

R package for multiple linear regression, F-test for Simple linear regression and vector matching

# Overview
The 'mypackage' package aims at stimulating statistical analysis in R. mlr(), Ftest() are included in the package, along with KMP(). mlr() fits a multiple linear model and output contains the estimated coefficients(betahat), standard error of betahat, Inference: t statistic for H0: beta=0, Inference: corresponding p-value for H0: beta=0. Ftest()  conducts Ftest for Simple Linear Regression and output contains Sum Sq`: Regression sum of squares, SSE/dfR, Inference: F statistic for H0: beta1=0, Inference: corresponding p-value for H0: beta1=0, reflects fit of model, Error sum of squares, SSE/dfE. KMP() realizes famous KMP algorithm to find times of appearance of x in y, especially for quite long vectors matching(including 'overlap' parts). More information on can be found in the package help pages, with complete examples included under Vignettes.

# Installation
To install the package from github, simply use the `install_github()` function from the **devtools** package.

```
install.packages("devtools")
devtools::install_github("miraclethief/mypackage")
```
#Authorship
Created by Ruoxuan Mao, mainly for bios625 course.
