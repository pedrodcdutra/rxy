
# ryx

![](correlation%20image.jpeg)

The goal of ryx is to make calculating correlations in R easier. With ryx, the user is able to return the correlations of all the numeric variables in the user's data with a specific variable. The ryx function also has a print, summary, and plot function which help the user better visualize and understand the relationship between their variables.

## Installation

You can install the development version of ryx from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pedrodcdutra/rxy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ryx)
ryx(mtcars, y = "mpg", x = c("hp", "wt", "disp", "cyl", "am", "gear"))
```
