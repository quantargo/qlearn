
<!-- README.md is generated from README.Rmd. Please edit that file -->

# qlearn

<!-- badges: start -->

<!-- badges: end -->

The goal of qlearn is to create required assets for the [Quantargo
course platform](https://www.quantargo.com/courses).

## Installation

You can install the development version from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("quantargo/qlearn")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(qlearn)
library(rmarkdown)
course_file <- system.file("index.Rmd", package = "qlearn")
render(course_file, output_format = "qlearn", quiet = TRUE)
#> Loading required namespace: shiny
## basic example code
```
