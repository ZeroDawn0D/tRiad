
# tRiad

<!-- badges: start -->

[![R-CMD-check](https://github.com/ZeroDawn0D/triad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZeroDawn0D/triad/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## Overview

tRiad is a package to perform Delaunay Triangulations
([Sloan(1987)](https://www.newcastle.edu.au/__data/assets/pdf_file/0017/22508/13_A-fast-algorithm-for-constructing-Delaunay-triangulations-in-the-plane.pdf))
on a set of 2D points. It was developed as part of GSoC 2022 for the R
Project for Statistical Computing. It was developed with the help of my
mentors Di Cook and Harriet Mason.

## Installation

``` r
#Development version
#install.packages("devtools")
devtools::install_github("ZeroDawn0D/tRiad")
```

## Running

``` r
x <- c(0,10,-20,30,10,-10)
y <- c(10,-20,-20,20,0,0)
plot(x,y)
```

![](man/figures/README-unnamed-chunk-2-1.png)<!-- -->

``` r
library(triad)
triad.obj <- DelTri(x,y)
plot(triad.obj)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->

## Using Data Frames

``` r
data("hundred")
plot(hundred)
```

![](man/figures/README-unnamed-chunk-4-1.png)<!-- -->

``` r
triad.hundred <- DelTri(hundred)
plot(triad.hundred, type = 'n')
```

![](man/figures/README-unnamed-chunk-5-1.png)<!-- -->
