# tRiad

<!-- badges: start -->

[![R-CMD-check](https://github.com/ZeroDawn0D/triad/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/ZeroDawn0D/triad/actions/workflows/R-CMD-check.yaml)

<!-- badges: end -->

## overview

tRiad is a package to perform Constrained Delaunay Triangulations ([Cline and Renka(1990)](https://epubs.siam.org/doi/abs/10.1137/0727074)) on a set of 2D points

## installation

``` r
#Development version
#install.packages("devtools")
devtools::install_github("ZeroDawn0D/tRiad")
#> WARNING: Rtools is required to build R packages, but is not currently installed.
#> 
#> Please download and install Rtools 4.2 from https://cran.r-project.org/bin/windows/Rtools/ or https://www.r-project.org/nosvn/winutf8/ucrt3/.
#> Downloading GitHub repo ZeroDawn0D/tRiad@HEAD
#> WARNING: Rtools is required to build R packages, but is not currently installed.
#> 
#> Please download and install Rtools 4.2 from https://cran.r-project.org/bin/windows/Rtools/ or https://www.r-project.org/nosvn/winutf8/ucrt3/.
#> * checking for file 'C:\Users\Umang\AppData\Local\Temp\Rtmpgh6GxM\remotes215035975ba2\ZeroDawn0D-tRiad-3bb4065/DESCRIPTION' ... OK
#> * preparing 'triad':
#> * checking DESCRIPTION meta-information ... OK
#> * checking for LF line-endings in source and make files and shell scripts
#> * checking for empty or unneeded directories
#> * building 'triad_0.0.0.9000.tar.gz'
#> 
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
triad.obj <- del_tri(x,y)
plot(triad.obj)
```

![](man/figures/README-unnamed-chunk-3-1.png)<!-- -->
