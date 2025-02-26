
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LCutils

<!-- badges: start -->
<!-- badges: end -->

The goal of LCutils is to deliver simple tables inspired by SAS’s
bread-and-butter analytic procedures: PROC FREQ and PROC MEANS.

## Installation

You can install the development version of LCutils from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mostlyunoriginal/LCutils")
```

## Examples

``` r
library(LCutils)

freqy(mtcars)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
#>          n        pct     cumn     cumpct
#> 1       32      100.0       32      100.0
```

``` r

freqy(mtcars,cyl,gear)
#>   cyl gear        n        pct     cumn     cumpct
#> 1   4    3        1        3.1        1        3.1
#> 2   4    4        8       25.0        9       28.1
#> 3   4    5        2        6.2       11       34.4
#> 4   6    3        2        6.2       13       40.6
#> 5   6    4        4       12.5       17       53.1
#> 6   6    5        1        3.1       18       56.2
#> 7   8    3       12       37.5       30       93.8
#> 8   8    5        2        6.2       32      100.0
```

``` r

freqy(mtcars,cyl,gear,where=hp>100)
#>   cyl gear        n        pct     cumn     cumpct
#> 1   4    4        1        4.3        1        4.3
#> 2   4    5        1        4.3        2        8.7
#> 3   6    3        2        8.7        4       17.4
#> 4   6    4        4       17.4        8       34.8
#> 5   6    5        1        4.3        9       39.1
#> 6   8    3       12       52.2       21       91.3
#> 7   8    5        2        8.7       23      100.0
```

``` r

freqy(mtcars,cyl,digits=2)
#>   cyl        n        pct     cumn     cumpct
#> 1   4       11      34.38       11      34.38
#> 2   6        7      21.88       18      56.25
#> 3   8       14      43.75       32     100.00
```

``` r

mtcars %>%
  mutate(cyl=ifelse(runif(n())<.2,NA_integer_,cyl)) %>%
  freqy(cyl,missincl=F)
#>   cyl        n        pct     cumn     cumpct
#> 1   4       10       37.0       10       37.0
#> 2   6        6       22.2       16       59.3
#> 3   8       11       40.7       27      100.0
```

``` r

meanit(mtcars,mpg)
#>   variable        n        pct     cumn     cumpct     nomiss      nmiss
#> 1      mpg       32      100.0       32      100.0         32          0
#>          min       mean         sd        max
#> 1       10.4       20.1        6.0       33.9
```

``` r

meanit(mtcars,mpg,cyl)
#>   variable cyl        n        pct     cumn     cumpct     nomiss      nmiss
#> 1      mpg   4       11       34.4       11       34.4         11          0
#> 2      mpg   6        7       21.9       18       56.2          7          0
#> 3      mpg   8       14       43.8       32      100.0         14          0
#>          min       mean         sd        max
#> 1       21.4       26.7        4.5       33.9
#> 2       17.8       19.7        1.5       21.4
#> 3       10.4       15.1        2.6       19.2
```

``` r

meanit(mtcars,c(mpg,hp),cyl,gear)
#>    variable cyl gear        n        pct     cumn     cumpct     nomiss
#> 1        hp   4    3        1        3.1        1        3.1          1
#> 2        hp   4    4        8       25.0        9       28.1          8
#> 3        hp   4    5        2        6.2       11       34.4          2
#> 4        hp   6    3        2        6.2       13       40.6          2
#> 5        hp   6    4        4       12.5       17       53.1          4
#> 6        hp   6    5        1        3.1       18       56.2          1
#> 7        hp   8    3       12       37.5       30       93.8         12
#> 8        hp   8    5        2        6.2       32      100.0          2
#> 9       mpg   4    3        1        3.1        1        3.1          1
#> 10      mpg   4    4        8       25.0        9       28.1          8
#> 11      mpg   4    5        2        6.2       11       34.4          2
#> 12      mpg   6    3        2        6.2       13       40.6          2
#> 13      mpg   6    4        4       12.5       17       53.1          4
#> 14      mpg   6    5        1        3.1       18       56.2          1
#> 15      mpg   8    3       12       37.5       30       93.8         12
#> 16      mpg   8    5        2        6.2       32      100.0          2
#>         nmiss        min       mean         sd        max
#> 1           0       97.0       97.0         NA       97.0
#> 2           0       52.0       76.0       20.1      109.0
#> 3           0       91.0      102.0       15.6      113.0
#> 4           0      105.0      107.5        3.5      110.0
#> 5           0      110.0      116.5        7.5      123.0
#> 6           0      175.0      175.0         NA      175.0
#> 7           0      150.0      194.2       33.4      245.0
#> 8           0      264.0      299.5       50.2      335.0
#> 9           0       21.5       21.5         NA       21.5
#> 10          0       21.4       26.9        4.8       33.9
#> 11          0       26.0       28.2        3.1       30.4
#> 12          0       18.1       19.8        2.3       21.4
#> 13          0       17.8       19.8        1.6       21.0
#> 14          0       19.7       19.7         NA       19.7
#> 15          0       10.4       15.1        2.8       19.2
#> 16          0       15.0       15.4        0.6       15.8
```

``` r

meanit(mtcars,mpg,cyl,center="median")
#>   variable cyl        n        pct     cumn     cumpct     nomiss      nmiss
#> 1      mpg   4       11       34.4       11       34.4         11          0
#> 2      mpg   6        7       21.9       18       56.2          7          0
#> 3      mpg   8       14       43.8       32      100.0         14          0
#>          min        q25     median        q75        max
#> 1       21.4       22.8       26.0       30.4       33.9
#> 2       17.8       18.6       19.7       21.0       21.4
#> 3       10.4       14.4       15.2       16.2       19.2
```
