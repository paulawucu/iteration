writing_function
================
Paula Wu
11/4/2021

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──

    ## ✓ ggplot2 3.3.5     ✓ purrr   0.3.4
    ## ✓ tibble  3.1.5     ✓ dplyr   1.0.7
    ## ✓ tidyr   1.1.4     ✓ stringr 1.4.0
    ## ✓ readr   2.0.1     ✓ forcats 0.5.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

# Z Score

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec))/sd(x_vec)
```

    ##  [1]  0.03093240 -0.10938404 -0.40867891 -1.15820918  0.34074379  0.96622027
    ##  [7] -0.25461294 -0.04643519 -0.28489034 -0.65313488  0.80574209  0.15409050
    ## [13] -1.89947545  0.04420071 -1.59834690  1.60408988  1.23319276  1.25757132
    ## [19]  2.25289355 -1.43101231 -0.43176497 -0.63250917 -0.36040020 -0.11892938
    ## [25]  0.69810659

``` r
z_score = function(x){
  z = (x-mean(x))/sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1]  0.03093240 -0.10938404 -0.40867891 -1.15820918  0.34074379  0.96622027
    ##  [7] -0.25461294 -0.04643519 -0.28489034 -0.65313488  0.80574209  0.15409050
    ## [13] -1.89947545  0.04420071 -1.59834690  1.60408988  1.23319276  1.25757132
    ## [19]  2.25289355 -1.43101231 -0.43176497 -0.63250917 -0.36040020 -0.11892938
    ## [25]  0.69810659

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_score(x = y_vec)
```

    ##  [1] -0.934705130 -0.200275168  2.614618327  2.142755187 -0.022034585
    ##  [6]  1.293114978 -0.708484830 -0.002024567  0.271935569 -0.034291535
    ## [11] -0.502049829  0.890974212  0.399348352  0.454445618 -1.336909498
    ## [16] -0.071076486  0.776799112 -0.126170737  1.160854643 -0.566210667
    ## [21]  0.738433517 -1.544798974 -0.104018673 -1.572211068  1.388616503
    ## [26]  0.349472120 -0.235791480 -0.915800831 -0.677185999  0.657519786
    ## [31]  0.195395459 -0.741922081 -0.075862581  0.847159477 -1.119817394
    ## [36]  0.937438081 -0.336394652  0.151784173 -1.438239936 -2.004388413

How great is this?

``` r
z_score(3)
z_scores("my name is jeff")
z_score(mtcars)
```

Let’s try again

``` r
z_score = function(x){
  if(is.numeric(x)){
    stop("x needs to be numeric")
  }
  if(length(x)<3){
    stop("x should be at least of length 3")
  }
  z = (x-mean(x))/sd(x)
  return(z)
}
```

``` r
z_score(3)
```

    ## Error in z_score(3): x needs to be numeric

``` r
z_scores("my name is jeff")
```

    ## Error in z_scores("my name is jeff"): could not find function "z_scores"

``` r
z_score(mtcars)
```

    ## Warning in mean.default(x): argument is not numeric or logical: returning NA

    ## Error in is.data.frame(x): 'list' object cannot be coerced to type 'double'
