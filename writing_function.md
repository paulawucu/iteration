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

    ##  [1] -0.18951307 -0.04097289 -1.85679476 -0.66357459  0.33590867  0.38005500
    ##  [7] -0.37703270 -0.77107218 -0.58309971 -0.51570759 -0.14306956  0.76411789
    ## [13] -0.87226838  0.56317393  1.33830854  2.64043618  0.29606393 -0.52044332
    ## [19] -0.05585226 -0.82894557 -0.48211649  1.26502404  1.42677857 -1.67195700
    ## [25]  0.56255333

``` r
z_score = function(x){
  z = (x-mean(x))/sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1] -0.18951307 -0.04097289 -1.85679476 -0.66357459  0.33590867  0.38005500
    ##  [7] -0.37703270 -0.77107218 -0.58309971 -0.51570759 -0.14306956  0.76411789
    ## [13] -0.87226838  0.56317393  1.33830854  2.64043618  0.29606393 -0.52044332
    ## [19] -0.05585226 -0.82894557 -0.48211649  1.26502404  1.42677857 -1.67195700
    ## [25]  0.56255333

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_score(x = y_vec)
```

    ##  [1]  0.14508868 -0.41831490  0.21874726 -0.57322252 -0.08151104  0.83263725
    ##  [7] -0.50486267 -0.55016816 -0.14625336 -0.43152193 -0.60183710 -1.32237781
    ## [13]  0.11651605  0.68872166  1.99113039 -0.05497240  1.38039949  1.30488840
    ## [19]  2.04887718 -0.14633797 -0.98045683 -0.06411475  0.02664308  1.24066343
    ## [25] -1.56914994 -0.97873451 -0.82440241  0.33416203 -0.49569414 -1.15583398
    ## [31] -1.01514117  1.36794452  1.62144873 -1.30137631  0.24425641  1.22735547
    ## [37] -1.77172216 -0.15897139 -0.88221103  1.23970842

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
# error = TRUE, intend to show the errors
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

## Multiple returns

``` r
mean_sd = function(x){
  if(!is.numeric(x)){
    stop("x needs to be numeric")
  }
  if(length(x)<3){
    stop("x should be at least of length 3")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  output_df = 
    tibble(
    mean = mean_x,
    sd = sd_x
  )
  return(output_df)
}

mean_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.49  3.09

## Different sample sizes, means, sds

``` r
stm_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

stm_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.52  2.21

Let’s write a function that simulates data, computes the mean and sd

``` r
sim_mean_sd = function(n, mu, sigma){
  sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}
sim_mean_sd(30,4,4)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.03  5.21

``` r
sim_mean_sd = function(n = 30, mu = 4, sigma = 3){
  sim_data = 
  tibble(
    x = rnorm(n, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
}
sim_mean_sd (20)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.67  2.84
