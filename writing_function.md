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

    ##  [1] -0.68139450 -0.39298154 -0.77958121  1.13675074  0.18788512  0.08826858
    ##  [7]  0.31699607  0.37637542 -1.31466382  0.15683373 -1.46719184  2.61981999
    ## [13] -0.73436063  0.48055487  0.86849883 -0.59386778 -0.43698453 -0.05286015
    ## [19] -0.99966037 -0.54507526  0.05807122  1.63283536  1.66210153 -0.36348766
    ## [25] -1.22288216

``` r
z_score = function(x){
  z = (x-mean(x))/sd(x)
  return(z)
}

z_score(x = x_vec)
```

    ##  [1] -0.68139450 -0.39298154 -0.77958121  1.13675074  0.18788512  0.08826858
    ##  [7]  0.31699607  0.37637542 -1.31466382  0.15683373 -1.46719184  2.61981999
    ## [13] -0.73436063  0.48055487  0.86849883 -0.59386778 -0.43698453 -0.05286015
    ## [19] -0.99966037 -0.54507526  0.05807122  1.63283536  1.66210153 -0.36348766
    ## [25] -1.22288216

``` r
y_vec = rnorm(40, mean = 12, sd = .3)
z_score(x = y_vec)
```

    ##  [1] -0.25677707 -1.62823773 -0.61883560  0.87619174 -0.36938301 -1.72708388
    ##  [7]  1.15943774  0.74083226 -0.17475842 -0.27167303 -0.92677064  0.92509373
    ## [13] -0.28344133  1.60212822 -0.69226715 -0.22217004 -0.46561904  0.86592290
    ## [19]  0.55650623 -1.13646875  1.99264468 -0.38851911  1.95726878  1.01974687
    ## [25] -0.79475558 -0.28936163 -0.62644621  1.03612649 -0.78838047  1.07012555
    ## [31]  0.29567276 -1.24388797 -2.17749637  0.42537874 -0.48485377 -0.45543117
    ## [37]  1.41756305  0.03739604 -0.52781579  0.57239800

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
    ## 1  4.76  3.57

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
    ## 1  1.44  3.03

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
    ## 1  4.30  3.91

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
    ## 1  3.72  3.60

## Napolean dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Okay, but there are many pages of reviews

Write a function that gets reviews based on page url

``` r
get_page_review = function(page_url){
  
  page_html = read_html(url)
  
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )
  return(reviews)
}

url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"
get_page_review(url)
```

    ## # A tibble: 10 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…

``` r
base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="
urls = str_c(base_url, 1:5)
bind_rows(
  get_page_review(urls[1]),
  get_page_review(urls[2]),
  get_page_review(urls[3]),
  get_page_review(urls[4]),
  get_page_review(urls[5])
)
```

    ## # A tibble: 50 × 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou… "\n  VOTE FOR …
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou… "\n  5 stars f…
    ##  3 Best quirky movie ever                                5.0 ou… "\n  You all k…
    ##  4 Classic Film                                          5.0 ou… "\n  Had to or…
    ##  5 hehehehe                                              5.0 ou… "\n  goodjobbo…
    ##  6 Painful                                               1.0 ou… "\n  I think I…
    ##  7 GRAND                                                 5.0 ou… "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou… "\n  So nostal…
    ##  9 Cult Classic                                          5.0 ou… "\n  Watched i…
    ## 10 Format was inaccurate                                 4.0 ou… "\n  There was…
    ## # … with 40 more rows
