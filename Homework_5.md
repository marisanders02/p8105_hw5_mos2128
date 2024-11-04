Homework 5
================
Mari Sanders

# Problem 1

``` r
bday_sim <- function(n) {
  bdays <- sample(1:365, size = n, replace = TRUE) 
  
  duplicate <- length(unique(bdays)) < n
  
  return(duplicate)
}

sim_res <- 
  expand_grid(
    n = 2:50, 
    iter = 1:10000
  ) %>% 
  mutate(res = map_lgl(n, bday_sim)) %>% 
  group_by(n) %>% 
  summarize(prob = mean(res))

sim_res %>% 
  ggplot(aes(x = n, y = prob)) + geom_line()
```

![](Homework_5_files/figure-gfm/unnamed-chunk-1-1.png)<!-- -->

# Problem 2

``` r
normal_dist <- function(n, true_mean, sd = 5) {
  sim_data <- rnorm(n = 30, mean = true_mean, sd = 5)
  t_test <- t.test(sim_data)
  results <- broom::tidy(t_test)
  
}

sim_results_df <- 
  expand_grid(
    sample_size = 30,
    true_mean = c(1,2,3,4,5,6),
    iter = 1:5000
  ) %>% 
  mutate(
    estimate_df = map(sample_size, \(x) normal_dist(x,true_mean, sd))) %>% 
      unnest(estimate_df) %>% select(estimate, p.value, true_mean) 

sim_results_df %>% group_by(true_mean) 
```

    ## # A tibble: 30,000 × 3
    ## # Groups:   true_mean [6]
    ##    estimate  p.value true_mean
    ##       <dbl>    <dbl>     <dbl>
    ##  1   3.70   0.000264         1
    ##  2   1.74   0.0702           1
    ##  3   1.62   0.0430           1
    ##  4   0.208  0.838            1
    ##  5   1.16   0.305            1
    ##  6   1.22   0.223            1
    ##  7   2.02   0.0612           1
    ##  8  -0.0774 0.930            1
    ##  9   1.19   0.249            1
    ## 10   2.79   0.00443          1
    ## # ℹ 29,990 more rows
