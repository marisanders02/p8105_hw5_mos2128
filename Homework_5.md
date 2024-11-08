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

sim_results_df %>% 
  filter(p.value < 0.05) %>% 
  group_by(true_mean) %>% 
  summarize(count = n()) %>% 
  ggplot(aes(x = true_mean, y = count)) + geom_line()
```

![](Homework_5_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
sim_results_df %>% 
  group_by(true_mean) %>% 
  summarize(mean_estimate = mean(estimate, na.rm = TRUE)) %>% 
  ggplot(aes(x = true_mean, y = mean_estimate)) +  geom_line()
```

![](Homework_5_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

``` r
sim_results_df %>% 
  filter(p.value < 0.05) %>% 
  group_by(true_mean) %>% 
  summarize(mean_estimate = mean(estimate, na.rm = TRUE)) %>% 
  ggplot(aes(x = true_mean, y = mean_estimate))  + geom_line()
```

![](Homework_5_files/figure-gfm/unnamed-chunk-2-3.png)<!-- -->
