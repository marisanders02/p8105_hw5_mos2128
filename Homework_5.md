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

# Problem 3

``` r
homicide_df <- read_csv("data/homicide-data.csv") %>% 
  unite(city_state, c(city, state), sep = ", ") 
```

    ## Rows: 52179 Columns: 12
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (9): uid, victim_last, victim_first, victim_race, victim_age, victim_sex...
    ## dbl (3): reported_date, lat, lon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

Describe the data.

``` r
homicide_df %>% 
  group_by(city_state, disposition) %>% 
  summarize(count = n())
```

    ## `summarise()` has grouped output by 'city_state'. You can override using the
    ## `.groups` argument.

    ## # A tibble: 146 × 3
    ## # Groups:   city_state [51]
    ##    city_state      disposition           count
    ##    <chr>           <chr>                 <int>
    ##  1 Albuquerque, NM Closed by arrest        232
    ##  2 Albuquerque, NM Closed without arrest    52
    ##  3 Albuquerque, NM Open/No arrest           94
    ##  4 Atlanta, GA     Closed by arrest        600
    ##  5 Atlanta, GA     Closed without arrest    58
    ##  6 Atlanta, GA     Open/No arrest          315
    ##  7 Baltimore, MD   Closed by arrest       1002
    ##  8 Baltimore, MD   Closed without arrest   152
    ##  9 Baltimore, MD   Open/No arrest         1673
    ## 10 Baton Rouge, LA Closed by arrest        228
    ## # ℹ 136 more rows

``` r
homicide_df %>% 
  filter(city_state == "Baltimore, MD") %>% group_by(disposition) %>% 
  summarize(count = n()) 
```

    ## # A tibble: 3 × 2
    ##   disposition           count
    ##   <chr>                 <int>
    ## 1 Closed by arrest       1002
    ## 2 Closed without arrest   152
    ## 3 Open/No arrest         1673
