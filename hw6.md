hw6
================
Brennan Baker
November 26, 2018

-   [Problem 1](#problem-1)
    -   [Load and tidy data](#load-and-tidy-data)
    -   [Baltimore model](#baltimore-model)
    -   [Models for all cities](#models-for-all-cities)
-   [Problem 2](#problem-2)
    -   [Load and tidy data](#load-and-tidy-data-1)
    -   [Propose model](#propose-model)
    -   [Plot residuals and predictions](#plot-residuals-and-predictions)
    -   [Compare proposed model to Jeff's models](#compare-proposed-model-to-jeffs-models)

Problem 1
=========

``` r
library(tidyverse)
```

    ## -- Attaching packages ------------------------------------------------------------ tidyverse 1.2.1 --

    ## v ggplot2 2.2.1     v purrr   0.2.5
    ## v tibble  1.4.2     v dplyr   0.7.6
    ## v tidyr   0.8.1     v stringr 1.3.1
    ## v readr   1.1.1     v forcats 0.3.0

    ## -- Conflicts --------------------------------------------------------------- tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(forcats)
library(modelr)
library(mgcv)
```

    ## Loading required package: nlme

    ## 
    ## Attaching package: 'nlme'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     collapse

    ## This is mgcv 1.8-25. For overview type 'help("mgcv-package")'.

### Load and tidy data

``` r
homicides_df = read_csv(file = "./data/data-homicides-master/homicide-data.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_character(),
    ##   victim_sex = col_character(),
    ##   city = col_character(),
    ##   state = col_character(),
    ##   lat = col_double(),
    ##   lon = col_double(),
    ##   disposition = col_character()
    ## )

``` r
homicides_df = homicides_df %>% 
  mutate(city_state = str_c(city, ",\ ", state)) %>% 
  filter(!city_state %in% c("Dallas, TX", "Phoenix, AZ", "Kansas City, MO", "Tulsa, AL")) %>% 
  mutate(victim_age = as.numeric(victim_age)) %>% 
  filter(!is.na(victim_age)) %>% 
  filter(victim_race != "unknown") %>% 
  mutate(victim_race = ifelse(victim_race == "White", "white", "non-white")) %>% 
  mutate(victim_race = as.factor(victim_race)) %>% 
  mutate(victim_race = relevel(victim_race, ref = "white")) %>% 
  mutate(resolution = factor(ifelse(
    disposition %in% c("Closed without arrest", "Open/No arrest"), "unresolved", "resolved")))
```

    ## Warning in evalq(as.numeric(victim_age), <environment>): NAs introduced by
    ## coercion

After importing the data, I created a city\_state variable. I excluded cities that have errors or do not report race. I converted age to numeric and removed NA age rows. I filtered out unknown race and recoded race as white or non-white, with white as the reference group. I removed unknown race because if the race is unknown, we cannot know if race is white or non-white. I recoded the disposition variavle as solved or unsolved.

### Baltimore model

``` r
baltimore_fit = homicides_df %>%
  filter(city_state == "Baltimore, MD") %>% 
  glm(resolution ~ victim_age + victim_race + victim_sex, data = ., family = binomial())
```

The above code saves a logistic regression to baltimore\_glm. The model uses victim age, race, and sex to predict whether the case is resolved or unresolved.

``` r
baltimore_fit %>% broom::tidy(conf.int = TRUE) %>% 
  mutate(OR = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>% 
  select(term, OR, conf.low, conf.high) %>% 
  filter(term == "victim_racenon-white") %>% 
  knitr::kable(digits = 3)
```

| term                  |    OR|  conf.low|  conf.high|
|:----------------------|-----:|---------:|----------:|
| victim\_racenon-white |  2.27|     1.614|      3.203|

The above table shows the estimate and confidence interval of the odds ratio for resolved vs unresolved homicides predicted by race, controling for age and sex.

### Models for all cities

``` r
homicides_models = homicides_df %>% 
  group_by(city_state) %>% 
  nest() %>% # now we have a list col called data containing all the data for each city state
  mutate(model = map(data, ~broom::tidy(glm(resolution ~ victim_age + victim_race + victim_sex, data = ., family = binomial()), conf.int = TRUE))) %>%
  select(-data) %>% 
  unnest() %>%
  mutate(OR = exp(estimate),
         conf.low = exp(conf.low),
         conf.high = exp(conf.high)) %>% 
  select(city_state, term, OR, conf.low, conf.high) %>%
  filter(term == "victim_racenon-white")

homicides_models %>% knitr::kable(digits = 3)
```

| city\_state        | term                  |     OR|  conf.low|  conf.high|
|:-------------------|:----------------------|------:|---------:|----------:|
| Albuquerque, NM    | victim\_racenon-white |  1.349|     0.823|      2.225|
| Atlanta, GA        | victim\_racenon-white |  1.328|     0.770|      2.356|
| Baltimore, MD      | victim\_racenon-white |  2.270|     1.614|      3.203|
| Baton Rouge, LA    | victim\_racenon-white |  1.498|     0.712|      3.285|
| Birmingham, AL     | victim\_racenon-white |  0.962|     0.570|      1.635|
| Boston, MA         | victim\_racenon-white |  8.730|     3.868|     23.525|
| Buffalo, NY        | victim\_racenon-white |  2.565|     1.409|      4.761|
| Charlotte, NC      | victim\_racenon-white |  1.794|     1.052|      3.194|
| Chicago, IL        | victim\_racenon-white |  1.779|     1.362|      2.316|
| Cincinnati, OH     | victim\_racenon-white |  3.141|     1.847|      5.558|
| Columbus, OH       | victim\_racenon-white |  1.170|     0.867|      1.577|
| Denver, CO         | victim\_racenon-white |  1.661|     0.992|      2.796|
| Detroit, MI        | victim\_racenon-white |  1.536|     1.150|      2.052|
| Durham, NC         | victim\_racenon-white |  0.997|     0.408|      2.562|
| Fort Worth, TX     | victim\_racenon-white |  1.194|     0.791|      1.809|
| Fresno, CA         | victim\_racenon-white |  2.233|     1.181|      4.498|
| Houston, TX        | victim\_racenon-white |  1.146|     0.918|      1.432|
| Indianapolis, IN   | victim\_racenon-white |  1.982|     1.503|      2.626|
| Jacksonville, FL   | victim\_racenon-white |  1.519|     1.160|      1.993|
| Las Vegas, NV      | victim\_racenon-white |  1.324|     1.029|      1.708|
| Long Beach, CA     | victim\_racenon-white |  1.260|     0.623|      2.638|
| Los Angeles, CA    | victim\_racenon-white |  1.502|     1.092|      2.078|
| Louisville, KY     | victim\_racenon-white |  2.552|     1.695|      3.888|
| Memphis, TN        | victim\_racenon-white |  1.278|     0.862|      1.926|
| Miami, FL          | victim\_racenon-white |  1.735|     1.135|      2.651|
| Milwaukee, wI      | victim\_racenon-white |  1.581|     1.019|      2.511|
| Minneapolis, MN    | victim\_racenon-white |  1.549|     0.831|      2.933|
| Nashville, TN      | victim\_racenon-white |  1.113|     0.810|      1.534|
| New Orleans, LA    | victim\_racenon-white |  2.146|     1.357|      3.400|
| New York, NY       | victim\_racenon-white |  1.882|     1.012|      3.695|
| Oakland, CA        | victim\_racenon-white |  4.695|     2.391|     10.109|
| Oklahoma City, OK  | victim\_racenon-white |  1.468|     1.031|      2.096|
| Omaha, NE          | victim\_racenon-white |  5.920|     3.356|     11.024|
| Philadelphia, PA   | victim\_racenon-white |  1.553|     1.176|      2.064|
| Pittsburgh, PA     | victim\_racenon-white |  3.552|     2.061|      6.359|
| Richmond, VA       | victim\_racenon-white |  2.235|     0.869|      6.933|
| San Antonio, TX    | victim\_racenon-white |  1.451|     0.975|      2.181|
| Sacramento, CA     | victim\_racenon-white |  1.281|     0.742|      2.256|
| Savannah, GA       | victim\_racenon-white |  1.677|     0.795|      3.640|
| San Bernardino, CA | victim\_racenon-white |  1.136|     0.500|      2.540|
| San Diego, CA      | victim\_racenon-white |  2.069|     1.285|      3.397|
| San Francisco, CA  | victim\_racenon-white |  2.182|     1.391|      3.472|
| St. Louis, MO      | victim\_racenon-white |  1.733|     1.222|      2.472|
| Stockton, CA       | victim\_racenon-white |  2.662|     1.403|      5.172|
| Tampa, FL          | victim\_racenon-white |  0.863|     0.436|      1.710|
| Tulsa, OK          | victim\_racenon-white |  1.660|     1.142|      2.432|
| Washington, DC     | victim\_racenon-white |  1.960|     1.011|      4.005|

The above code uses a map function to run the model that we used with baltimore on each city\_state.

``` r
homicides_models %>% 
  mutate(city_state = forcats::fct_reorder(city_state, OR)) %>%
  ggplot(aes(x = city_state, y = OR)) + 
      geom_point() +
      geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
      labs(
        x = "City, State",
        y = "Odds Ratio",
        title = "Odds homicide remains unsolved if non-white compared to white") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](hw6_files/figure-markdown_github/unnamed-chunk-1-1.png)

The above plot shows that Boston is the US city with the largest discrepancy in homicide resolutions between white and non-white victims. In Boston, non-white victim homicides are 8.7 times as likely to remain unresolved compared to white victim homicides.

Problem 2
=========

### Load and tidy data

``` r
birthweight_df = read_csv(file = "./data/birthweight.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   .default = col_integer(),
    ##   gaweeks = col_double(),
    ##   ppbmi = col_double(),
    ##   smoken = col_double()
    ## )

    ## See spec(...) for full column specifications.

``` r
birthweight_df = birthweight_df %>% 
   mutate(
      babysex = as.factor(babysex),
      frace = as.factor(frace),
      malform = as.factor(malform),
      mrace = as.factor(mrace))
```

The code above loads the data and converts sex, mother's race, father's race, and birth malformation into factor variables.

``` r
birthweight_df %>% 
  is.na() %>% summary()
```

    ##   babysex          bhead          blength           bwt         
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    delwt          fincome          frace          gaweeks       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##   malform         menarche        mheight          momage       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    mrace           parity         pnumlbw         pnumsga       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342     
    ##    ppbmi            ppwt           smoken          wtgain       
    ##  Mode :logical   Mode :logical   Mode :logical   Mode :logical  
    ##  FALSE:4342      FALSE:4342      FALSE:4342      FALSE:4342

From the code above we see that there is no missing data

### Propose model

``` r
lin_mod1 = lm(bwt ~ delwt + mrace, data = birthweight_df)
lin_mod2 = lm(bwt ~ delwt + frace, data = birthweight_df)
lin_mod3 = lm(bwt ~ delwt + mrace + frace, data = birthweight_df)
lin_mod4 = lm(bwt ~ delwt + mrace*frace, data = birthweight_df)

broom::tidy(lin_mod1)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  2287.      48.1      47.5   0.      
    ## 2 delwt           6.70     0.322    20.8   6.94e-92
    ## 3 mrace2       -316.      14.7     -21.5   6.64e-98
    ## 4 mrace3         18.5     72.1       0.256 7.98e- 1
    ## 5 mrace4       -148.      31.7      -4.66  3.28e- 6

``` r
broom::tidy(lin_mod2)
```

    ## # A tibble: 6 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  2279.      47.9     47.6    0.      
    ## 2 delwt           6.76     0.321   21.1    5.92e-94
    ## 3 frace2       -317.      14.7    -21.5    6.35e-98
    ## 4 frace3          3.50    69.7      0.0502 9.60e- 1
    ## 5 frace4       -150.      31.4     -4.79   1.74e- 6
    ## 6 frace8        -88.1    125.      -0.704  4.81e- 1

``` r
broom::tidy(lin_mod3)
```

    ## # A tibble: 9 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  2284.      48.1      47.4   0.      
    ## 2 delwt           6.73     0.322    20.9   1.43e-92
    ## 3 mrace2       -158.      78.6      -2.01  4.46e- 2
    ## 4 mrace3         52.6    123.        0.429 6.68e- 1
    ## 5 mrace4        -69.7     76.8      -0.907 3.64e- 1
    ## 6 frace2       -161.      78.7      -2.05  4.04e- 2
    ## 7 frace3        -34.0    118.       -0.287 7.74e- 1
    ## 8 frace4        -83.5     76.2      -1.10  2.73e- 1
    ## 9 frace8        -58.9    126.       -0.466 6.42e- 1

``` r
broom::tidy(lin_mod4)
```

    ## # A tibble: 20 x 5
    ##    term          estimate std.error statistic  p.value
    ##    <chr>            <dbl>     <dbl>     <dbl>    <dbl>
    ##  1 (Intercept)    2282.      48.2     47.4    0.      
    ##  2 delwt             6.75     0.322   21.0    6.50e-93
    ##  3 mrace2         -273.     177.      -1.55   1.21e- 1
    ##  4 mrace3         -333.     233.      -1.43   1.53e- 1
    ##  5 mrace4         -103.     125.      -0.822  4.11e- 1
    ##  6 frace2         -133.     117.      -1.14   2.54e- 1
    ##  7 frace3         -173.     165.      -1.05   2.96e- 1
    ##  8 frace4         -159.     121.      -1.31   1.90e- 1
    ##  9 frace8         -167.     148.      -1.13   2.60e- 1
    ## 10 mrace2:frace2    86.1    212.       0.407  6.84e- 1
    ## 11 mrace3:frace2   145.     534.       0.272  7.86e- 1
    ## 12 mrace4:frace2    16.7    289.       0.0576 9.54e- 1
    ## 13 mrace2:frace3   280.     525.       0.533  5.94e- 1
    ## 14 mrace3:frace3   561.     296.       1.90   5.80e- 2
    ## 15 mrace4:frace3  -116.     510.      -0.227  8.20e- 1
    ## 16 mrace2:frace4   171.     270.       0.632  5.27e- 1
    ## 17 mrace3:frace4  1030.     535.       1.92   5.43e- 2
    ## 18 mrace4:frace4   110.     176.       0.624  5.33e- 1
    ## 19 mrace2:frace8   634.     354.       1.79   7.32e- 2
    ## 20 mrace3:frace8   319.     542.       0.589  5.56e- 1

I want to see how race of the parents affects birthweight. The above code creates 4 models, one with mother's race, one with father's race, one with both factors, and one with both factors and their interaction.

``` r
cv_df = 
  crossv_mc(birthweight_df, 100)
```

The above code uses modelr to do 100 training/testing splits on the birthweight\_df.

``` r
cv_df = 
  cv_df %>% 
  mutate(lin_mod1 = map(train, ~lm(bwt ~ mrace, data = .x)),
         lin_mod2 = map(train, ~lm(bwt ~ frace, data = .x)),
         lin_mod3 = map(train, ~lm(bwt ~ mrace + frace, data = .x)),
         lin_mod4 = map(train, ~lm(bwt ~ mrace*frace, data = .x))) %>% 
  mutate(rmse_lin1 = map2_dbl(lin_mod1, test, ~rmse(model = .x, data = .y)),
         rmse_lin2 = map2_dbl(lin_mod2, test, ~rmse(model = .x, data = .y)),
         rmse_lin3 = map2_dbl(lin_mod3, test, ~rmse(model = .x, data = .y)),
         rmse_lin4 = map2_dbl(lin_mod4, test, ~rmse(model = .x, data = .y)))
```

The above code fits the 4 proposed models and gets RMSEs

``` r
cv_df %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](hw6_files/figure-markdown_github/proposed%20model%20plot-1.png)

From the above plots, we see that the full model with mother's race, father's race, and the interaction was the best model because on average, it produced the lowest RMSEs when run with 100 training datasets.

### Plot residuals and predictions

``` r
resid = modelr::add_residuals(birthweight_df, lin_mod4)
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

``` r
resid_and_pred = modelr::add_predictions(resid, lin_mod4)
```

    ## Warning in predict.lm(model, data): prediction from a rank-deficient fit
    ## may be misleading

``` r
resid_and_pred %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point() +
  labs(
    title = "Model",
    x = "Predictions",
    y = "Residuals"
    ) 
```

![](hw6_files/figure-markdown_github/pred%20and%20resid%20plot-1.png) show a plot of model residuals against fitted values – use add\_predictions and add\_residuals in making this plot.

### Compare proposed model to Jeff's models

``` r
cv_df2 = 
  crossv_mc(birthweight_df, 100)

cv_df2 = 
  cv_df2 %>% 
  mutate(lin_mod5 = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         lin_mod6 = map(train, ~lm(bwt ~ blength*bhead*babysex, data = .x)),
         proposed_mod = map(train, ~lm(bwt ~ mrace*frace, data = .x))) %>% 
  mutate(rmse_mod5 = map2_dbl(lin_mod5, test, ~rmse(model = .x, data = .y)),
         rmse_mod6 = map2_dbl(lin_mod6, test, ~rmse(model = .x, data = .y)),
         rmse_proposed = map2_dbl(proposed_mod, test, ~rmse(model = .x, data = .y)))

cv_df2 %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](hw6_files/figure-markdown_github/compare%20to%20jeffs%20models-1.png)

From the plot above we see that the best model has head circumference, length, sex, and all interactions (model 6).
