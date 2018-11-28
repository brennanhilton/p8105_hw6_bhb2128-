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

    ## v ggplot2 3.1.0     v purrr   0.2.5
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
homicides_df = read_csv(file = "./data/data-homicides-master/homicide-data.csv", na = c("", "NA", "Unknown"))
```

    ## Parsed with column specification:
    ## cols(
    ##   uid = col_character(),
    ##   reported_date = col_integer(),
    ##   victim_last = col_character(),
    ##   victim_first = col_character(),
    ##   victim_race = col_character(),
    ##   victim_age = col_integer(),
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

After importing the data, I created a city\_state variable. I excluded cities that have errors or do not report race. I converted age to numeric and removed NA age rows. I filtered out unknown race and recoded race as white or non-white, with white as the reference group. I removed unknown race because if the race is unknown, we cannot know if race is white or non-white. I recoded the disposition variavle as unresolved or resolved.

### Baltimore model

``` r
baltimore_fit = homicides_df %>%
  filter(city_state == "Baltimore, MD") %>% 
  glm(resolution ~ victim_age + victim_race + victim_sex, data = ., family = binomial())
```

The above code saves a logistic regression to baltimore\_glm. The model uses victim age, race, and sex to predict whether the case is unresolved.

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

The above table shows the estimate and confidence interval of the odds ratio for resolved vs unresolved homicides predicted by race, controling for age and sex. We can see that non-white victims are 2.27 times as likely as white victims to have their case unresolved.

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
| Albuquerque, NM    | victim\_racenon-white |  1.353|     0.820|      2.248|
| Atlanta, GA        | victim\_racenon-white |  1.328|     0.770|      2.356|
| Baltimore, MD      | victim\_racenon-white |  2.270|     1.614|      3.203|
| Baton Rouge, LA    | victim\_racenon-white |  1.498|     0.712|      3.285|
| Birmingham, AL     | victim\_racenon-white |  0.962|     0.570|      1.635|
| Boston, MA         | victim\_racenon-white |  7.895|     3.504|     21.193|
| Buffalo, NY        | victim\_racenon-white |  2.549|     1.400|      4.733|
| Charlotte, NC      | victim\_racenon-white |  1.794|     1.052|      3.194|
| Chicago, IL        | victim\_racenon-white |  1.779|     1.362|      2.316|
| Cincinnati, OH     | victim\_racenon-white |  3.141|     1.847|      5.558|
| Columbus, OH       | victim\_racenon-white |  1.162|     0.861|      1.567|
| Denver, CO         | victim\_racenon-white |  1.661|     0.992|      2.796|
| Detroit, MI        | victim\_racenon-white |  1.535|     1.150|      2.051|
| Durham, NC         | victim\_racenon-white |  0.997|     0.408|      2.562|
| Fort Worth, TX     | victim\_racenon-white |  1.194|     0.791|      1.809|
| Fresno, CA         | victim\_racenon-white |  2.248|     1.189|      4.528|
| Houston, TX        | victim\_racenon-white |  1.146|     0.918|      1.432|
| Indianapolis, IN   | victim\_racenon-white |  1.982|     1.503|      2.626|
| Jacksonville, FL   | victim\_racenon-white |  1.519|     1.160|      1.993|
| Las Vegas, NV      | victim\_racenon-white |  1.311|     1.019|      1.691|
| Long Beach, CA     | victim\_racenon-white |  1.260|     0.623|      2.638|
| Los Angeles, CA    | victim\_racenon-white |  1.502|     1.092|      2.078|
| Louisville, KY     | victim\_racenon-white |  2.552|     1.695|      3.888|
| Memphis, TN        | victim\_racenon-white |  1.285|     0.866|      1.937|
| Miami, FL          | victim\_racenon-white |  1.734|     1.129|      2.661|
| Milwaukee, wI      | victim\_racenon-white |  1.581|     1.019|      2.511|
| Minneapolis, MN    | victim\_racenon-white |  1.549|     0.831|      2.933|
| Nashville, TN      | victim\_racenon-white |  1.108|     0.807|      1.528|
| New Orleans, LA    | victim\_racenon-white |  2.142|     1.354|      3.393|
| New York, NY       | victim\_racenon-white |  1.880|     1.011|      3.694|
| Oakland, CA        | victim\_racenon-white |  4.695|     2.391|     10.109|
| Oklahoma City, OK  | victim\_racenon-white |  1.468|     1.031|      2.096|
| Omaha, NE          | victim\_racenon-white |  5.879|     3.334|     10.944|
| Philadelphia, PA   | victim\_racenon-white |  1.553|     1.176|      2.064|
| Pittsburgh, PA     | victim\_racenon-white |  3.552|     2.061|      6.359|
| Richmond, VA       | victim\_racenon-white |  2.235|     0.869|      6.933|
| San Antonio, TX    | victim\_racenon-white |  1.451|     0.975|      2.181|
| Sacramento, CA     | victim\_racenon-white |  1.281|     0.742|      2.256|
| Savannah, GA       | victim\_racenon-white |  1.653|     0.783|      3.586|
| San Bernardino, CA | victim\_racenon-white |  1.136|     0.500|      2.540|
| San Diego, CA      | victim\_racenon-white |  2.069|     1.285|      3.397|
| San Francisco, CA  | victim\_racenon-white |  2.182|     1.391|      3.472|
| St. Louis, MO      | victim\_racenon-white |  1.733|     1.222|      2.472|
| Stockton, CA       | victim\_racenon-white |  2.662|     1.403|      5.172|
| Tampa, FL          | victim\_racenon-white |  0.863|     0.436|      1.710|
| Tulsa, OK          | victim\_racenon-white |  1.679|     1.155|      2.461|
| Washington, DC     | victim\_racenon-white |  1.946|     1.003|      3.975|

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

![](hw6_files/figure-markdown_github/all%20cities%20plot-1.png)

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
lin_mod1 = lm(bwt ~ mheight, data = birthweight_df)
lin_mod2 = lm(bwt ~ mheight + mrace, data = birthweight_df)
lin_mod3 = lm(bwt ~ mheight + mrace + momage, data = birthweight_df)
lin_mod4 = lm(bwt ~ mheight + mrace + momage + delwt, data = birthweight_df)

broom::tidy(lin_mod1)
```

    ## # A tibble: 2 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)    762.     183.        4.18 3.03e- 5
    ## 2 mheight         37.0      2.87     12.9  2.18e-37

``` r
broom::tidy(lin_mod2)
```

    ## # A tibble: 5 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)   1178.     181.        6.49 9.46e-11
    ## 2 mheight         32.7      2.84     11.5  2.43e-30
    ## 3 mrace2        -301.      15.2     -19.8  2.72e-83
    ## 4 mrace3         -77.9     74.3      -1.05 2.94e- 1
    ## 5 mrace4        -120.      33.5      -3.59 3.38e- 4

``` r
broom::tidy(lin_mod3)
```

    ## # A tibble: 6 x 5
    ##   term        estimate std.error statistic  p.value
    ##   <chr>          <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  1137.      184.        6.19 6.38e-10
    ## 2 mheight        32.3       2.85     11.4  1.58e-29
    ## 3 mrace2       -293.       16.2     -18.1  1.05e-70
    ## 4 mrace3        -89.6      74.7      -1.20 2.30e- 1
    ## 5 mrace4       -116.       33.7      -3.45 5.62e- 4
    ## 6 momage          2.94      2.03      1.44 1.49e- 1

``` r
broom::tidy(lin_mod4)
```

    ## # A tibble: 7 x 5
    ##   term         estimate std.error statistic  p.value
    ##   <chr>           <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)  1610.      179.        8.97  4.34e-19
    ## 2 mheight        11.6       3.00      3.87  1.12e- 4
    ## 3 mrace2       -308.       15.6     -19.7   1.13e-82
    ## 4 mrace3         20.4      72.5       0.281 7.79e- 1
    ## 5 mrace4       -119.       32.5      -3.67  2.50e- 4
    ## 6 momage          0.847     1.97      0.430 6.67e- 1
    ## 7 delwt           6.14      0.351    17.5   3.28e-66

I decided to examine models with maternal characteristics. I simply added a new maternal variable into each successive model.

``` r
cv_df = 
  crossv_mc(birthweight_df, 100)
```

The above code uses modelr to do 100 training/testing splits on the birthweight\_df.

``` r
cv_df = 
  cv_df %>% 
  mutate(lin_mod1 = map(train, ~lm(bwt ~ mheight, data = .x)),
         lin_mod2 = map(train, ~lm(bwt ~ mheight + mrace, data = .x)),
         lin_mod3 = map(train, ~lm(bwt ~ mheight + mrace + momage, data = .x)),
         lin_mod4 = map(train, ~lm(bwt ~ mheight + mrace + momage + delwt, data = .x))) %>% 
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

From the above plots, we see that the full model with mother's height, race, age, and weight was the best model because on average, it produced the lowest RMSEs when run with 100 training datasets.

### Plot residuals and predictions

``` r
resid = modelr::add_residuals(birthweight_df, lin_mod4)
resid_and_pred = modelr::add_predictions(resid, lin_mod4)

resid_and_pred %>% 
  ggplot(aes(x = pred, y = resid)) +
  geom_point() +
  labs(
    title = "Model",
    x = "Predictions",
    y = "Residuals"
    ) 
```

![](hw6_files/figure-markdown_github/pred%20and%20resid%20plot-1.png) Above is a plot of the proposed model residuals against fitted values. Points are clustered nicely around 0 and there is no odd pattern, which is good! A linear model seems appropriate for these data.

### Compare proposed model to Jeff's models

``` r
cv_df2 = 
  crossv_mc(birthweight_df, 100)

cv_df2 = 
  cv_df2 %>% 
  mutate(lin_mod5 = map(train, ~lm(bwt ~ blength + gaweeks, data = .x)),
         lin_mod6 = map(train, ~lm(bwt ~ blength*bhead*babysex, data = .x)),
         proposed_mod = map(train, ~lm(bwt ~ mheight + mrace + momage + delwt, data = .x)),
         combined_mod = map(train, ~lm(bwt ~ blength*bhead*babysex + mheight + mrace + momage + delwt, data = .x))) %>% 
  mutate(rmse_mod5 = map2_dbl(lin_mod5, test, ~rmse(model = .x, data = .y)),
         rmse_mod6 = map2_dbl(lin_mod6, test, ~rmse(model = .x, data = .y)),
         rmse_proposed = map2_dbl(proposed_mod, test, ~rmse(model = .x, data = .y)),
         rmse_combined = map2_dbl(combined_mod, test, ~rmse(model = .x, data = .y)))

cv_df2 %>% 
  select(starts_with("rmse")) %>% 
  gather(key = model, value = rmse) %>% 
  mutate(model = str_replace(model, "rmse_", ""),
         model = fct_inorder(model)) %>% 
  ggplot(aes(x = model, y = rmse)) + geom_violin()
```

![](hw6_files/figure-markdown_github/compare%20to%20jeffs%20models-1.png)

From the plot above we see that Jeff's best model with head circumference, length, sex, and all interactions (model 6) was better than my model (proposed model). However, when I combined my proposed model with Jeff's best model, the RMSE was lower than Jeff's best model (combined model).
