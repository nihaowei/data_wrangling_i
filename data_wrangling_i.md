data\_wrangling\_i
================

``` r
library(tidyverse)
```

    ## ─ Attaching packages ───────────────── tidyverse 1.2.1 ─

    ## ✔ ggplot2 3.0.0     ✔ purrr   0.2.5
    ## ✔ tibble  1.4.2     ✔ dplyr   0.7.6
    ## ✔ tidyr   0.8.1     ✔ stringr 1.3.1
    ## ✔ readr   1.1.1     ✔ forcats 0.3.0

    ## ─ Conflicts ────────────────── tidyverse_conflicts() ─
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
options(tibble.print_min = 3)
litters_data = read_csv("./data/FAS_litters.csv", col_types = "ccddiiii")
litters_data = janitor::clean_names(litters_data)
pups_data = read_csv("./data/FAS_pups.csv",
  col_types = "ciiiii")
pups_data = janitor::clean_names(pups_data)
rename(litters_data, GROUP = group, my_dogs = litter_number)
```

    ## # A tibble: 49 x 8
    ##   GROUP my_dogs gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>        <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85           19.7        34.7          20               3
    ## 2 Con7  #1/2/9…       27          42            19               8
    ## 3 Con7  #5/5/3…       26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
select(litters_data, litter_number, group, everything())
```

    ## # A tibble: 49 x 8
    ##   litter_number group gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr>         <chr>      <dbl>       <dbl>       <int>           <int>
    ## 1 #85           Con7        19.7        34.7          20               3
    ## 2 #1/2/95/2     Con7        27          42            19               8
    ## 3 #5/5/3/83/3-3 Con7        26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
select(pups_data, litter_number, sex, pd_ears)
```

    ## # A tibble: 313 x 3
    ##   litter_number   sex pd_ears
    ##   <chr>         <int>   <int>
    ## 1 #85               1       4
    ## 2 #85               1       4
    ## 3 #1/2/95/2         1       5
    ## # ... with 310 more rows

``` r
filter(litters_data, pups_born_alive >= 20)
```

    ## # A tibble: 0 x 8
    ## # ... with 8 variables: group <chr>, litter_number <chr>,
    ## #   gd0_weight <dbl>, gd18_weight <dbl>, gd_of_birth <int>,
    ## #   pups_born_alive <int>, pups_dead_birth <int>, pups_survive <int>

``` r
filter(pups_data, sex == 2, pd_walk < 11)
```

    ## # A tibble: 127 x 6
    ##   litter_number   sex pd_ears pd_eyes pd_pivot pd_walk
    ##   <chr>         <int>   <int>   <int>    <int>   <int>
    ## 1 #1/2/95/2         2       4      13        7       9
    ## 2 #1/2/95/2         2       4      13        7      10
    ## 3 #1/2/95/2         2       5      13        8      10
    ## # ... with 124 more rows

``` r
# how to show wt_gain?
mutate(litters_data, wt_gain = gd18_weight - gd0_weight,
     group = tolower(group)
     )
```

    ## # A tibble: 49 x 9
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ## 1 con7  #85                 19.7        34.7          20               3
    ## 2 con7  #1/2/95/2           27          42            19               8
    ## 3 con7  #5/5/3/83/3-3       26          41.4          19               6
    ## # ... with 46 more rows, and 3 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>, wt_gain <dbl>

``` r
mutate(pups_data, pivot_minus7 = pd_pivot - 7)
```

    ## # A tibble: 313 x 7
    ##   litter_number   sex pd_ears pd_eyes pd_pivot pd_walk pivot_minus7
    ##   <chr>         <int>   <int>   <int>    <int>   <int>        <dbl>
    ## 1 #85               1       4      13        7      11            0
    ## 2 #85               1       4      13        7      12            0
    ## 3 #1/2/95/2         1       5      13        7       9            0
    ## # ... with 310 more rows

``` r
mutate(pups_data, pd_sum = pd_ears + pd_eyes + pd_pivot + pd_walk)
```

    ## # A tibble: 313 x 7
    ##   litter_number   sex pd_ears pd_eyes pd_pivot pd_walk pd_sum
    ##   <chr>         <int>   <int>   <int>    <int>   <int>  <int>
    ## 1 #85               1       4      13        7      11     35
    ## 2 #85               1       4      13        7      12     36
    ## 3 #1/2/95/2         1       5      13        7       9     34
    ## # ... with 310 more rows

``` r
# arrange?
head(arrange(litters_data, group, gd_of_birth), 10)
```

    ## # A tibble: 10 x 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>              <dbl>       <dbl>       <int>           <int>
    ##  1 Con7  #1/2/95/2           27          42            19               8
    ##  2 Con7  #5/5/3/83/3-3       26          41.4          19               6
    ##  3 Con7  #5/4/2/95/2         28.5        44.1          19               5
    ##  4 Con7  #85                 19.7        34.7          20               3
    ##  5 Con7  #4/2/95/3-3         NA          NA            20               6
    ##  6 Con7  #2/2/95/3-2         NA          NA            20               6
    ##  7 Con7  #1/5/3/83/3-…       NA          NA            20               9
    ##  8 Con8  #5/4/3/83/3         28          NA            19               9
    ##  9 Con8  #2/2/95/2           NA          NA            19               5
    ## 10 Con8  #3/83/3-3           NA          NA            20               9
    ## # ... with 2 more variables: pups_dead_birth <int>, pups_survive <int>

``` r
select(litters_data, group, litter_number, gd0_weight)
```

    ## # A tibble: 49 x 3
    ##   group litter_number gd0_weight
    ##   <chr> <chr>              <dbl>
    ## 1 Con7  #85                 19.7
    ## 2 Con7  #1/2/95/2           27  
    ## 3 Con7  #5/5/3/83/3-3       26  
    ## # ... with 46 more rows

``` r
select(litters_data, litter_number, gd0_weight, pups_survive)
```

    ## # A tibble: 49 x 3
    ##   litter_number gd0_weight pups_survive
    ##   <chr>              <dbl>        <int>
    ## 1 #85                 19.7            3
    ## 2 #1/2/95/2           27              7
    ## 3 #5/5/3/83/3-3       26              5
    ## # ... with 46 more rows

``` r
# difference? 
select(litters_data, gd_of_birth:pups_survive)
```

    ## # A tibble: 49 x 4
    ##   gd_of_birth pups_born_alive pups_dead_birth pups_survive
    ##         <int>           <int>           <int>        <int>
    ## 1          20               3               4            3
    ## 2          19               8               0            7
    ## 3          19               6               0            5
    ## # ... with 46 more rows

``` r
select(litters_data, group, gd_of_birth:pups_survive)
```

    ## # A tibble: 49 x 5
    ##   group gd_of_birth pups_born_alive pups_dead_birth pups_survive
    ##   <chr>       <int>           <int>           <int>        <int>
    ## 1 Con7           20               3               4            3
    ## 2 Con7           19               8               0            7
    ## 3 Con7           19               6               0            5
    ## # ... with 46 more rows

``` r
select(litters_data, -litter_number)
```

    ## # A tibble: 49 x 7
    ##   group gd0_weight gd18_weight gd_of_birth pups_born_alive pups_dead_birth
    ##   <chr>      <dbl>       <dbl>       <int>           <int>           <int>
    ## 1 Con7        19.7        34.7          20               3               4
    ## 2 Con7        27          42            19               8               0
    ## 3 Con7        26          41.4          19               6               0
    ## # ... with 46 more rows, and 1 more variable: pups_survive <int>

``` r
select(litters_data, group, litter_number, gest_day_0_weight = gd0_weight)
```

    ## # A tibble: 49 x 3
    ##   group litter_number gest_day_0_weight
    ##   <chr> <chr>                     <dbl>
    ## 1 Con7  #85                        19.7
    ## 2 Con7  #1/2/95/2                  27  
    ## 3 Con7  #5/5/3/83/3-3              26  
    ## # ... with 46 more rows

``` r
rename(litters_data, gest_0_weight = gd0_weight)
```

    ## # A tibble: 49 x 8
    ##   group litter_number gest_0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>                 <dbl>       <dbl>       <int>           <int>
    ## 1 Con7  #85                    19.7        34.7          20               3
    ## 2 Con7  #1/2/95/2              27          42            19               8
    ## 3 Con7  #5/5/3/83/3-3          26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>

``` r
select(litters_data, starts_with("gd"))
```

    ## # A tibble: 49 x 3
    ##   gd0_weight gd18_weight gd_of_birth
    ##        <dbl>       <dbl>       <int>
    ## 1       19.7        34.7          20
    ## 2       27          42            19
    ## 3       26          41.4          19
    ## # ... with 46 more rows

``` r
select(litters_data, litter_number, starts_with("pup"))
```

    ## # A tibble: 49 x 4
    ##   litter_number pups_born_alive pups_dead_birth pups_survive
    ##   <chr>                   <int>           <int>        <int>
    ## 1 #85                         3               4            3
    ## 2 #1/2/95/2                   8               0            7
    ## 3 #5/5/3/83/3-3               6               0            5
    ## # ... with 46 more rows

``` r
select(litters_data, litter_number, everything())
```

    ## # A tibble: 49 x 8
    ##   litter_number group gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr>         <chr>      <dbl>       <dbl>       <int>           <int>
    ## 1 #85           Con7        19.7        34.7          20               3
    ## 2 #1/2/95/2     Con7        27          42            19               8
    ## 3 #5/5/3/83/3-3 Con7        26          41.4          19               6
    ## # ... with 46 more rows, and 2 more variables: pups_dead_birth <int>,
    ## #   pups_survive <int>
