Challenge 1
================
Jhon

# REF

<https://www.youtube.com/watch?v=sSnbmbRmtSA>

# Library

``` r
librarian::shelf(tidyverse)
```

    ## 
    ##   The 'cran_repo' argument in shelf() was not set, so it will use
    ##   cran_repo = 'https://cran.r-project.org' by default.
    ## 
    ##   To avoid this message, set the 'cran_repo' argument to a CRAN
    ##   mirror URL (see https://cran.r-project.org/mirrors.html) or set
    ##   'quiet = TRUE'.

## Dirty data

``` r
dirty <- 
  here::here("db", "data_cleaning_challenge.csv") %>% 
  read_csv()
```

    ## Warning: Missing column names filled in: 'X10' [10], 'X11' [11]

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   `Row Type` = col_character(),
    ##   `Iter Number` = col_character(),
    ##   Power1 = col_character(),
    ##   Speed1 = col_character(),
    ##   Speed2 = col_character(),
    ##   Electricity = col_character(),
    ##   Effort = col_character(),
    ##   Weight = col_character(),
    ##   Torque = col_character(),
    ##   X10 = col_logical(),
    ##   X11 = col_logical()
    ## )

    ## Warning: 83 parsing failures.
    ##   row col           expected           actual                                                       file
    ## 59351 X11 1/0/T/F/TRUE/FALSE notes: test note '/home/jhon/Documents/R/yt/db/data_cleaning_challenge.csv'
    ## 60642 X11 1/0/T/F/TRUE/FALSE notes: 2BLOCK    '/home/jhon/Documents/R/yt/db/data_cleaning_challenge.csv'
    ## 60654 X11 1/0/T/F/TRUE/FALSE notes: 2BLOCK    '/home/jhon/Documents/R/yt/db/data_cleaning_challenge.csv'
    ## 60664 X11 1/0/T/F/TRUE/FALSE notes: 2BLOCK    '/home/jhon/Documents/R/yt/db/data_cleaning_challenge.csv'
    ## 60674 X11 1/0/T/F/TRUE/FALSE notes: 2BLOCK    '/home/jhon/Documents/R/yt/db/data_cleaning_challenge.csv'
    ## ..... ... .................. ................ ..........................................................
    ## See problems(...) for more details.

``` r
head(dirty, 20)
```

    ## # A tibble: 20 × 11
    ##    `Row Type`    `Iter Number` Power1    Speed1 Speed2 Electricity Effort Weight
    ##    <chr>         <chr>         <chr>     <chr>  <chr>  <chr>       <chr>  <chr> 
    ##  1 first name: … last name: H… date: en… <NA>   <NA>   <NA>        <NA>   <NA>  
    ##  2 <NA>          <NA>          <NA>      <NA>   <NA>   <NA>        <NA>   <NA>  
    ##  3 Row Type      Iter Number   Power1    Speed1 Speed2 Electricity Effort Weight
    ##  4 Iter          1             360       108    863    599         680    442   
    ##  5 Iter          2             684       508    613    241         249    758   
    ##  6 Iter          3             365       126    825    407         855    164   
    ##  7 Iter          4             764       594    304    718         278    674   
    ##  8 Iter          5             487       97     593    206         779    800   
    ##  9 Average       182           361       741    231    731         493    847   
    ## 10 Maximum       276           33        97     154    25          922    9     
    ## 11 Std.Dev.      523           1000      34     904    237         600    170   
    ## 12 Total         336           -         -      -      -           977    744   
    ## 13 <NA>          <NA>          <NA>      <NA>   <NA>   <NA>        <NA>   <NA>  
    ## 14 <NA>          <NA>          <NA>      <NA>   <NA>   <NA>        <NA>   <NA>  
    ## 15 first name: … last name: H… date: en… <NA>   <NA>   <NA>        <NA>   <NA>  
    ## 16 <NA>          <NA>          <NA>      <NA>   <NA>   <NA>        <NA>   <NA>  
    ## 17 Row Type      Iter Number   Power1    Speed1 Speed2 Electricity Effort Weight
    ## 18 Iter          1             702       494    311    492         456    370   
    ## 19 Iter          2             929       82     838    421         154    346   
    ## 20 Iter          3             763       402    344    951         139    295   
    ## # … with 3 more variables: Torque <chr>, X10 <lgl>, X11 <lgl>

# Clean data

``` r
clean <- 
  dirty %>% 
  janitor::clean_names() %>% 
  select(!c(x10, x11)) %>% 
  mutate(
    first_name  = case_when(str_detect(row_type, "name") ~ str_sub(row_type, 13, -1))
    , last_name  = case_when(str_detect(iter_number, "name") ~ str_sub(iter_number, 12, -1))
    , date  = case_when(str_detect(power1, "date") ~ str_sub(power1, 7, -1))
    , iter  = case_when(str_detect(row_type, "Total") ~ row_number()
  )) %>% 
  fill(10:12) %>% 
  fill(iter, .direction = "up") %>% 
  drop_na(speed1) %>% 
  with_groups(iter, ~mutate(., id = cur_group_id())) %>% 
  select(!iter) %>% 
  filter(!str_detect(row_type, "Type")) %>% 
  relocate(id, first_name, last_name, date) %>% 
  mutate(across(iter_number:torque, as.numeric)) %>% 
  rename_with(str_to_sentence)
```

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

    ## Warning in mask$eval_all_mutate(quo): NAs introduced by coercion

``` r
head(clean)
```

    ## # A tibble: 6 × 13
    ##      Id First_name Last_name Date      Row_type Iter_number Power1 Speed1 Speed2
    ##   <int> <chr>      <chr>     <chr>     <chr>          <dbl>  <dbl>  <dbl>  <dbl>
    ## 1     1 Person     Human     end of t… Iter               1    360    108    863
    ## 2     1 Person     Human     end of t… Iter               2    684    508    613
    ## 3     1 Person     Human     end of t… Iter               3    365    126    825
    ## 4     1 Person     Human     end of t… Iter               4    764    594    304
    ## 5     1 Person     Human     end of t… Iter               5    487     97    593
    ## 6     1 Person     Human     end of t… Average          182    361    741    231
    ## # … with 4 more variables: Electricity <dbl>, Effort <dbl>, Weight <dbl>,
    ## #   Torque <dbl>

# Clean data v2

No average, maximmum, std.dev, total

``` r
clean_v2 <- 
  clean %>% 
  filter(!str_detect(Row_type, "Avera|Maximimum|Std.|Total"))
```

``` r
head(clean_v2)
```

    ## # A tibble: 6 × 13
    ##      Id First_name Last_name Date      Row_type Iter_number Power1 Speed1 Speed2
    ##   <int> <chr>      <chr>     <chr>     <chr>          <dbl>  <dbl>  <dbl>  <dbl>
    ## 1     1 Person     Human     end of t… Iter               1    360    108    863
    ## 2     1 Person     Human     end of t… Iter               2    684    508    613
    ## 3     1 Person     Human     end of t… Iter               3    365    126    825
    ## 4     1 Person     Human     end of t… Iter               4    764    594    304
    ## 5     1 Person     Human     end of t… Iter               5    487     97    593
    ## 6     1 Person     Human     end of t… Maximum          276     33     97    154
    ## # … with 4 more variables: Electricity <dbl>, Effort <dbl>, Weight <dbl>,
    ## #   Torque <dbl>
