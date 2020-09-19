
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pivot

*{pivot}* is an a package to pivot inside `dplyr::summarize()` calls.
It’s experimental, not thoroughly tested, etc…

Pivoting operations are grouped operations, although they are
traditionally treated differently, this package explores this idea .

The goal is to provide 2 functions, `widen()` and `lengthen()`, which do
most of what `pivot_wider()` and `pivot_longer()` do. Their key
characteristics are :

  - They’re used in a `dplyr::summarize()` call, which allows one to
    combine them with other aggregations
  - The new names are on the left side of `=` or `:=`, the right side
    describes the values, which is what we’re used to
  - Very few arguments are needed (often just one), so it’s very compact

The examples below take every example from `?tidyr::pivot_longer` and
`?tidyr::pivot_wider` and translates it into *{pivot}* syntax, right
below the comented original code.

It has only been tested on these examples, please report if you find
issues.

## Installation

You can install the released version of pivot from
[CRAN](https://CRAN.R-project.org) with:

``` r
remotes::install_github("pivot")
```

## widen

`widen()` calls have the form `widen(..., .fill = NULL)`.

glue strings describe the names of the columns to create, values are an
expression of the column to take values from.

``` r
widen(
  "{col_to_take_new_names_from_1}" = col_to_take_values_from_1,
  "foo_{toupper(col_to_take_new_names_from_2)}" = col_to_take_values_from_2,
  .fill =  0
)

widen(
  "{col_to_take_new_names_from}" = mean(col_to_take_values_from)
)
```

## lengthen

`lengthen()` calls have the form `lengthen(expr, drop_na = FALSE)`.

On the left side, names of new columns are given, ending by the column
containing the values.

On the right side a regular expression describes what columns should be
considered, and how to parse their names into the key columns.

`_val_` is a special value to handle multiple value columns.

``` r
lengthen(
  c("key", "value") = "^myprefix"
)

# use groups for several key columns
lengthen(
  c("key1", "key2", "value") = "^(.*)_(.*)$"
)

# use named capture for a different order
lengthen(
  c("key1", "key2", "value") = "^(?<key2>.*)_(.*)$"
)

# use '_val_' to get several value columns
lengthen(
  c("key", "_val_") = "^(?<_val_>.*)_(.*)$"
)
```

## Examples from `?pivot_wider`

original code is commented right before our translation

``` r
suppressPackageStartupMessages(library(tidyverse, warn.conflicts = FALSE))
library(pivot)

fish_encounters
#> # A tibble: 114 x 3
#>    fish  station  seen
#>    <fct> <fct>   <int>
#>  1 4842  Release     1
#>  2 4842  I80_1       1
#>  3 4842  Lisbon      1
#>  4 4842  Rstr        1
#>  5 4842  Base_TD     1
#>  6 4842  BCE         1
#>  7 4842  BCW         1
#>  8 4842  BCE2        1
#>  9 4842  BCW2        1
#> 10 4842  MAE         1
#> # ... with 104 more rows

# fish_encounters %>%
#   pivot_wider(names_from = station, values_from = seen)
fish_encounters %>%
  group_by(fish) %>%
  summarize(
    widen("{station}"= seen), 
    .groups = "drop")
#> # A tibble: 19 x 12
#>    fish  Release I80_1 Lisbon  Rstr Base_TD   BCE   BCW  BCE2  BCW2   MAE   MAW
#>    <fct>   <int> <int>  <int> <int>   <int> <int> <int> <int> <int> <int> <int>
#>  1 4842        1     1      1     1       1     1     1     1     1     1     1
#>  2 4843        1     1      1     1       1     1     1     1     1     1     1
#>  3 4844        1     1      1     1       1     1     1     1     1     1     1
#>  4 4845        1     1      1     1       1    NA    NA    NA    NA    NA    NA
#>  5 4847        1     1      1    NA      NA    NA    NA    NA    NA    NA    NA
#>  6 4848        1     1      1     1      NA    NA    NA    NA    NA    NA    NA
#>  7 4849        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
#>  8 4850        1     1     NA     1       1     1     1    NA    NA    NA    NA
#>  9 4851        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
#> 10 4854        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
#> 11 4855        1     1      1     1       1    NA    NA    NA    NA    NA    NA
#> 12 4857        1     1      1     1       1     1     1     1     1    NA    NA
#> 13 4858        1     1      1     1       1     1     1     1     1     1     1
#> 14 4859        1     1      1     1       1    NA    NA    NA    NA    NA    NA
#> 15 4861        1     1      1     1       1     1     1     1     1     1     1
#> 16 4862        1     1      1     1       1     1     1     1     1    NA    NA
#> 17 4863        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
#> 18 4864        1     1     NA    NA      NA    NA    NA    NA    NA    NA    NA
#> 19 4865        1     1      1    NA      NA    NA    NA    NA    NA    NA    NA

# Fill in missing values
# fish_encounters %>%
#   pivot_wider(names_from = station, values_from = seen, values_fill = 0)
fish_encounters %>%
  group_by(fish) %>%
  summarize(
    widen("{station}" = seen, .fill = 0), 
    .groups = "drop")
#> # A tibble: 19 x 12
#>    fish  Release I80_1 Lisbon  Rstr Base_TD   BCE   BCW  BCE2  BCW2   MAE   MAW
#>    <fct>   <int> <int>  <dbl> <dbl>   <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 4842        1     1      1     1       1     1     1     1     1     1     1
#>  2 4843        1     1      1     1       1     1     1     1     1     1     1
#>  3 4844        1     1      1     1       1     1     1     1     1     1     1
#>  4 4845        1     1      1     1       1     0     0     0     0     0     0
#>  5 4847        1     1      1     0       0     0     0     0     0     0     0
#>  6 4848        1     1      1     1       0     0     0     0     0     0     0
#>  7 4849        1     1      0     0       0     0     0     0     0     0     0
#>  8 4850        1     1      0     1       1     1     1     0     0     0     0
#>  9 4851        1     1      0     0       0     0     0     0     0     0     0
#> 10 4854        1     1      0     0       0     0     0     0     0     0     0
#> 11 4855        1     1      1     1       1     0     0     0     0     0     0
#> 12 4857        1     1      1     1       1     1     1     1     1     0     0
#> 13 4858        1     1      1     1       1     1     1     1     1     1     1
#> 14 4859        1     1      1     1       1     0     0     0     0     0     0
#> 15 4861        1     1      1     1       1     1     1     1     1     1     1
#> 16 4862        1     1      1     1       1     1     1     1     1     0     0
#> 17 4863        1     1      0     0       0     0     0     0     0     0     0
#> 18 4864        1     1      0     0       0     0     0     0     0     0     0
#> 19 4865        1     1      1     0       0     0     0     0     0     0     0

# Generate column names from multiple variables
us_rent_income
#> # A tibble: 104 x 5
#>    GEOID NAME       variable estimate   moe
#>    <chr> <chr>      <chr>       <dbl> <dbl>
#>  1 01    Alabama    income      24476   136
#>  2 01    Alabama    rent          747     3
#>  3 02    Alaska     income      32940   508
#>  4 02    Alaska     rent         1200    13
#>  5 04    Arizona    income      27517   148
#>  6 04    Arizona    rent          972     4
#>  7 05    Arkansas   income      23789   165
#>  8 05    Arkansas   rent          709     5
#>  9 06    California income      29454   109
#> 10 06    California rent         1358     3
#> # ... with 94 more rows

# us_rent_income %>%
#   pivot_wider(names_from = variable, values_from = c(estimate, moe))
us_rent_income %>%
  group_by(GEOID, NAME) %>%
  summarize(widen("estimate_{variable}" = estimate,
                  "moe_{variable}" = moe), 
            .groups ="drop")
#> # A tibble: 52 x 6
#>    GEOID NAME                 estimate_income estimate_rent moe_income moe_rent
#>    <chr> <chr>                          <dbl>         <dbl>      <dbl>    <dbl>
#>  1 01    Alabama                        24476           747        136        3
#>  2 02    Alaska                         32940          1200        508       13
#>  3 04    Arizona                        27517           972        148        4
#>  4 05    Arkansas                       23789           709        165        5
#>  5 06    California                     29454          1358        109        3
#>  6 08    Colorado                       32401          1125        109        5
#>  7 09    Connecticut                    35326          1123        195        5
#>  8 10    Delaware                       31560          1076        247       10
#>  9 11    District of Columbia           43198          1424        681       17
#> 10 12    Florida                        25952          1077         70        3
#> # ... with 42 more rows


# When there are multiple `names_from` or `values_from`, you can use
# use `names_sep` or `names_glue` to control the output variable names
# us_rent_income %>%
#   pivot_wider(
#     names_from = variable,
#     names_sep = ".",
#     values_from = c(estimate, moe)
#   )
us_rent_income %>%
  group_by(GEOID, NAME) %>%
  summarize(widen("estimate.{variable}" = estimate,
                  "moe.{variable}" = moe), 
            .groups ="drop")
#> # A tibble: 52 x 6
#>    GEOID NAME                 estimate.income estimate.rent moe.income moe.rent
#>    <chr> <chr>                          <dbl>         <dbl>      <dbl>    <dbl>
#>  1 01    Alabama                        24476           747        136        3
#>  2 02    Alaska                         32940          1200        508       13
#>  3 04    Arizona                        27517           972        148        4
#>  4 05    Arkansas                       23789           709        165        5
#>  5 06    California                     29454          1358        109        3
#>  6 08    Colorado                       32401          1125        109        5
#>  7 09    Connecticut                    35326          1123        195        5
#>  8 10    Delaware                       31560          1076        247       10
#>  9 11    District of Columbia           43198          1424        681       17
#> 10 12    Florida                        25952          1077         70        3
#> # ... with 42 more rows

# us_rent_income %>%
#   pivot_wider(
#     names_from = variable,
#     names_glue = "{variable}_{.value}",
#     values_from = c(estimate, moe)
#   )
us_rent_income %>%
  group_by(GEOID, NAME) %>%
  summarize(widen("{variable}_estimate" = estimate,
                  "{variable}_moe" = moe), 
            .groups ="drop")
#> # A tibble: 52 x 6
#>    GEOID NAME                 income_estimate rent_estimate income_moe rent_moe
#>    <chr> <chr>                          <dbl>         <dbl>      <dbl>    <dbl>
#>  1 01    Alabama                        24476           747        136        3
#>  2 02    Alaska                         32940          1200        508       13
#>  3 04    Arizona                        27517           972        148        4
#>  4 05    Arkansas                       23789           709        165        5
#>  5 06    California                     29454          1358        109        3
#>  6 08    Colorado                       32401          1125        109        5
#>  7 09    Connecticut                    35326          1123        195        5
#>  8 10    Delaware                       31560          1076        247       10
#>  9 11    District of Columbia           43198          1424        681       17
#> 10 12    Florida                        25952          1077         70        3
#> # ... with 42 more rows

# Can perform aggregation with values_fn
warpbreaks <- as_tibble(warpbreaks[c("wool", "tension", "breaks")])
warpbreaks
#> # A tibble: 54 x 3
#>    wool  tension breaks
#>    <fct> <fct>    <dbl>
#>  1 A     L           26
#>  2 A     L           30
#>  3 A     L           54
#>  4 A     L           25
#>  5 A     L           70
#>  6 A     L           52
#>  7 A     L           51
#>  8 A     L           26
#>  9 A     L           67
#> 10 A     M           18
#> # ... with 44 more rows

# warpbreaks %>%
#   pivot_wider(
#     names_from = wool,
#     values_from = breaks,
#     values_fn = mean
#   )
warpbreaks %>%
  group_by(tension) %>%
  summarize(
    widen("{wool}" = mean(breaks)),
    .groups = "drop"
  )
#> # A tibble: 3 x 3
#>   tension     A     B
#>   <fct>   <dbl> <dbl>
#> 1 L        44.6  28.2
#> 2 M        24    28.8
#> 3 H        24.6  18.8
```

## Examples from `?pivot_longer`

``` r
# Simplest case where column names are character data
relig_income
#> # A tibble: 18 x 11
#>    religion `<$10k` `$10-20k` `$20-30k` `$30-40k` `$40-50k` `$50-75k` `$75-100k`
#>    <chr>      <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>      <dbl>
#>  1 Agnostic      27        34        60        81        76       137        122
#>  2 Atheist       12        27        37        52        35        70         73
#>  3 Buddhist      27        21        30        34        33        58         62
#>  4 Catholic     418       617       732       670       638      1116        949
#>  5 Don’t k~      15        14        15        11        10        35         21
#>  6 Evangel~     575       869      1064       982       881      1486        949
#>  7 Hindu          1         9         7         9        11        34         47
#>  8 Histori~     228       244       236       238       197       223        131
#>  9 Jehovah~      20        27        24        24        21        30         15
#> 10 Jewish        19        19        25        25        30        95         69
#> 11 Mainlin~     289       495       619       655       651      1107        939
#> 12 Mormon        29        40        48        51        56       112         85
#> 13 Muslim         6         7         9        10         9        23         16
#> 14 Orthodox      13        17        23        32        32        47         38
#> 15 Other C~       9         7        11        13        13        14         18
#> 16 Other F~      20        33        40        46        49        63         46
#> 17 Other W~       5         2         3         4         2         7          3
#> 18 Unaffil~     217       299       374       365       341       528        407
#> # ... with 3 more variables: `$100-150k` <dbl>, `>150k` <dbl>, `Don't
#> #   know/refused` <dbl>

# relig_income %>%
#   pivot_longer(-religion, names_to = "income", values_to = "count")
relig_income %>%
  group_by(religion) %>%
  summarize(
    lengthen(c("income", "count") := ".*"),
    .groups = "drop")
#> # A tibble: 180 x 3
#>    religion income             count
#>    <chr>    <chr>              <dbl>
#>  1 Agnostic <$10k                 27
#>  2 Agnostic $10-20k               34
#>  3 Agnostic $20-30k               60
#>  4 Agnostic $30-40k               81
#>  5 Agnostic $40-50k               76
#>  6 Agnostic $50-75k              137
#>  7 Agnostic $75-100k             122
#>  8 Agnostic $100-150k            109
#>  9 Agnostic >150k                 84
#> 10 Agnostic Don't know/refused    96
#> # ... with 170 more rows

# Slightly more complex case where columns have common prefix,
# and missing missings are structural so should be dropped.
billboard
#> # A tibble: 317 x 79
#>    artist track date.entered   wk1   wk2   wk3   wk4   wk5   wk6   wk7   wk8
#>    <chr>  <chr> <date>       <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl> <dbl>
#>  1 2 Pac  Baby~ 2000-02-26      87    82    72    77    87    94    99    NA
#>  2 2Ge+h~ The ~ 2000-09-02      91    87    92    NA    NA    NA    NA    NA
#>  3 3 Doo~ Kryp~ 2000-04-08      81    70    68    67    66    57    54    53
#>  4 3 Doo~ Loser 2000-10-21      76    76    72    69    67    65    55    59
#>  5 504 B~ Wobb~ 2000-04-15      57    34    25    17    17    31    36    49
#>  6 98^0   Give~ 2000-08-19      51    39    34    26    26    19     2     2
#>  7 A*Tee~ Danc~ 2000-07-08      97    97    96    95   100    NA    NA    NA
#>  8 Aaliy~ I Do~ 2000-01-29      84    62    51    41    38    35    35    38
#>  9 Aaliy~ Try ~ 2000-03-18      59    53    38    28    21    18    16    14
#> 10 Adams~ Open~ 2000-08-26      76    76    74    69    68    67    61    58
#> # ... with 307 more rows, and 68 more variables: wk9 <dbl>, wk10 <dbl>,
#> #   wk11 <dbl>, wk12 <dbl>, wk13 <dbl>, wk14 <dbl>, wk15 <dbl>, wk16 <dbl>,
#> #   wk17 <dbl>, wk18 <dbl>, wk19 <dbl>, wk20 <dbl>, wk21 <dbl>, wk22 <dbl>,
#> #   wk23 <dbl>, wk24 <dbl>, wk25 <dbl>, wk26 <dbl>, wk27 <dbl>, wk28 <dbl>,
#> #   wk29 <dbl>, wk30 <dbl>, wk31 <dbl>, wk32 <dbl>, wk33 <dbl>, wk34 <dbl>,
#> #   wk35 <dbl>, wk36 <dbl>, wk37 <dbl>, wk38 <dbl>, wk39 <dbl>, wk40 <dbl>,
#> #   wk41 <dbl>, wk42 <dbl>, wk43 <dbl>, wk44 <dbl>, wk45 <dbl>, wk46 <dbl>,
#> #   wk47 <dbl>, wk48 <dbl>, wk49 <dbl>, wk50 <dbl>, wk51 <dbl>, wk52 <dbl>,
#> #   wk53 <dbl>, wk54 <dbl>, wk55 <dbl>, wk56 <dbl>, wk57 <dbl>, wk58 <dbl>,
#> #   wk59 <dbl>, wk60 <dbl>, wk61 <dbl>, wk62 <dbl>, wk63 <dbl>, wk64 <dbl>,
#> #   wk65 <dbl>, wk66 <lgl>, wk67 <lgl>, wk68 <lgl>, wk69 <lgl>, wk70 <lgl>,
#> #   wk71 <lgl>, wk72 <lgl>, wk73 <lgl>, wk74 <lgl>, wk75 <lgl>, wk76 <lgl>

# billboard %>%
#  pivot_longer(
#    cols = starts_with("wk"),
#    names_to = "week",
#    names_prefix = "wk",
#    values_to = "rank",
#    values_drop_na = TRUE
#  )
billboard %>%
  group_by(artist, track, date.entered) %>%
  summarize(
    lengthen(c("week", "rank") := "^wk(.*?)$", drop_na = TRUE),
    .groups = "drop")
#> # A tibble: 5,307 x 5
#>    artist  track                   date.entered week   rank
#>    <chr>   <chr>                   <date>       <chr> <dbl>
#>  1 2 Pac   Baby Don't Cry (Keep... 2000-02-26   1        87
#>  2 2 Pac   Baby Don't Cry (Keep... 2000-02-26   2        82
#>  3 2 Pac   Baby Don't Cry (Keep... 2000-02-26   3        72
#>  4 2 Pac   Baby Don't Cry (Keep... 2000-02-26   4        77
#>  5 2 Pac   Baby Don't Cry (Keep... 2000-02-26   5        87
#>  6 2 Pac   Baby Don't Cry (Keep... 2000-02-26   6        94
#>  7 2 Pac   Baby Don't Cry (Keep... 2000-02-26   7        99
#>  8 2Ge+her The Hardest Part Of ... 2000-09-02   1        91
#>  9 2Ge+her The Hardest Part Of ... 2000-09-02   2        87
#> 10 2Ge+her The Hardest Part Of ... 2000-09-02   3        92
#> # ... with 5,297 more rows

# Multiple variables stored in column names
# who %>% pivot_longer(
#   cols = new_sp_m014:newrel_f65,
#   names_to = c("diagnosis", "gender", "age"),
#   names_pattern = "new_?(.*)_(.)(.*)",
#   values_to = "count"
# )

who %>%
  group_by(country, iso2, iso3, year) %>%
  summarize(lengthen(
    c("diagnosis", "gender", "age", "count") := "^new_?(.*)_(.)(.*)"
  )) 
#> `summarise()` regrouping output by 'country', 'iso2', 'iso3', 'year' (override with `.groups` argument)
#> # A tibble: 405,440 x 8
#> # Groups:   country, iso2, iso3, year [7,240]
#>    country     iso2  iso3   year diagnosis gender age   count
#>    <chr>       <chr> <chr> <int> <chr>     <chr>  <chr> <int>
#>  1 Afghanistan AF    AFG    1980 sp        m      014      NA
#>  2 Afghanistan AF    AFG    1980 sp        m      1524     NA
#>  3 Afghanistan AF    AFG    1980 sp        m      2534     NA
#>  4 Afghanistan AF    AFG    1980 sp        m      3544     NA
#>  5 Afghanistan AF    AFG    1980 sp        m      4554     NA
#>  6 Afghanistan AF    AFG    1980 sp        m      5564     NA
#>  7 Afghanistan AF    AFG    1980 sp        m      65       NA
#>  8 Afghanistan AF    AFG    1980 sp        f      014      NA
#>  9 Afghanistan AF    AFG    1980 sp        f      1524     NA
#> 10 Afghanistan AF    AFG    1980 sp        f      2534     NA
#> # ... with 405,430 more rows

# Multiple observations per row
anscombe
#>    x1 x2 x3 x4    y1   y2    y3    y4
#> 1  10 10 10  8  8.04 9.14  7.46  6.58
#> 2   8  8  8  8  6.95 8.14  6.77  5.76
#> 3  13 13 13  8  7.58 8.74 12.74  7.71
#> 4   9  9  9  8  8.81 8.77  7.11  8.84
#> 5  11 11 11  8  8.33 9.26  7.81  8.47
#> 6  14 14 14  8  9.96 8.10  8.84  7.04
#> 7   6  6  6  8  7.24 6.13  6.08  5.25
#> 8   4  4  4 19  4.26 3.10  5.39 12.50
#> 9  12 12 12  8 10.84 9.13  8.15  5.56
#> 10  7  7  7  8  4.82 7.26  6.42  7.91
#> 11  5  5  5  8  5.68 4.74  5.73  6.89
# anscombe %>%
#  pivot_longer(everything(),
#    names_to = c(".value", "set"),
#    names_pattern = "(.)(.)"
#  )

anscombe %>%
  as_tibble() %>%
  summarize(lengthen(
    c("set", "_val_") := "(?<_val_>.)(.)"
  ))
#> # A tibble: 44 x 3
#>    set       x     y
#>    <chr> <dbl> <dbl>
#>  1 1        10  8.04
#>  2 2        10  9.14
#>  3 3        10  7.46
#>  4 4         8  6.58
#>  5 1         8  6.95
#>  6 2         8  8.14
#>  7 3         8  6.77
#>  8 4         8  5.76
#>  9 1        13  7.58
#> 10 2        13  8.74
#> # ... with 34 more rows
```
