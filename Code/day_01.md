Day 1
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
input <- read_table(here::here("Inputs", "input_01.txt"), col_names = FALSE, skip_empty_rows=FALSE)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   X1 = col_double()
    ## )

``` r
input
```

    ## # A tibble: 2,264 × 1
    ##       X1
    ##    <dbl>
    ##  1  7532
    ##  2 37124
    ##  3    NA
    ##  4 37309
    ##  5    NA
    ##  6  7616
    ##  7  2128
    ##  8  2657
    ##  9  8061
    ## 10  8565
    ## # … with 2,254 more rows

``` r
input %>%
  mutate(
    NextElf=is.na(lag(X1)),
    Elf=cumsum(NextElf)
  ) %>%
  filter(!is.na(X1)) %>%
  group_by(Elf) %>%
  summarise(Calories=sum(X1), .groups="drop") %>%
  pull(Calories) %>%
  max()
```

    ## [1] 69795

``` r
input %>%
  mutate(
    NextElf=is.na(lag(X1)),
    Elf=cumsum(NextElf)
  ) %>%
  filter(!is.na(X1)) %>%
  group_by(Elf) %>%
  summarise(Calories=sum(X1), .groups="drop") %>%
  slice_max(Calories, n=3) %>%
  pull(Calories) %>%
  sum()
```

    ## [1] 208437
