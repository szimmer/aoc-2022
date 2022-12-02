Day 2
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
input <- read_table(here::here("Inputs", "input_02.txt"), col_names = c("O", "Y"), skip_empty_rows=FALSE)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   O = col_character(),
    ##   Y = col_character()
    ## )

``` r
input
```

    ## # A tibble: 2,500 × 2
    ##    O     Y    
    ##    <chr> <chr>
    ##  1 C     X    
    ##  2 B     Y    
    ##  3 C     Z    
    ##  4 C     Z    
    ##  5 B     X    
    ##  6 C     Z    
    ##  7 C     Z    
    ##  8 C     Z    
    ##  9 B     X    
    ## 10 B     Y    
    ## # … with 2,490 more rows

A for rock, B for Paper, C for Scissors

Believe X for rock, Y for paper, and Z for Scissors

``` r
score_shape <- function(x){
  case_when(
    x %in% c("X", "A")~1,
    x %in% c("Y", "B")~2,
    x %in% c("Z", "C")~3
  )
}

name_shape <- function(x){
  case_when(
    x %in% c("X", "A")~"rock",
    x %in% c("Y", "B")~"paper",
    x %in% c("Z", "C")~"scissors"
  )
}

game_score <- function(me, you){
  case_when(
    me==you~3, #draw
    (me %% 3) == ((you + 1) %% 3)~6, #win
    TRUE ~ 0 #lose
  )
}

GameScores <- input %>%
  mutate(
    ShapeScoreO=score_shape(O),
    ShapeScoreY=score_shape(Y),
    ShapeO=name_shape(O),
    ShapeY=name_shape(Y),
    GameScore=game_score(ShapeScoreY, ShapeScoreO),
    TotScore=ShapeScoreY+GameScore
    
  ) 

GameScores
```

    ## # A tibble: 2,500 × 8
    ##    O     Y     ShapeScoreO ShapeScoreY ShapeO   ShapeY   GameScore TotScore
    ##    <chr> <chr>       <dbl>       <dbl> <chr>    <chr>        <dbl>    <dbl>
    ##  1 C     X               3           1 scissors rock             6        7
    ##  2 B     Y               2           2 paper    paper            3        5
    ##  3 C     Z               3           3 scissors scissors         3        6
    ##  4 C     Z               3           3 scissors scissors         3        6
    ##  5 B     X               2           1 paper    rock             0        1
    ##  6 C     Z               3           3 scissors scissors         3        6
    ##  7 C     Z               3           3 scissors scissors         3        6
    ##  8 C     Z               3           3 scissors scissors         3        6
    ##  9 B     X               2           1 paper    rock             0        1
    ## 10 B     Y               2           2 paper    paper            3        5
    ## # … with 2,490 more rows

``` r
GameScores %>% pull(TotScore) %>% sum()
```

    ## [1] 13221

``` r
GameScores2 <- input %>%
  mutate(
    ShapeScoreO=score_shape(O),
    ShapeScoreY=case_when(
      Y=="X"~ if_else(ShapeScoreO-1>0, ShapeScoreO-1, 3), #you need to lose
      Y=="Y"~ShapeScoreO, #draw
      Y=="Z"~ if_else(ShapeScoreO+1<=3, ShapeScoreO+1, 1), #you need to win
    ),
    GameScore=case_when(
      Y=="X"~ 0, #lose
      Y=="Y"~3, #draw
      Y=="Z"~ 6 #win
    ),
    TotScore=ShapeScoreY+GameScore
  )

GameScores2
```

    ## # A tibble: 2,500 × 6
    ##    O     Y     ShapeScoreO ShapeScoreY GameScore TotScore
    ##    <chr> <chr>       <dbl>       <dbl>     <dbl>    <dbl>
    ##  1 C     X               3           2         0        2
    ##  2 B     Y               2           2         3        5
    ##  3 C     Z               3           1         6        7
    ##  4 C     Z               3           1         6        7
    ##  5 B     X               2           1         0        1
    ##  6 C     Z               3           1         6        7
    ##  7 C     Z               3           1         6        7
    ##  8 C     Z               3           1         6        7
    ##  9 B     X               2           1         0        1
    ## 10 B     Y               2           2         3        5
    ## # … with 2,490 more rows

``` r
GameScores2 %>% pull(TotScore) %>% sum()
```

    ## [1] 13131
