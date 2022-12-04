Day 3
================

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.5.0 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2

    ## Warning: package 'ggplot2' was built under R version 4.2.2

    ## Warning: package 'readr' was built under R version 4.2.2

    ## Warning: package 'purrr' was built under R version 4.2.2

    ## Warning: package 'stringr' was built under R version 4.2.2

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
input <- read_table(here::here("Inputs", "input_03.txt"), col_names = c("contents"), skip_empty_rows=FALSE)
```

    ## 
    ## ── Column specification ────────────────────────────────────────────────────────
    ## cols(
    ##   contents = col_character()
    ## )

``` r
input
```

    ## # A tibble: 300 × 1
    ##    contents                                        
    ##    <chr>                                           
    ##  1 zBBtHnnHtwwHplmlRlzPLCpp                        
    ##  2 vvhJccJFGFcNsdNNJbhJsJQplQMRLQMlfdfTPCLfQQCT    
    ##  3 GPhjcjhZDjWtnSVH                                
    ##  4 BNhHVhrGNVTbDHdDJdJRPJdSQQSJwPjR                
    ##  5 lvtsfbsqzwSnJcvjSm                              
    ##  6 MftttFLftZMLgtgMbltMqZzbDNrTpVGhNWrDTrpTGNpZGZhD
    ##  7 VSSHcTgTtTdtllZlzmmbljTn                        
    ##  8 RqMqsFfQLLFLQFMMfRLPZLvPpCfWrbpmCbjCnfjlWmnrmmnm
    ##  9 hqRDqPDRsqNHwtHSNBZtJd                          
    ## 10 tNFDpDFrtdjfmjjjFmFFdScpZhZScTJgpHccHhMJgS      
    ## # … with 290 more rows

``` r
priority <- tibble(
  common=c(letters, LETTERS),
  priority=1:52
)

puzz1 <- input %>%
  mutate(
    len = str_length(contents),
    Comp1=str_sub(contents, 1, len/2),
    Comp2=str_sub(contents, len/2+1, len),
    common=str_match(Comp1, str_c("[",Comp2, "]"))
  ) %>%
  left_join(priority, by="common")

puzz1 %>% pull(priority) %>% sum()
```

    ## [1] 7597

``` r
puzz2 <- input %>%
  mutate(
    Group=ceiling((row_number())/3),
    Pers=str_c("Pers", (row_number()-1) %% 3)
  ) %>%
  pivot_wider(id_cols=Group, names_from=Pers, values_from = contents) 

priority_v <- 1:52
names(priority_v) <- c(letters, LETTERS)

priority_sum <- 0
for (i in 1:nrow(puzz2)){
  Pers0 <- puzz2 %>% slice(i) %>% pull(Pers0)
  Pers1 <- puzz2 %>% slice(i) %>% pull(Pers1)
  Pers2 <- puzz2 %>% slice(i) %>% pull(Pers2)
  
  commlet <- Pers0 %>% str_match_all(str_c("[", Pers1, "]")) %>% 
    .[[1]] %>% as.vector() %>%
    unique() %>% str_c(collapse="") %>%
    str_match_all(str_c("[", Pers2, "]")) %>%
    .[[1]] %>% as.vector()
  
  priority_sum <- priority_sum+ priority_v[commlet]
}

priority_sum
```

    ##    P 
    ## 2607
