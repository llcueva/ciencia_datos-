Desafío I
================
Lucía Cueva
2024-08-29

# PREPARAR LA INFORMACION

``` r
# cargo la librerias Dplyr y tidyverse
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ forcats   1.0.0     ✔ readr     2.1.5
    ## ✔ ggplot2   3.5.1     ✔ stringr   1.5.1
    ## ✔ lubridate 1.9.3     ✔ tibble    3.2.1
    ## ✔ purrr     1.0.2     ✔ tidyr     1.3.1

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
#para buscar dirrecion de la base de datos de interés
file.choose() 
```

    ## [1] "D:\\PROGRAMA CIENCIA DE DATOS\\Modulo I\\Desafio I\\Desafio_I.Rmd"

``` r
#cargar la base de datos: data
data <- read.csv("D:\\PROGRAMA CIENCIA DE DATOS\\Modulo I\\Iowa_Liquor_Sales.csv", 
                 stringsAsFactors = F, 
                 header = T)
# creo una nueva base de datos "datos" a partir de data con algunas modificaciones
datos <- data%>%
  mutate(Sale..Dollars.=(as.numeric(substr(data$Sale..Dollars.,2,15))),
         City=toupper(City),
         Store.Name=(toupper(Store.Name)),
         Date=as.Date(Date,format = "%m/%d/%Y"),
         anio=lubridate::year(Date)) %>% 
  rename(ventas=Sale..Dollars.,
         ciudad=City,
         categoria=Category.Name,
         nombre_tienda=Store.Name)
```

# Top 5 de tiendas (promedio ventas) para ciudad CEDAR RAPIDS, 2016

``` r
datos %>% 
  filter(anio== 2016, ciudad=="CEDAR RAPIDS") %>% 
  group_by(ciudad, nombre_tienda, anio) %>%
  summarise_each(.,funs(media=mean(.,na.rm=T)), ventas) %>%
  arrange(-media) %>% 
  pivot_wider(names_from = ciudad,
              values_from = media)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 82 × 3
    ## # Groups:   nombre_tienda [82]
    ##    nombre_tienda                        anio `CEDAR RAPIDS`
    ##    <chr>                               <dbl>          <dbl>
    ##  1 SAM'S CLUB 8162 / CEDAR RAPIDS       2016           354.
    ##  2 FAREWAY STORES #151 / CEDAR RAPIDS   2016           338.
    ##  3 BENZ DISTRIBUTING                    2016           171.
    ##  4 LEO1  /  CEDAR RAPIDS                2016           163.
    ##  5 TARGET STORE T-1771 / CEDAR RAPIDS   2016           162.
    ##  6 CASEY'S GENERAL STORE #2763 / CEDAR  2016           157.
    ##  7 WAL-MART 2716 / CEDAR RAPIDS         2016           156.
    ##  8 FAS MART # 5150/ CEDAR RAPIDS        2016           152.
    ##  9 WAL-MART 1528 / CEDAR RAPIDS         2016           152.
    ## 10 WALGREENS #03875 / CEDAR RAPIDS      2016           144.
    ## # ℹ 72 more rows

# TOP 5 últimos vendedores (promedio de ventas, para el 2016, para DAVENPORT

``` r
datos %>% 
  filter(anio== 2016, ciudad=="DAVENPORT") %>% 
  group_by(ciudad, Vendor.Name, anio) %>%
  summarise_each(.,funs(media=mean(.,na.rm=T)), ventas) %>%
  arrange(+media) %>% 
  pivot_wider(names_from = ciudad,
              values_from = media)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 42 × 3
    ## # Groups:   Vendor.Name [42]
    ##    Vendor.Name                    anio DAVENPORT
    ##    <chr>                         <dbl>     <dbl>
    ##  1 Luxco-St Louis                 2016      36.5
    ##  2 A HARDY USA LTD                2016      37.0
    ##  3 Rumcoqui and Co                2016      38.3
    ##  4 Prestige Wine & Spirits Group  2016      42.1
    ##  5 Dehner Distillery              2016      42.7
    ##  6 Stoli Group                    2016      43.1
    ##  7 McCormick Distilling Co.       2016      46.9
    ##  8 Mccormick Distilling Company   2016      48.6
    ##  9 Blaum Bros. Distilling Co.     2016      49  
    ## 10 Aiko Importers Inc             2016      49.0
    ## # ℹ 32 more rows

# Top 5 de productos más vendidos, para el 2016 y 2017, por ciudad

``` r
# 2016
datos %>% 
  filter(anio==2016) %>% 
  group_by(ciudad, categoria, anio) %>%
  summarise_each(.,funs(ventas_total=sum(.,na.rm=T)), ventas) %>%
  arrange(ciudad, -ventas_total) %>% 
  pivot_wider(names_from = anio,
              values_from = ventas_total,
              values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 15 × 3
    ## # Groups:   ciudad, categoria [15]
    ##    ciudad       categoria       `2016`
    ##    <chr>        <chr>            <dbl>
    ##  1 CEDAR RAPIDS VODKA FLAVORED 387502.
    ##  2 CEDAR RAPIDS FLAVORED RUM   225265.
    ##  3 CEDAR RAPIDS CREAM LIQUEURS 176921.
    ##  4 CEDAR RAPIDS Cream Liqueurs 122494.
    ##  5 CEDAR RAPIDS Flavored Rum    73943.
    ##  6 DAVENPORT    VODKA FLAVORED 340294.
    ##  7 DAVENPORT    FLAVORED RUM   164755.
    ##  8 DAVENPORT    CREAM LIQUEURS 130100.
    ##  9 DAVENPORT    Cream Liqueurs  79628.
    ## 10 DAVENPORT    Flavored Rum    47737.
    ## 11 WATERLOO     VODKA FLAVORED 186123.
    ## 12 WATERLOO     FLAVORED RUM   135026.
    ## 13 WATERLOO     CREAM LIQUEURS  88358.
    ## 14 WATERLOO     Cream Liqueurs  46449.
    ## 15 WATERLOO     Flavored Rum    35207.

``` r
# 2017
datos %>% 
  filter(anio==2017) %>% 
  group_by(ciudad, categoria, anio) %>%
  summarise_each(.,funs(ventas_total=sum(.,na.rm=T)), ventas) %>%
  arrange(ciudad, -ventas_total) %>% 
  pivot_wider(names_from = anio,
              values_from = ventas_total,
              values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 6 × 3
    ## # Groups:   ciudad, categoria [6]
    ##   ciudad       categoria      `2017`
    ##   <chr>        <chr>           <dbl>
    ## 1 CEDAR RAPIDS Cream Liqueurs 33633.
    ## 2 CEDAR RAPIDS Flavored Rum   30469.
    ## 3 DAVENPORT    Cream Liqueurs 24267.
    ## 4 DAVENPORT    Flavored Rum   17191.
    ## 5 WATERLOO     Flavored Rum   14199.
    ## 6 WATERLOO     Cream Liqueurs 13720.

``` r
# 2016 y 2017
datos %>% 
  filter(anio!=2015) %>% 
  group_by(ciudad, categoria, anio) %>%
  summarise_each(.,funs(ventas_total=sum(.,na.rm=T)), ventas) %>%
  arrange(ciudad, -ventas_total) %>% 
  pivot_wider(names_from = anio,
              values_from = ventas_total,
              values_fill = 0)
```

    ## Warning: `summarise_each()` was deprecated in dplyr 0.7.0.
    ## ℹ Please use `across()` instead.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## Warning: `funs()` was deprecated in dplyr 0.8.0.
    ## ℹ Please use a list of either functions or lambdas:
    ## 
    ## # Simple named list: list(mean = mean, median = median)
    ## 
    ## # Auto named with `tibble::lst()`: tibble::lst(mean, median)
    ## 
    ## # Using lambdas list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

    ## # A tibble: 15 × 4
    ## # Groups:   ciudad, categoria [15]
    ##    ciudad       categoria       `2016` `2017`
    ##    <chr>        <chr>            <dbl>  <dbl>
    ##  1 CEDAR RAPIDS VODKA FLAVORED 387502.     0 
    ##  2 CEDAR RAPIDS FLAVORED RUM   225265.     0 
    ##  3 CEDAR RAPIDS CREAM LIQUEURS 176921.     0 
    ##  4 CEDAR RAPIDS Cream Liqueurs 122494. 33633.
    ##  5 CEDAR RAPIDS Flavored Rum    73943. 30469.
    ##  6 DAVENPORT    VODKA FLAVORED 340294.     0 
    ##  7 DAVENPORT    FLAVORED RUM   164755.     0 
    ##  8 DAVENPORT    CREAM LIQUEURS 130100.     0 
    ##  9 DAVENPORT    Cream Liqueurs  79628. 24267.
    ## 10 DAVENPORT    Flavored Rum    47737. 17191.
    ## 11 WATERLOO     VODKA FLAVORED 186123.     0 
    ## 12 WATERLOO     FLAVORED RUM   135026.     0 
    ## 13 WATERLOO     CREAM LIQUEURS  88358.     0 
    ## 14 WATERLOO     Cream Liqueurs  46449. 13720.
    ## 15 WATERLOO     Flavored Rum    35207. 14199.
