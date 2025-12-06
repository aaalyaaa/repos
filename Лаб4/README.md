# Практическая работа 004
gribanovaaalya@yandex.ru

## Название

Исследование метаданных DNS трафика

## Цель работы

1.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R
3.  Закрепить навыки исследования метаданных DNS трафика

## Исходные данные

1.  Ноутбук с ОС Windows 10
2.  Rstudio Desktop
3.  Интерпретатор R 4.5.1

## Задание

Используя программный пакет dplyr, освоить анализ DNS логов с помощью
языка программирования R

## План выполнения

1.  Импортируйте данные DNS
    https://storage.yandexcloud.net/dataset.ctfsec/dns.zip
2.  Добавьте пропущенные данные о структуре данных (назначении столбцов)
3.  Преобразуйте данные в столбцах в нужный формат
4.  Просмотрите общую структуру данных с помощью функции glimpse()
5.  Сколько участников информационного обмена в сети Доброй Организации?
6.  Какое соотношение участников обмена внутри сети и участников
    обращений к внешним ресурсам?
7.  Найдите топ-10 участников сети, проявляющих наибольшую сетевую
    активность
8.  Найдите топ-10 доменов, к которым обращаются пользователи сети и
    соответственное количество обращений
9.  Опеределите базовые статистические характеристики (функция
    summary()) интервала времени между последовательными обращениями к
    топ-10 доменам
10. Часто вредоносное программное обеспечение использует DNS канал в
    качестве канала управления, периодически отправляя запросы на
    подконтрольный злоумышленникам DNS сервер. По периодическим запросам
    на один и тот же домен можно выявить скрытый DNS канал. Есть ли
    такие IP адреса в исследуемом датасете?
11. Определите местоположение (страну, город) и организацию-провайдера
    для топ-10 доменов. Для этого можно использовать сторонние сервисы,
    например http://ip-api.com (API-эндпоинт http://ip-api.com/json)

## Описание шагов

### Шаг 1

Подключим необходимые библиотеки:

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

``` r
library(readr)
```

    Warning: пакет 'readr' был собран под R версии 4.5.2

``` r
library(httr)
```

    Warning: пакет 'httr' был собран под R версии 4.5.2

``` r
library(tidyverse)
```

    Warning: пакет 'tidyverse' был собран под R версии 4.5.2

    Warning: пакет 'ggplot2' был собран под R версии 4.5.2

    Warning: пакет 'tidyr' был собран под R версии 4.5.2

    Warning: пакет 'purrr' был собран под R версии 4.5.2

    Warning: пакет 'forcats' был собран под R версии 4.5.2

    Warning: пакет 'lubridate' был собран под R версии 4.5.2

    ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ✔ forcats   1.0.1     ✔ stringr   1.5.2
    ✔ ggplot2   4.0.1     ✔ tibble    3.3.0
    ✔ lubridate 1.9.4     ✔ tidyr     1.3.1
    ✔ purrr     1.2.0     
    ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ✖ dplyr::filter() masks stats::filter()
    ✖ dplyr::lag()    masks stats::lag()
    ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

### Шаг 2

Выполним подготовку данных

#### 1. Импортируем данные:

``` r
dns_data <- read.csv(file = "dns.log", header = FALSE, sep = '\t', na.strings = c("-"))
```

#### 2. Добавим данные о структуре о структуре данных (назначении столбцов):

``` r
col_names <- c("timestamp", "uid", "id_orig_h", "id_orig_p", "id_resp_h", "id_resp_p", "proto", "trans_id", "query", "qclass", "qclass_name", "qtype", "qtype_name", "rcode", "rcode_name", "AA", "TC", "RD", "RA", "Z", "answers", "TTLs", "rejected")
```

``` r
names(dns_data) <- col_names
```

#### 3. Преобразуем данные в столбцах в нужный формат:

``` r
dns_data <- dns_data %>%
  mutate(timestamp = as_datetime(timestamp))
```

#### 4. Посмотрим общую структуру данных:

``` r
glimpse(dns_data)
```

    Rows: 427,935
    Columns: 23
    $ timestamp   <dttm> 2012-03-16 12:30:05, 2012-03-16 12:30:15, 2012-03-16 12:3…
    $ uid         <chr> "CWGtK431H9XuaTN4fi", "C36a282Jljz7BsbGH", "C36a282Jljz7Bs…
    $ id_orig_h   <chr> "192.168.202.100", "192.168.202.76", "192.168.202.76", "19…
    $ id_orig_p   <int> 45658, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 1…
    $ id_resp_h   <chr> "192.168.27.203", "192.168.202.255", "192.168.202.255", "1…
    $ id_resp_p   <int> 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137, 137…
    $ proto       <chr> "udp", "udp", "udp", "udp", "udp", "udp", "udp", "udp", "u…
    $ trans_id    <int> 33008, 57402, 57402, 57402, 57398, 57398, 57398, 62187, 62…
    $ query       <chr> "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\…
    $ qclass      <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1…
    $ qclass_name <chr> "C_INTERNET", "C_INTERNET", "C_INTERNET", "C_INTERNET", "C…
    $ qtype       <int> 33, 32, 32, 32, 32, 32, 32, 32, 32, 32, 33, 33, 33, 12, 12…
    $ qtype_name  <chr> "SRV", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB", "NB…
    $ rcode       <int> 0, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ rcode_name  <chr> "NOERROR", NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,…
    $ AA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ TC          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ RD          <lgl> FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRU…
    $ RA          <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…
    $ Z           <int> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 1, 1, 1, 1, 0…
    $ answers     <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ TTLs        <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    $ rejected    <lgl> FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FA…

### Шаг 3

Проведем анализ данных

#### 5. Определим количество участников информационного обмена в сети Доброй Организации:

``` r
length(unique(c(dns_data$id_orig_h, dns_data$id_resp_h)))
```

    [1] 1359

#### 6. Определим соотношение участников обмена внутри сети и участников обращений к внешним ресурсам:

``` r
private_networks <- "^(192\\.168\\.|10\\.|172\\.(1[6-9]|2[0-9]|3[0-1])\\.)"
unique_ips <- unique(c(dns_data$id_orig_h, dns_data$id_resp_h))
ips_internal <- unique_ips[grepl(private_networks, unique_ips)]
ips_external <- unique_ips[!grepl(private_networks, unique_ips)]
length(ips_internal) / length(ips_external)
```

    [1] 13.77174

#### 7. Найдем топ-10 участников сети, проявляющих наибольшую сетевую активность:

``` r
dns_data %>% group_by(id_orig_h) %>% summarise(requests = n()) %>% arrange(desc(requests)) %>% head(10)
```

    # A tibble: 10 × 2
       id_orig_h       requests
       <chr>              <int>
     1 10.10.117.210      75943
     2 192.168.202.93     26522
     3 192.168.202.103    18121
     4 192.168.202.76     16978
     5 192.168.202.97     16176
     6 192.168.202.141    14967
     7 10.10.117.209      14222
     8 192.168.202.110    13372
     9 192.168.203.63     12148
    10 192.168.202.106    10784

#### 8. Найдем топ-10 доменов, к которым обращаются пользователи сети и соответственное количество обращений:

``` r
dns_data %>% group_by(query) %>% summarise(requests = n()) %>% arrange(desc(requests)) %>% head(10)
```

    # A tibble: 10 × 2
       query                                                                requests
       <chr>                                                                   <int>
     1 "teredo.ipv6.microsoft.com"                                             39273
     2 "tools.google.com"                                                      14057
     3 "www.apple.com"                                                         13390
     4 "time.apple.com"                                                        13109
     5 "safebrowsing.clients.google.com"                                       11658
     6 "*\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00…    10401
     7 "WPAD"                                                                   9134
     8 "44.206.168.192.in-addr.arpa"                                            7248
     9 "HPE8AA67"                                                               6929
    10 "ISATAP"                                                                 6569

#### 9. Опеределим базовые статистические характеристики (функция summary()) интервала времени между последовательными обращениями к топ-10 доменам.

``` r
top10 <- dns_data %>% group_by(query) %>% summarise(requests = n()) %>% arrange(desc(requests)) %>% head(10)

for(domain in top10$query) {
  intervals <- dns_data %>%
    filter(query == domain) %>%
    arrange(timestamp) %>%
    pull(timestamp) %>%
    diff()
  
  cat("\n", domain, ":\n")
  print(summary(intervals))
}
```


     teredo.ipv6.microsoft.com :
    Time differences in secs
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     0.000     2.941     0.510 50387.760 

     tools.google.com :
    Time differences in secs
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     0.000     8.187     1.000 50364.830 

     www.apple.com :
    Time differences in secs
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.000     1.000     8.607     3.010 50963.630 

     time.apple.com :
    Time differences in secs
         Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
        0.000     0.370     1.760     8.665     4.723 50924.280 

     safebrowsing.clients.google.com :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.00     1.00    10.00     2.01 49952.32 

     *\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00 :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.15     0.50    11.24     1.50 52723.50 

     WPAD :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.75     0.75    12.61     1.11 50049.11 

     44.206.168.192.in-addr.arpa :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     2.09     4.00    16.01    20.09 49679.81 

     HPE8AA67 :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.75     0.75    16.61    25.49 50044.43 

     ISATAP :
    Time differences in secs
        Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
        0.00     0.75     0.76    17.46     1.05 51997.79 

#### 10. Часто вредоносное программное обеспечение использует DNS канал в качестве канала управления, периодически отправляя запросы на подконтрольный злоумышленникам DNS сервер. По периодическим запросам на один и тот же домен можно выявить скрытый DNS канал. Определим, есть ли такие IP адреса в исследуемом датасете:

``` r
suspicious_patterns <- dns_data %>%
  filter(!is.na(id_orig_h), !is.na(query), query != "") %>%
  arrange(timestamp) %>% group_by(id_orig_h, query) %>%
  filter(n() >= 35) %>%
  mutate(time_num = as.numeric(timestamp), time_diff = c(NA, diff(time_num))) %>%
  filter(!is.na(time_diff)) %>%
  summarise(request_count = n() + 1, mean_interval = mean(time_diff), std_interval = sd(time_diff), .groups = "drop") %>%
  filter(mean_interval <= 60, std_interval <= 2) %>%
  arrange(mean_interval, desc(request_count))

suspicious_patterns
```

    # A tibble: 14 × 5
       id_orig_h       query                request_count mean_interval std_interval
       <chr>           <chr>                        <dbl>         <dbl>        <dbl>
     1 10.10.117.210   hq.h                           377       0             0     
     2 10.10.117.210   httphq.hec.net                 102       0             0     
     3 10.10.117.210   www.h                           64       0             0     
     4 192.168.202.103 www.hkparts.net                 36       0.00371       0.0124
     5 192.168.202.103 lifehacker.com                  68       0.00612       0.0171
     6 10.10.117.210   29-courier.push.app…            56       0.237         1.76  
     7 192.168.202.49  ISATAP                          90       0.767         0.121 
     8 192.168.202.126 linkhelp.clients.go…            40       0.866         1.34  
     9 192.168.0.3     ISATAP                         108       0.874         0.313 
    10 192.168.202.157 ARMMF.ADOBE.COM                476       1.09          0.811 
    11 192.168.202.87  linkhelp.clients.go…            48       1.13          1.50  
    12 192.168.202.89  ISATAP                          78       1.19          1.63  
    13 192.168.202.71  zfoulwfnno                      43       1.25          1.98  
    14 192.168.202.102 ARMMF.ADOBE.COM                422       1.47          1.68  

### Шаг 4

Проведем обогащение данных

#### 10. Определим местоположение (страну, город) и организацию-провайдера для топ-10 доменов:

``` r
geo_data <- data.frame()

for(domain in top10$query) {
  url <- paste0("http://ip-api.com/json/", domain)
  response <- GET(url)
  if(status_code(response) == 200) {
    info <- content(response)
    geo_data <- rbind(geo_data, data.frame(
      domain_name = domain,
      country = ifelse(is.null(info$country), NA, info$country),
      city = ifelse(is.null(info$city), NA, info$city),
      organization = ifelse(is.null(info$isp), NA, info$isp)))
  } else {
    geo_data <- rbind(geo_data, data.frame(
      domain_name = domain,
      country = NA,
      city = NA,
      organization = NA))}
  Sys.sleep(0.5)}
geo_data
```

                                                                   domain_name
    1                                                teredo.ipv6.microsoft.com
    2                                                         tools.google.com
    3                                                            www.apple.com
    4                                                           time.apple.com
    5                                          safebrowsing.clients.google.com
    6  *\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00\\x00
    7                                                                     WPAD
    8                                              44.206.168.192.in-addr.arpa
    9                                                                 HPE8AA67
    10                                                                  ISATAP
              country          city        organization
    1            <NA>          <NA>                <NA>
    2  United Kingdom        London          Google LLC
    3  United Kingdom        London Akamai Technologies
    4          France        Clichy          Apple Inc.
    5   United States Mountain View          Google LLC
    6            <NA>          <NA>                <NA>
    7            <NA>          <NA>                <NA>
    8            <NA>          <NA>                <NA>
    9            <NA>          <NA>                <NA>
    10           <NA>          <NA>                <NA>

## Вывод

В результате выполнения практической работы были закреплены навыки
использования языка R для исследования DNS логов
