# Практическая работа 005
gribanovaaalya@yandex.ru

## Название

Исследование информации о состоянии беспроводных сетей

## Цель работы

1.  Получить знания о методах исследования радиоэлектронной обстановки
2.  Составить представление о механизмах работы Wi-Fi сетей на канальном
    и сетевом уровне модели OSI
3.  Зекрепить практические навыки использования языка программирования R
    для обработки данных
4.  Закрепить знания основных функций обработки данных экосистемы
    tidyverse языка R

## Исходные данные

1.  Ноутбук с ОС Windows 10
2.  Rstudio Desktop
3.  Интерпретатор R 4.5.1

## Задание

Используя программный пакет dplyr языка программирования R провести
анализ журналов и ответить на вопросы

## План выполнения

1.  Импортировать данные
2.  Провести анализ точек доступа
3.  Провести анализ данных клиентов

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

#### 1. Импортируем данные:

``` r
td_data <- read_csv("P2_wifi_data.csv", skip = 1, n_max = 167)
```

    Rows: 167 Columns: 15
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (6): BSSID, Privacy, Cipher, Authentication, LAN IP, ESSID
    dbl  (6): channel, Speed, Power, # beacons, # IV, ID-length
    lgl  (1): Key
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
client_data <- read_csv("P2_wifi_data.csv", skip = 169)
```

    Warning: One or more parsing issues, call `problems()` on your data frame for details,
    e.g.:
      dat <- vroom(...)
      problems(dat)

    Rows: 12081 Columns: 7
    ── Column specification ────────────────────────────────────────────────────────
    Delimiter: ","
    chr  (3): Station MAC, BSSID, Probed ESSIDs
    dbl  (2): Power, # packets
    dttm (2): First time seen, Last time seen

    ℹ Use `spec()` to retrieve the full column specification for this data.
    ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

#### 2. Приведем датасеты к аккуратному виду:

``` r
td_data <- td_data %>% 
  rename(
    First_time_seen = 2,
    Last_time_seen = 3,
    Channel = 4,
    Beacons = 10,
    IV = 11,
    LAN_IP = 12,
    ID_length = 13) %>%
  mutate(
    Channel = as.integer(Channel),
    Speed = as.integer(Speed),
    Power = as.integer(Power),
    Beacons = as.integer(Beacons),
    IV = as.integer(IV),
    ID_length = as.integer(ID_length)
  )
```

``` r
client_data <- client_data %>% 
  rename(
    Station_MAC = 1,
    First_time_seen = 2,
    Last_time_seen = 3,
    Packets = 5,
    Probed_ESSIDs = 7) %>%
  mutate(
    Power = as.integer(Power),
    Packets = as.integer(Packets)
  )
```

#### 3. Посмотрим общую структуру данных:

``` r
glimpse(td_data)
```

    Rows: 167
    Columns: 15
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "6E:C7:EC:16:DA:1A", "9A:75:A8:B9…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 11:50:50, 2023-07-28 11:55:12, 2023-07-28 …
    $ Channel         <int> 1, 1, 1, 7, 6, 6, 11, 11, 11, 1, 6, 14, 11, 11, 6, 6, …
    $ Speed           <int> 195, 130, 360, 360, 130, 130, 195, 130, 130, 195, 180,…
    $ Privacy         <chr> "WPA2", "WPA2", "WPA2", "WPA2", "WPA2", "OPN", "WPA2",…
    $ Cipher          <chr> "CCMP", "CCMP", "CCMP", "CCMP", "CCMP", NA, "CCMP", "C…
    $ Authentication  <chr> "PSK", "PSK", "PSK", "PSK", "PSK", NA, "PSK", "PSK", "…
    $ Power           <int> -30, -30, -68, -37, -57, -63, -27, -38, -38, -66, -42,…
    $ Beacons         <int> 846, 750, 694, 510, 647, 251, 1647, 1251, 704, 617, 13…
    $ IV              <int> 504, 116, 26, 21, 6, 3430, 80, 11, 0, 0, 86, 0, 0, 0, …
    $ LAN_IP          <chr> "0.  0.  0.  0", "0.  0.  0.  0", "0.  0.  0.  0", "0.…
    $ ID_length       <int> 12, 4, 2, 14, 25, 13, 12, 13, 24, 12, 10, 0, 24, 24, 1…
    $ ESSID           <chr> "C322U13 3965", "Cnet", "KC", "POCO X5 Pro 5G", NA, "M…
    $ Key             <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…

``` r
glimpse(client_data)
```

    Rows: 12,081
    Columns: 7
    $ Station_MAC     <chr> "CA:66:3B:8F:56:DD", "96:35:2D:3D:85:E6", "5C:3A:45:9E…
    $ First_time_seen <dttm> 2023-07-28 09:13:03, 2023-07-28 09:13:03, 2023-07-28 …
    $ Last_time_seen  <dttm> 2023-07-28 10:59:44, 2023-07-28 09:13:03, 2023-07-28 …
    $ Power           <int> -33, -65, -39, -61, -53, -43, -31, -71, -74, -65, -45,…
    $ Packets         <int> 858, 4, 432, 958, 1, 344, 163, 3, 115, 437, 265, 77, 7…
    $ BSSID           <chr> "BE:F1:71:D5:17:8B", "(not associated)", "BE:F1:71:D6:…
    $ Probed_ESSIDs   <chr> "C322U13 3965", "IT2 Wireless", "C322U21 0566", "C322U…

### Шаг 2

Выполним анализ точек доступа:

#### 1. Определим небезопасные точки доступа (без шифрования – OPN):

``` r
unsave_p <- td_data %>% filter(Privacy == "OPN") %>% select(BSSID)
unsave_p
```

    # A tibble: 42 × 1
       BSSID            
       <chr>            
     1 E8:28:C1:DC:B2:52
     2 E8:28:C1:DC:B2:50
     3 E8:28:C1:DC:B2:51
     4 E8:28:C1:DC:FF:F2
     5 00:25:00:FF:94:73
     6 E8:28:C1:DD:04:52
     7 E8:28:C1:DE:74:31
     8 E8:28:C1:DE:74:32
     9 E8:28:C1:DC:C8:32
    10 E8:28:C1:DD:04:50
    # ℹ 32 more rows

#### 2. Определим производителя для каждого обнаруженного устройства:

``` r
manuf_db <- read.delim("manuf.txt", sep = "\t", header = FALSE,
                       comment.char = "#", strip.white = TRUE,
                       col.names = c("MAC_prefix", "Short_name", "Full_name"),
                       quote = "")
```

``` r
unsave_p %>% mutate(OUI = substr(BSSID, 1, 8)) %>% left_join(manuf_db, by = c("OUI" = "MAC_prefix")) %>% select(BSSID, Full_name)
```

    # A tibble: 42 × 2
       BSSID             Full_name            
       <chr>             <chr>                
     1 E8:28:C1:DC:B2:52 Eltex Enterprise Ltd.
     2 E8:28:C1:DC:B2:50 Eltex Enterprise Ltd.
     3 E8:28:C1:DC:B2:51 Eltex Enterprise Ltd.
     4 E8:28:C1:DC:FF:F2 Eltex Enterprise Ltd.
     5 00:25:00:FF:94:73 Apple, Inc.          
     6 E8:28:C1:DD:04:52 Eltex Enterprise Ltd.
     7 E8:28:C1:DE:74:31 Eltex Enterprise Ltd.
     8 E8:28:C1:DE:74:32 Eltex Enterprise Ltd.
     9 E8:28:C1:DC:C8:32 Eltex Enterprise Ltd.
    10 E8:28:C1:DD:04:50 Eltex Enterprise Ltd.
    # ℹ 32 more rows

#### 3. Выявим устройства, использующие последнюю версию протокола шифрования WPA3, и названия точек доступа, реализованных на этих устройствах:

``` r
td_data %>% filter(str_detect(Privacy, "WPA3")) %>% select(BSSID, ESSID, Privacy)
```

    # A tibble: 8 × 3
      BSSID             ESSID                                          Privacy  
      <chr>             <chr>                                          <chr>    
    1 26:20:53:0C:98:E8  <NA>                                          WPA3 WPA2
    2 A2:FE:FF:B8:9B:C9 "Christie’s"                                   WPA3 WPA2
    3 96:FF:FC:91:EF:64  <NA>                                          WPA3 WPA2
    4 CE:48:E7:86:4E:33 "iPhone (Анастасия)"                           WPA3 WPA2
    5 8E:1F:94:96:DA:FD "iPhone (Анастасия)"                           WPA3 WPA2
    6 BE:FD:EF:18:92:44 "Димасик"                                      WPA3 WPA2
    7 3A:DA:00:F9:0C:02 "iPhone XS Max \U0001f98a\U0001f431\U0001f98a" WPA3 WPA2
    8 76:C5:A0:70:08:96  <NA>                                          WPA3 WPA2

#### 4. Отсортируем точки доступа по интервалу времени, в течение которого они находились на связи, по убыванию:

``` r
session_result <- td_data %>%
  arrange(BSSID, First_time_seen) %>%
  group_by(BSSID) %>%
  mutate(
    time_break = as.numeric(
      First_time_seen - lag(Last_time_seen, default = first(First_time_seen))
    ),
    session_number = cumsum(time_break > 2700) + 1
  ) %>%
  group_by(BSSID, session_number) %>%
  summarise(
    session_begin = min(First_time_seen),
    session_complete = max(Last_time_seen),
    .groups = "drop"
  ) %>%
  mutate(
    active_seconds = as.numeric(session_complete - session_begin)
  ) %>%
  arrange(desc(active_seconds)) %>%
  select(BSSID, session_begin, session_complete, active_seconds)

session_result
```

    # A tibble: 167 × 4
       BSSID             session_begin       session_complete    active_seconds
       <chr>             <dttm>              <dttm>                       <dbl>
     1 00:25:00:FF:94:73 2023-07-28 09:13:06 2023-07-28 11:56:21           9795
     2 E8:28:C1:DD:04:52 2023-07-28 09:13:09 2023-07-28 11:56:05           9776
     3 E8:28:C1:DC:B2:52 2023-07-28 09:13:03 2023-07-28 11:55:38           9755
     4 08:3A:2F:56:35:FE 2023-07-28 09:13:27 2023-07-28 11:55:53           9746
     5 6E:C7:EC:16:DA:1A 2023-07-28 09:13:03 2023-07-28 11:55:12           9729
     6 E8:28:C1:DC:B2:50 2023-07-28 09:13:06 2023-07-28 11:55:12           9726
     7 48:5B:39:F9:7A:48 2023-07-28 09:13:06 2023-07-28 11:55:11           9725
     8 E8:28:C1:DC:B2:51 2023-07-28 09:13:06 2023-07-28 11:55:11           9725
     9 E8:28:C1:DC:FF:F2 2023-07-28 09:13:06 2023-07-28 11:55:10           9724
    10 8E:55:4A:85:5B:01 2023-07-28 09:13:06 2023-07-28 11:55:09           9723
    # ℹ 157 more rows

#### 5. Обнаружим топ-10 самых быстрых точек доступа:

``` r
td_data %>% arrange(desc(Speed)) %>% select(BSSID, Speed) %>% head(10)
```

    # A tibble: 10 × 2
       BSSID             Speed
       <chr>             <int>
     1 26:20:53:0C:98:E8   866
     2 96:FF:FC:91:EF:64   866
     3 CE:48:E7:86:4E:33   866
     4 8E:1F:94:96:DA:FD   866
     5 9A:75:A8:B9:04:1E   360
     6 4A:EC:1E:DB:BF:95   360
     7 56:C5:2B:9F:84:90   360
     8 E8:28:C1:DC:B2:41   360
     9 E8:28:C1:DC:B2:40   360
    10 E8:28:C1:DC:B2:42   360

#### 6. Отсортировать точки доступа по частоте отправки запросов (beacons) в единицу времени по их убыванию:

``` r
td_data %>% group_by(BSSID, ESSID) %>%
  summarise(
    Total_beac = sum(Beacons),
    Time_start = min(First_time_seen),
    Time_end = max(Last_time_seen),
    .groups = "drop"
  ) %>%
  mutate(
    Sec = as.numeric(Time_end - Time_start, units = "secs"),
    Beac_per_sec = Total_beac / Sec
  ) %>%
  filter(Sec > 0) %>%
  ungroup() %>%
  arrange(desc(Beac_per_sec)) %>%
  select(BSSID, Total_beac, Sec, Beac_per_sec)
```

    # A tibble: 124 × 4
       BSSID             Total_beac   Sec Beac_per_sec
       <chr>                  <int> <dbl>        <dbl>
     1 F2:30:AB:E9:03:ED          6     7        0.857
     2 B2:CF:C0:00:4A:60          4     5        0.8  
     3 3A:DA:00:F9:0C:02          5     9        0.556
     4 00:3E:1A:5D:14:45          1     2        0.5  
     5 02:BC:15:7E:D5:DC          1     2        0.5  
     6 76:C5:A0:70:08:96          1     2        0.5  
     7 D2:25:91:F6:6C:D8          5    13        0.385
     8 BE:F1:71:D6:10:D7       1647  9461        0.174
     9 00:03:7A:1A:03:56          1     6        0.167
    10 38:1A:52:0D:84:D7        704  4319        0.163
    # ℹ 114 more rows

### Шаг 3

Выполним анализ данных клиентов:

#### 1. Определим производителя для каждого обнаруженного устройства:

``` r
client_data %>% mutate(OUI = substr(Station_MAC, 1, 8)) %>% left_join(manuf_db, by = c("OUI" = "MAC_prefix")) %>% select(Station_MAC, Full_name)
```

    # A tibble: 12,081 × 2
       Station_MAC       Full_name                           
       <chr>             <chr>                               
     1 CA:66:3B:8F:56:DD <NA>                                
     2 96:35:2D:3D:85:E6 <NA>                                
     3 5C:3A:45:9E:1A:7B Chongqing Fugui Electronics Co.,Ltd.
     4 C0:E4:34:D8:E7:E5 AzureWave Technology Inc.           
     5 5E:8E:A6:5E:34:81 <NA>                                
     6 10:51:07:CB:33:E7 Intel Corporate                     
     7 68:54:5A:40:35:9E Intel Corporate                     
     8 74:4C:A1:70:CE:F7 Liteon Technology Corporation       
     9 8A:A3:5A:33:76:57 <NA>                                
    10 CA:54:C4:8B:B5:3A <NA>                                
    # ℹ 12,071 more rows

#### 2. Обнаружим устройства, которые НЕ рандомизируют свой MAC адрес:

``` r
client_data %>% filter(!substr(Station_MAC, 2, 2) %in% c("2", "6", "A", "E")) %>% distinct() %>% select(Station_MAC)
```

    # A tibble: 220 × 1
       Station_MAC      
       <chr>            
     1 5C:3A:45:9E:1A:7B
     2 C0:E4:34:D8:E7:E5
     3 10:51:07:CB:33:E7
     4 68:54:5A:40:35:9E
     5 74:4C:A1:70:CE:F7
     6 BC:F1:71:D4:DB:04
     7 4C:44:5B:14:76:E3
     8 A0:E7:0B:AE:D5:44
     9 00:95:69:E7:7F:35
    10 00:95:69:E7:7C:ED
    # ℹ 210 more rows

#### 3. Кластеризуем запросы от устройств к точкам доступа по их именам. Определим время появления устройства в зоне радиовидимости и время выхода его из нее:

``` r
result <- client_data %>%
  filter(Probed_ESSIDs != "" & !is.na(Probed_ESSIDs)) %>%
  mutate(networks = strsplit(Probed_ESSIDs, ",")) %>%
  unnest(networks) %>%
  mutate(networks = trimws(networks)) %>%
  group_by(Station_MAC, networks) %>%
  summarise(
    first = min(First_time_seen),
    last = max(Last_time_seen),
    .groups = "drop")

result
```

    # A tibble: 1,831 × 4
       Station_MAC       networks            first               last               
       <chr>             <chr>               <dttm>              <dttm>             
     1 00:90:4C:E6:54:54 "Redmi"             2023-07-28 09:16:59 2023-07-28 10:21:15
     2 00:95:69:E7:7C:ED "nvripcsuite"       2023-07-28 09:13:11 2023-07-28 11:56:13
     3 00:95:69:E7:7D:21 "nvripcsuite"       2023-07-28 09:13:15 2023-07-28 11:56:17
     4 00:95:69:E7:7F:35 "nvripcsuite"       2023-07-28 09:13:11 2023-07-28 11:56:07
     5 00:F4:8D:F7:C5:19 "Hornet24"          2023-07-28 10:45:04 2023-07-28 11:43:26
     6 00:F4:8D:F7:C5:19 "Redmi 12"          2023-07-28 10:45:04 2023-07-28 11:43:26
     7 02:00:00:00:00:00 "CPPK_FREE"         2023-07-28 09:54:40 2023-07-28 11:55:36
     8 02:00:00:00:00:00 "MIREA"             2023-07-28 09:54:40 2023-07-28 11:55:36
     9 02:00:00:00:00:00 "MIREA_HOTSPOT"     2023-07-28 09:54:40 2023-07-28 11:55:36
    10 02:00:00:00:00:00 "\\xAC\\xBA\\xAC\\… 2023-07-28 09:54:40 2023-07-28 11:55:36
    # ℹ 1,821 more rows

#### 4. Оценим стабильность уровня сигнала внури кластера во времени. Выявим наиболее стабильный кластер:

``` r
client_data %>%
  filter(Probed_ESSIDs != "" & !is.na(Probed_ESSIDs)) %>%
  mutate(networks = strsplit(Probed_ESSIDs, ",")) %>%
  unnest(networks) %>%
  mutate(networks = trimws(networks)) %>%
  group_by(device = Station_MAC, network = networks) %>%
  summarise(
    request_count = n(),
    avg_power = mean(Power, na.rm = TRUE),
    sd_power = if (n() == 1) 0 else sd(Power, na.rm = TRUE),
    .groups = "drop") %>%
  filter(request_count > 1) %>%
  arrange(sd_power) %>%
  head(1)
```

    # A tibble: 1 × 5
      device            network request_count avg_power sd_power
      <chr>             <chr>           <int>     <dbl>    <dbl>
    1 BA:2B:CE:7D:F1:C4 podval              2       -65        0

## Вывод

В результате выполнения практической работы были получены знания о
методах исследования радиоэлектронной обстановки, составлено
представление о механизмах работы Wi-Fi сетей на канальном и сетевом
уровне модели OSI, закреплены практические навыки использования языка
программирования R для обработки данных.
