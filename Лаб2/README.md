# Практическая работа 002
gribanovaaalya@yandex.ru

## Название

Основы обработки данных с помощью R и Dplyr

## Цель работы

1.  Развить практические навыки использования языка программирования R
    для обработки данных
2.  Закрепить знания базовых типов данных языка R
3.  Развить практические навыки использования функций обработки данных
    пакета dplyr – функции select(), filter(), mutate(), arrange(),
    group_by()

## Исходные данные

1.  Ноутбук с ОС Windows 10
2.  Rstudio Desktop
3.  Интерпретатор R 4.5.1

## Задание

Проанализировать встроенный в пакет dplyr набор данных starwars с
помощью языка R и ответить на вопросы:

-   Сколько строк в датафрейме?
-   Сколько столбцов в датафрейме?
-   Как просмотреть примерный вид датафрейма?
-   Сколько уникальных рас персонажей (species) представлено в данных?
-   Найти самого высокого персонажа
-   Найти всех персонажей ниже 170
-   Подсчитать ИМТ (индекс массы тела) для всех персонажей.
-   Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по
    отношению массы (mass) к росту (height) персонажей.
-   Найти средний возраст персонажей каждой расы вселенной Звездных
    войн.
-   Найти самый распространенный цвет глаз персонажей вселенной Звездных
    войн.
-   Подсчитать среднюю длину имени в каждой расе вселенной Звездных
    войн.

## Описание шагов

### Шаг 1

Подключим библиотеку dplyr:

``` r
library(dplyr)
```


    Присоединяю пакет: 'dplyr'

    Следующие объекты скрыты от 'package:stats':

        filter, lag

    Следующие объекты скрыты от 'package:base':

        intersect, setdiff, setequal, union

### Шаг 2

Ответим на вопросы:

#### 1. Сколько строк в датафрейме?

``` r
starwars %>% nrow()
```

    [1] 87

#### 2. Сколько столбцов в датафрейме?

``` r
starwars %>% ncol()
```

    [1] 14

#### 3. Как просмотреть примерный вид датафрейма?

``` r
starwars %>% glimpse()
```

    Rows: 87
    Columns: 14
    $ name       <chr> "Luke Skywalker", "C-3PO", "R2-D2", "Darth Vader", "Leia Or…
    $ height     <int> 172, 167, 96, 202, 150, 178, 165, 97, 183, 182, 188, 180, 2…
    $ mass       <dbl> 77.0, 75.0, 32.0, 136.0, 49.0, 120.0, 75.0, 32.0, 84.0, 77.…
    $ hair_color <chr> "blond", NA, NA, "none", "brown", "brown, grey", "brown", N…
    $ skin_color <chr> "fair", "gold", "white, blue", "white", "light", "light", "…
    $ eye_color  <chr> "blue", "yellow", "red", "yellow", "brown", "blue", "blue",…
    $ birth_year <dbl> 19.0, 112.0, 33.0, 41.9, 19.0, 52.0, 47.0, NA, 24.0, 57.0, …
    $ sex        <chr> "male", "none", "none", "male", "female", "male", "female",…
    $ gender     <chr> "masculine", "masculine", "masculine", "masculine", "femini…
    $ homeworld  <chr> "Tatooine", "Tatooine", "Naboo", "Tatooine", "Alderaan", "T…
    $ species    <chr> "Human", "Droid", "Droid", "Human", "Human", "Human", "Huma…
    $ films      <list> <"A New Hope", "The Empire Strikes Back", "Return of the J…
    $ vehicles   <list> <"Snowspeeder", "Imperial Speeder Bike">, <>, <>, <>, "Imp…
    $ starships  <list> <"X-wing", "Imperial shuttle">, <>, <>, "TIE Advanced x1",…

#### 4. Сколько уникальных рас персонажей (species) представлено в данных?

``` r
starwars %>% select(species) %>% filter(!is.na(species)) %>% unique() %>% count() %>% as.data.frame() 
```

       n
    1 37

#### 5. Найти самого высокого персонажа.

``` r
starwars %>% arrange(desc(height)) %>% select(name, height) %>% head(1) %>% as.data.frame()
```

             name height
    1 Yarael Poof    264

#### 6. Найти всех персонажей ниже 170.

``` r
starwars %>% filter(height < 170) %>% select(name, height) %>% as.data.frame()
```

                        name height
    1                  C-3PO    167
    2                  R2-D2     96
    3            Leia Organa    150
    4     Beru Whitesun Lars    165
    5                  R5-D4     97
    6                   Yoda     66
    7             Mon Mothma    150
    8  Wicket Systri Warrick     88
    9              Nien Nunb    160
    10                 Watto    137
    11               Sebulba    112
    12        Shmi Skywalker    163
    13          Ratts Tyerel     79
    14              Dud Bolt     94
    15               Gasgano    122
    16        Ben Quadinaros    163
    17                 Cordé    157
    18         Barriss Offee    166
    19                 Dormé    165
    20            Zam Wesell    168
    21            Jocasta Nu    167
    22                R4-P17     96

#### 7. Подсчитать ИМТ (индекс массы тела) для всех персонажей.

``` r
starwars %>% mutate(imt = mass / (height^2)) %>% select(name, imt) %>% as.data.frame()
```

                        name         imt
    1         Luke Skywalker 0.002602758
    2                  C-3PO 0.002689232
    3                  R2-D2 0.003472222
    4            Darth Vader 0.003333007
    5            Leia Organa 0.002177778
    6              Owen Lars 0.003787401
    7     Beru Whitesun Lars 0.002754821
    8                  R5-D4 0.003400999
    9      Biggs Darklighter 0.002508286
    10        Obi-Wan Kenobi 0.002324598
    11      Anakin Skywalker 0.002376641
    12        Wilhuff Tarkin          NA
    13             Chewbacca 0.002154509
    14              Han Solo 0.002469136
    15                Greedo 0.002472518
    16 Jabba Desilijic Tiure 0.044342857
    17        Wedge Antilles 0.002664360
    18      Jek Tono Porkins 0.003395062
    19                  Yoda 0.003902663
    20             Palpatine 0.002595156
    21             Boba Fett 0.002335095
    22                 IG-88 0.003500000
    23                 Bossk 0.003130194
    24      Lando Calrissian 0.002521625
    25                 Lobot 0.002579592
    26                Ackbar 0.002561728
    27            Mon Mothma          NA
    28          Arvel Crynyd          NA
    29 Wicket Systri Warrick 0.002582645
    30             Nien Nunb 0.002656250
    31          Qui-Gon Jinn 0.002389326
    32           Nute Gunray 0.002467038
    33         Finis Valorum          NA
    34         Padmé Amidala 0.001314828
    35         Jar Jar Binks 0.001718034
    36          Roos Tarpals 0.001634247
    37            Rugor Nass          NA
    38              Ric Olié          NA
    39                 Watto          NA
    40               Sebulba 0.003188776
    41         Quarsh Panaka          NA
    42        Shmi Skywalker          NA
    43            Darth Maul 0.002612245
    44           Bib Fortuna          NA
    45           Ayla Secura 0.001735892
    46          Ratts Tyerel 0.002403461
    47              Dud Bolt 0.005092802
    48               Gasgano          NA
    49        Ben Quadinaros 0.002446460
    50            Mace Windu 0.002376641
    51          Ki-Adi-Mundi 0.002091623
    52             Kit Fisto 0.002264681
    53             Eeth Koth          NA
    54            Adi Gallia 0.001476843
    55           Saesee Tiin          NA
    56           Yarael Poof          NA
    57              Plo Koon 0.002263468
    58            Mas Amedda          NA
    59          Gregar Typho 0.002483565
    60                 Cordé          NA
    61           Cliegg Lars          NA
    62     Poggle the Lesser 0.002388844
    63       Luminara Unduli 0.001944637
    64         Barriss Offee 0.001814487
    65                 Dormé          NA
    66                 Dooku 0.002147709
    67   Bail Prestor Organa          NA
    68            Jango Fett 0.002358984
    69            Zam Wesell 0.001948696
    70       Dexter Jettster 0.002601775
    71               Lama Su 0.001678076
    72               Taun We          NA
    73            Jocasta Nu          NA
    74                R4-P17          NA
    75            Wat Tambor 0.001288625
    76              San Hill          NA
    77              Shaak Ti 0.001799015
    78              Grievous 0.003407922
    79               Tarfful 0.002483746
    80       Raymus Antilles 0.002235174
    81             Sly Moore 0.001514960
    82            Tion Medon 0.001885192
    83                  Finn          NA
    84                   Rey          NA
    85           Poe Dameron          NA
    86                   BB8          NA
    87        Captain Phasma          NA

#### 8. Найти 10 самых “вытянутых” персонажей. “Вытянутость” оценить по отношению массы (mass) к росту (height) персонажей.

``` r
starwars %>% mutate(v = mass / height) %>% arrange(v) %>% head(10) %>% select(name, v) %>% as.data.frame()
```

                        name         v
    1           Ratts Tyerel 0.1898734
    2  Wicket Systri Warrick 0.2272727
    3          Padmé Amidala 0.2432432
    4             Wat Tambor 0.2487047
    5                   Yoda 0.2575758
    6              Sly Moore 0.2696629
    7             Adi Gallia 0.2717391
    8          Barriss Offee 0.3012048
    9            Ayla Secura 0.3089888
    10              Shaak Ti 0.3202247

#### 9. Найти средний возраст персонажей каждой расы вселенной Звездных войн.

``` r
starwars %>% filter(!is.na(species)) %>% filter(!is.na(birth_year)) %>% group_by(species) %>% summarise(mean_y = mean(100 - birth_year)) %>% select(species, mean_y) %>% as.data.frame()
```

              species     mean_y
    1          Cerean    8.00000
    2           Droid   46.66667
    3            Ewok   92.00000
    4          Gungan   48.00000
    5           Human   46.25769
    6            Hutt -500.00000
    7         Kel Dor   78.00000
    8        Mirialan   51.00000
    9    Mon Calamari   59.00000
    10         Rodian   56.00000
    11     Trandoshan   47.00000
    12        Twi'lek   52.00000
    13        Wookiee -100.00000
    14 Yoda's species -796.00000
    15         Zabrak   46.00000

#### 10. Найти самый распространенный цвет глаз персонажей вселенной Звездных войн.

``` r
starwars %>% group_by(eye_color) %>% summarise(count = n()) %>% arrange(desc(count)) %>% select(eye_color) %>% head(1) %>% as.data.frame()
```

      eye_color
    1     brown

#### 11. Подсчитать среднюю длину имени в каждой расе вселенной Звездных войн.

``` r
starwars %>% filter(!is.na(species)) %>% filter(!is.na(name)) %>% group_by(species) %>% summarise(mean_l = mean(nchar(name))) %>% select(species, mean_l) %>% as.data.frame()
```

              species    mean_l
    1          Aleena 12.000000
    2        Besalisk 15.000000
    3          Cerean 12.000000
    4        Chagrian 10.000000
    5        Clawdite 10.000000
    6           Droid  4.833333
    7             Dug  7.000000
    8            Ewok 21.000000
    9       Geonosian 17.000000
    10         Gungan 11.666667
    11          Human 11.342857
    12           Hutt 21.000000
    13       Iktotchi 11.000000
    14        Kaleesh  8.000000
    15       Kaminoan  7.000000
    16        Kel Dor  8.000000
    17       Mirialan 14.000000
    18   Mon Calamari  6.000000
    19           Muun  8.000000
    20       Nautolan  9.000000
    21      Neimodian 11.000000
    22         Pau'an 10.000000
    23       Quermian 11.000000
    24         Rodian  6.000000
    25        Skakoan 10.000000
    26      Sullustan  9.000000
    27     Tholothian 10.000000
    28        Togruta  8.000000
    29          Toong 14.000000
    30      Toydarian  5.000000
    31     Trandoshan  5.000000
    32        Twi'lek 11.000000
    33     Vulptereen  8.000000
    34        Wookiee  8.000000
    35          Xexto  7.000000
    36 Yoda's species  4.000000
    37         Zabrak  9.500000

## Вывод

В результате выполнения практической работы были освоены базовые
операции обработки данных в R с использованием функций пакета dplyr.
