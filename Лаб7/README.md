# Практическая работа 007
gribanovaaalya@yandex.ru

## Название

Использование технологии Yandex Query для анализа данных сетевой
активности

## Цель работы

1.  Изучить возможности технологии Yandex Query для анализа
    структурированных наборов данных
2.  Получить навыки построения аналитического пайплайна для анализа
    данных с помощью сервисов Yandex Cloud
3.  Закрепить практические навыки использования SQL для анализа данных
    сетевой активности в сегментированной корпоративной сети

## Исходные данные

1.  Ноутбук с ОС Windows 10
2.  Rstudio Desktop
3.  Интерпретатор R 4.5.1

``` r
sessionInfo()
```

    R version 4.5.1 (2025-06-13 ucrt)
    Platform: x86_64-w64-mingw32/x64
    Running under: Windows 10 x64 (build 19045)

    Matrix products: default
      LAPACK version 3.12.1

    locale:
    [1] LC_COLLATE=Russian_Russia.utf8  LC_CTYPE=Russian_Russia.utf8   
    [3] LC_MONETARY=Russian_Russia.utf8 LC_NUMERIC=C                   
    [5] LC_TIME=Russian_Russia.utf8    

    time zone: Europe/Moscow
    tzcode source: internal

    attached base packages:
    [1] stats     graphics  grDevices utils     datasets  methods   base     

    loaded via a namespace (and not attached):
     [1] compiler_4.5.1    fastmap_1.2.0     cli_3.6.5         tools_4.5.1      
     [5] htmltools_0.5.8.1 rstudioapi_0.17.1 yaml_2.3.10       rmarkdown_2.30   
     [9] knitr_1.50        jsonlite_2.0.0    xfun_0.53         digest_0.6.37    
    [13] rlang_1.1.6       evaluate_1.0.5   

## Задание

Используя сервис Yandex Query, настроить доступ к данным, хранящимся в
сервисе хранения данных Yandex Object Storage. При помощи
соответствующих SQL запросов ответить на вопросы.

## План выполнения

1.  Проверить доступность данных в Yandex Object Storage
2.  Подключить бакет как источник данных для Yandex Query
3.  Провести анализ данных

## Описание шагов

### Шаг 1

Проверим доступность данных в Yandex Object Storage, перейдя по ссылке:

![](./img/img1.png)

### Шаг 2

Подключим бакет как источник данных для Yandex Query:

#### 1. Создадим соединение для бакета в S3 хранилище:

![](./img/img2.png)

#### 2. Настроим привязку данных:

![](./img/img3.png)

#### 3. Опишем входные данные:

![](./img/img4.png)

#### 4. Убедимся в правильности настроек, сделав аналитический запрос с выводом первых 100 строк:

![](./img/img5.png)

### Шаг 3

Решим следующие задания:

#### 1. Известно, что IP адреса внутренней сети начинаются с октетов, принадлежащих интервалу \[12-14\]. Определите количество хостов внутренней сети, представленных в датасете.

    SELECT
        COUNT(DISTINCT ip) AS hosts_count
    FROM (
        SELECT src AS ip FROM `arrow_binding_gribanova`
        UNION ALL
        SELECT dst AS ip FROM `arrow_binding_gribanova`
    ) 
    WHERE ip LIKE '12.%' OR ip LIKE '13.%' OR ip LIKE '14.%';

![](./img/img6.png)

#### 2. Определите суммарный объем исходящего трафика.

    SELECT
        SUM(bytes) AS out_traf
    FROM
        `arrow_binding_gribanova`
    WHERE
        (src LIKE '12.%' OR src LIKE '13.%' OR src LIKE '14.%')
        AND
        (dst NOT LIKE '12.%' AND dst NOT LIKE '13.%' AND dst NOT LIKE '14.%');

![](./img/img7.png)

#### 3. Определите суммарный объем входящего трафика.

    SELECT
        SUM(bytes) AS in_traf
    FROM
        `arrow_binding_gribanova`
    WHERE
        (src NOT LIKE '12.%' AND src NOT LIKE '13.%' AND src NOT LIKE '14.%')
        AND
        (dst LIKE '12.%' OR dst LIKE '13.%' OR dst LIKE '14.%');

![](./img/img8.png)

## Вывод

В результате выполнения практической работы были изучены возможности
технологии Yandex Query для анализа структурированных наборов данных,
получены навыки построения аналитического пайплайна для анализа данных с
помощью сервисов Yandex Cloud, закреплены практические навыки
использования SQL для анализа данных сетевой активности в
сегментированной корпоративной сети.
