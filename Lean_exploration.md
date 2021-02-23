---
title: "Lean_data_exploration_2/15"
output: 
  html_document: 
    keep_md: yes
---




```r
library(tidyverse)
```

```
## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──
```

```
## ✓ ggplot2 3.3.3     ✓ purrr   0.3.4
## ✓ tibble  3.0.6     ✓ dplyr   1.0.4
## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
## ✓ readr   1.4.0     ✓ forcats 0.5.1
```

```
## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
## x dplyr::filter() masks stats::filter()
## x dplyr::lag()    masks stats::lag()
```

```r
library(RColorBrewer)
library(paletteer)
library(janitor)
```

```
## 
## Attaching package: 'janitor'
```

```
## The following objects are masked from 'package:stats':
## 
##     chisq.test, fisher.test
```

```r
library(here)
```

```
## here() starts at /Users/alvarezlean/Desktop/BIS15L Project/BIS15W2021_Group_6_Project/BIS15W2021_Group_6_Project
```

```r
library(skimr)
library(ggthemes)
library(naniar)
```

```
## 
## Attaching package: 'naniar'
```

```
## The following object is masked from 'package:skimr':
## 
##     n_complete
```

```r
library(readr)
options(scipen=999)
```

This is a preloading of the data. The original publication can be found [here](https://doi.org/10.1038/nature16532) and it was also mentioned in [this paper.](https://doi.org/10.1111/2041-210X.12779).

```r
nectar_perflower <- read_csv(here("potential_datasets", "AgriLand_Nectar_perflower.csv"))
```

```
## 
## ── Column specification ────────────────────────────────────────────────────────
## cols(
##   species = col_character(),
##   location = col_character(),
##   habitat = col_character(),
##   id = col_character(),
##   bagging = col_character(),
##   rinsing = col_character(),
##   bagging.date = col_character(),
##   bagging.hour = col_time(format = ""),
##   collection.date = col_character(),
##   collection.hour = col_time(format = ""),
##   year = col_double(),
##   temp = col_double(),
##   hum = col_double(),
##   plant.no = col_double(),
##   flower.no = col_double(),
##   flower.age = col_character(),
##   flower.sex = col_character(),
##   `sugar in micrograms/flower/24h` = col_double(),
##   `sugarmax in micrograms/flower/24h` = col_double()
## )
```


```r
nectar_perflower <- janitor::clean_names(nectar_perflower)
```


```r
nectar_perflower
```

```
## # A tibble: 3,303 x 19
##    species location habitat id    bagging rinsing bagging_date bagging_hour
##    <chr>   <chr>    <chr>   <chr> <chr>   <chr>   <chr>        <time>      
##  1 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  2 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  3 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  4 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  5 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  6 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  7 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  8 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
##  9 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
## 10 Acer p… clifton… grassl… 2012… bag     Y       3/5/12       16:00       
## # … with 3,293 more rows, and 11 more variables: collection_date <chr>,
## #   collection_hour <time>, year <dbl>, temp <dbl>, hum <dbl>, plant_no <dbl>,
## #   flower_no <dbl>, flower_age <chr>, flower_sex <chr>,
## #   sugar_in_micrograms_flower_24h <dbl>,
## #   sugarmax_in_micrograms_flower_24h <dbl>
```



```r
nectar_per_flower_clean <- nectar_perflower %>% 
  separate(species,
           into = c("genus", "epithet")) %>% 
  separate(bagging_date,
           into = c("bagging_day", "bagging_month", "bagging_year"),
           sep = "/")
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 245 rows [477, 478,
## 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494,
## 495, 496, ...].
```

```r
nectar_per_flower_clean
```

```
## # A tibble: 3,303 x 22
##    genus epithet location habitat id    bagging rinsing bagging_day
##    <chr> <chr>   <chr>    <chr>   <chr> <chr>   <chr>   <chr>      
##  1 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  2 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  3 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  4 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  5 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  6 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  7 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  8 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
##  9 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
## 10 Acer  pseudo… clifton… grassl… 2012… bag     Y       3          
## # … with 3,293 more rows, and 14 more variables: bagging_month <chr>,
## #   bagging_year <chr>, bagging_hour <time>, collection_date <chr>,
## #   collection_hour <time>, year <dbl>, temp <dbl>, hum <dbl>, plant_no <dbl>,
## #   flower_no <dbl>, flower_age <chr>, flower_sex <chr>,
## #   sugar_in_micrograms_flower_24h <dbl>,
## #   sugarmax_in_micrograms_flower_24h <dbl>
```


# Lean's analyses
How do time, flower age, and flower sex affect the amount of nectar per flower?


#### Note: NA's are removed

```r
nectar_per_flower_clean %>% 
  filter(flower_age!="NA", sugar_in_micrograms_flower_24h!="NA") %>% 
  ggplot(aes(x=flower_age, y=sugar_in_micrograms_flower_24h))+
  geom_col()+
  labs(title = "How Flower Age Affects the Amount of Nectar", x = "Flower Age", y = "Sugar Collected by Flower in 24 Hours (mg)") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
```

![](Lean_exploration_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

Flowers that are not too young or not too old produce the most nectar. 





```r
nectar_per_flower_clean %>% 
  filter(flower_sex!="NA", sugar_in_micrograms_flower_24h!="NA") %>% 
  group_by(flower_sex) %>% 
  ggplot(aes(x=flower_sex, y=sugar_in_micrograms_flower_24h))+
  geom_col()+
  labs(title = "How Flower Sex Affects the Amount of Nectar", x = "Flower Sex", y = "Sugar Collected by Flower in 24 Hours (mg)") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
```

![](Lean_exploration_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

Flowers that are hermaphrodite (have both male and female sex organs) produce the most nectar. 




```r
nectar_per_flower_clean %>% 
  filter(collection_hour!="NA", sugar_in_micrograms_flower_24h!="NA") %>% 
  ggplot(aes(x=collection_hour, y=sugar_in_micrograms_flower_24h))+
  geom_col()+
  labs(title = "What Time Gives the Most Amount of Nectar?", x = "Collection Hour", y = "Sugar Collected by Flower in 24 Hours (mg)") +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12))
```

![](Lean_exploration_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Flowers collected from about 10:00 AM to 12:00 PM produce the most nectar. Perhaps this is because it is generally quite sunny around this time. Therefore, pollinators likely come to the flowers most at around this time. 


```r
nectar_perflower %>% 
  filter(species!="NA", sugar_in_micrograms_flower_24h!="NA") %>% 
  group_by(species) %>% 
  summarise(mean_nectar_per_species=mean(sugar_in_micrograms_flower_24h)) %>% 
  arrange(desc(mean_nectar_per_species))
```

```
## # A tibble: 175 x 2
##    species                mean_nectar_per_species
##    <chr>                                    <dbl>
##  1 Impatiens glandulifera                   5112.
##  2 Iris pseudacorus                         3413.
##  3 Gladiolus sp.                            3142.
##  4 Rubus fruticosus agg.                    1893.
##  5 Lonicera periclymenum                    1864.
##  6 Calystegia sepium                        1801.
##  7 Digitalis purpurea                       1274.
##  8 Rhododendron ponticum                    1257.
##  9 Vaccinium myrtillus                      1058.
## 10 Symphytum officinale                     1045.
## # … with 165 more rows
```


"There is considerable concern over declines in insect pollinator communities and potential impacts on the pollination of crops and wildflowers."

### Main Question:
-Why are insect pollinator communities declining?

### Possible Analyses
1. Which habitats have the least and most amount of nectar per flower? Why?
2. Which months have the least and most amount of nectar per flower? Why?
2. How do temperature and humidity affect the amount of nectar per flower?
3. How do age, sex, and collection hour affect the amount of nectar per flower?
4. Why is there a discrepancy between sugar and sugar max?

-Which combinations of factors result in the least and most amount of nectar per flower? 
  (amount of nectar is correlated to amount of pollinators bc flowers use nectar to attract pollinators)
  
-what factors possibly contribute to the decline of pollinators?

-what steps can we take to minimize the loss of pollinator communities?


#### changes
-add hypotheses
-change each x axis numbers manually
