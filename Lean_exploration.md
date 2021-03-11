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
## ✓ tibble  3.1.0     ✓ dplyr   1.0.4
## ✓ tidyr   1.1.3     ✓ stringr 1.4.0
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
library(shiny)
library(shinydashboard)
```

```
## 
## Attaching package: 'shinydashboard'
```

```
## The following object is masked from 'package:graphics':
## 
##     box
```

```r
options(scipen=999)
if (!require("tidyverse")) install.packages('tidyverse')
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
nectar_perflower
```

```
## # A tibble: 3,303 x 19
##    species  location  habitat id       bagging rinsing bagging.date bagging.hour
##    <chr>    <chr>     <chr>   <chr>    <chr>   <chr>   <chr>        <time>      
##  1 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  2 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  3 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  4 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  5 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  6 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  7 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  8 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
##  9 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
## 10 Acer ps… clifton-… grassl… 2012-Ac… bag     Y       3/5/12       16:00       
## # … with 3,293 more rows, and 11 more variables: collection.date <chr>,
## #   collection.hour <time>, year <dbl>, temp <dbl>, hum <dbl>, plant.no <dbl>,
## #   flower.no <dbl>, flower.age <chr>, flower.sex <chr>,
## #   sugar in micrograms/flower/24h <dbl>,
## #   sugarmax in micrograms/flower/24h <dbl>
```


```r
nectar_perflower_clean <- nectar_perflower %>% 
  clean_names() %>% 
  mutate_all(tolower) %>% 
  separate(species,
           into = c("genus", "epithet")) %>% 
  separate(bagging_date,
           into = c("bagging_day", "bagging_month", "bagging_year"),
           sep = "/") %>% 
  separate(collection_date,
           into = c("collection_day", "collection_month", "collection_year"),
           sep = "/") %>% 
  separate(habitat,
           into = c("habitat_class", "habitat_specific"),
                    sep = "-",
                    extra = "merge") %>% 
  separate(collection_hour,
           into = c("collection_hour", "collection_minute", "collection_second"),
           sep = ":") %>% 
  separate(bagging_hour,
           into = c("bagging_hour", "bagging_minute", "bagging_second"),
           sep = ":")
```

```
## Warning: Expected 2 pieces. Additional pieces discarded in 245 rows [477, 478,
## 479, 480, 481, 482, 483, 484, 485, 486, 487, 488, 489, 490, 491, 492, 493, 494,
## 495, 496, ...].
```

```
## Warning: Expected 2 pieces. Missing pieces filled with `NA` in 1561 rows [1, 2,
## 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, ...].
```




```r
nectar_perflower_clean$sugar_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugar_in_micrograms_flower_24h)
nectar_perflower_clean$sugarmax_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugarmax_in_micrograms_flower_24h)
nectar_perflower_clean$hum <- as.numeric(nectar_perflower_clean$hum)
nectar_perflower_clean$temp <- as.numeric(nectar_perflower_clean$temp)
nectar_perflower_clean$year <- as.numeric(nectar_perflower_clean$year)
```



```r
nectar_perflower_clean
```

```
## # A tibble: 3,303 x 29
##    genus epithet  location habitat_class habitat_specific id     bagging rinsing
##    <chr> <chr>    <chr>    <chr>         <chr>            <chr>  <chr>   <chr>  
##  1 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  2 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  3 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  4 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  5 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  6 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  7 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  8 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
##  9 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
## 10 acer  pseudop… clifton… grassland     <NA>             2012-… bag     y      
## # … with 3,293 more rows, and 21 more variables: bagging_day <chr>,
## #   bagging_month <chr>, bagging_year <chr>, bagging_hour <chr>,
## #   bagging_minute <chr>, bagging_second <chr>, collection_day <chr>,
## #   collection_month <chr>, collection_year <chr>, collection_hour <chr>,
## #   collection_minute <chr>, collection_second <chr>, year <dbl>, temp <dbl>,
## #   hum <dbl>, plant_no <chr>, flower_no <chr>, flower_age <chr>,
## #   flower_sex <chr>, sugar_in_micrograms_flower_24h <dbl>,
## #   sugarmax_in_micrograms_flower_24h <dbl>
```

#### Note: NA's are removed

#### Biological Analyses
######How do time of day, flower age, and flower sex affect the amount of nectar per flower?



```r
nectar_perflower_clean %>% 
  filter(sugar_in_micrograms_flower_24h!="NA", flower_sex!="NA") %>% 
  select(flower_sex, sugar_in_micrograms_flower_24h) %>% 
  group_by(flower_sex) %>% 
  summarise(total_nectar = sum(sugar_in_micrograms_flower_24h)) %>% 
  arrange(desc(total_nectar))
```

```
## # A tibble: 3 x 2
##   flower_sex total_nectar
##   <chr>             <dbl>
## 1 h               657428.
## 2 m               158550.
## 3 f                43076.
```




```r
nectar_perflower_clean %>% 
  filter(sugar_in_micrograms_flower_24h!="NA", flower_age!="NA") %>% 
  select(flower_age, sugar_in_micrograms_flower_24h) %>% 
  group_by(flower_age) %>% 
  summarise(total_nectar = sum(sugar_in_micrograms_flower_24h)) %>% 
  arrange(desc(total_nectar))
```

```
## # A tibble: 3 x 2
##   flower_age total_nectar
##   <chr>             <dbl>
## 1 m               596967.
## 2 o               190714.
## 3 y                70322.
```




```r
nectar_perflower_clean %>% 
  filter(sugar_in_micrograms_flower_24h!="NA", collection_hour!="NA") %>% 
  select(collection_hour, sugar_in_micrograms_flower_24h) %>% 
  group_by(collection_hour) %>% 
  summarise(total_nectar = sum(sugar_in_micrograms_flower_24h)) %>% 
  arrange(desc(total_nectar))
```

```
## # A tibble: 10 x 2
##    collection_hour total_nectar
##    <chr>                  <dbl>
##  1 11                  283515. 
##  2 12                  167021. 
##  3 10                  120885. 
##  4 13                   94046. 
##  5 14                   80439. 
##  6 15                   57420. 
##  7 09                   13939. 
##  8 16                    9660. 
##  9 08                    8989. 
## 10 17                      45.0
```
 



```r
ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Why are Insect Pollinator Communities Declining?",
                  titleWidth = 450),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Variables Affecting Nectar Production", width = 3,
  selectInput("x", "Select Variable:", choices = c("year", "temp", "hum", "collection_hour", "flower_sex", "flower_age"),
              selected = "year"),
  hr(),
      helpText("Reference: Baude M, Kunin W, Boatman N, Davies N, Gillespie M, Morton D, Smart S, Memmott J. Historical nectar assessment reveals the fall and rise of floral resources in Britain. 2016.")
  ), 
  box(title = "Amount of Nectar Produced", width = 7,
  plotOutput("plot", width = "600px", height = "500px")
  ) 
  ) 
  ) 
)

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
  nectar_perflower_clean %>%
    filter(flower_sex!="NA", flower_age!="NA", sugar_in_micrograms_flower_24h!="NA", sugar_in_micrograms_flower_24h!="0.0000000",  year!="NA", temp!="NA", hum!="NA", collection_hour!="NA") %>%
    ggplot(aes_string(x=input$x)) + 
    geom_bar(position="dodge", color="black", fill="olivedrab4", alpha=0.7) +
    ggthemes::theme_pander(base_size = 13) +
    labs(y = "Nectar Produced (mg)")
  })
  
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}




