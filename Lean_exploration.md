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
nectar_per_flower <- read_csv(here("potential_datasets", "AgriLand_Nectar_perflower.csv"))
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
nectar_per_flower <- janitor::clean_names(nectar_per_flower)
nectar_per_flower
```

```
## # A tibble: 3,303 x 19
##    species  location  habitat id       bagging rinsing bagging_date bagging_hour
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
## # … with 3,293 more rows, and 11 more variables: collection_date <chr>,
## #   collection_hour <time>, year <dbl>, temp <dbl>, hum <dbl>, plant_no <dbl>,
## #   flower_no <dbl>, flower_age <chr>, flower_sex <chr>,
## #   sugar_in_micrograms_flower_24h <dbl>,
## #   sugarmax_in_micrograms_flower_24h <dbl>
```


```r
nectar_per_flower_clean <- nectar_per_flower %>% 
  mutate_all(funs(str_replace(., "2011", "11"))) %>% 
  mutate_all(funs(str_replace(., "2012", "12"))) %>% 
  separate(bagging_date,
           into = c("bagging_day", "bagging_month", "bagging_year"),
           sep = "/") %>% 
  separate(collection_date,
           into = c("collection_day", "collection_month", "collection_year"),
           sep = "/")
```

```
## Warning: `funs()` was deprecated in dplyr 0.8.0.
## Please use a list of either functions or lambdas: 
## 
##   # Simple named list: 
##   list(mean = mean, median = median)
## 
##   # Auto named with `tibble::lst()`: 
##   tibble::lst(mean, median)
## 
##   # Using lambdas
##   list(~ mean(., trim = .2), ~ median(., na.rm = TRUE))
```

```r
nectar_per_flower_clean
```

```
## # A tibble: 3,303 x 23
##    species  location  habitat id       bagging rinsing bagging_day bagging_month
##    <chr>    <chr>     <chr>   <chr>    <chr>   <chr>   <chr>       <chr>        
##  1 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  2 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  3 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  4 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  5 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  6 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  7 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  8 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
##  9 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
## 10 Acer ps… clifton-… grassl… 12-Acer… bag     Y       3           5            
## # … with 3,293 more rows, and 15 more variables: bagging_year <chr>,
## #   bagging_hour <chr>, collection_day <chr>, collection_month <chr>,
## #   collection_year <chr>, collection_hour <chr>, year <chr>, temp <chr>,
## #   hum <chr>, plant_no <chr>, flower_no <chr>, flower_age <chr>,
## #   flower_sex <chr>, sugar_in_micrograms_flower_24h <chr>,
## #   sugarmax_in_micrograms_flower_24h <chr>
```





```r
nectar_per_flower$sugar_in_micrograms_flower_24h <- as.numeric(nectar_per_flower$sugar_in_micrograms_flower_24h)
nectar_per_flower$year <- as.numeric(nectar_per_flower$year)
```




# Lean's analyses
How do time of day, flower age, and flower sex affect the amount of nectar per flower?


#### Note: NA's are removed

```r
nectar_per_flower_clean %>% 
  filter(flower_age!="NA", sugar_in_micrograms_flower_24h!="NA") %>% 
  ggplot(aes(x=flower_age, y=sugar_in_micrograms_flower_24h))+
  geom_col()+
  labs(title = "How Flower Age Affects the Amount of Nectar", x = "Flower Age", y = "Sugar Made by Flower in 24 Hours (mg)") +
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

Flowers collected from about 10:00 AM to 2:00 PM produce the most nectar. Perhaps this is because it is generally quite sunny around this time. Therefore, pollinators likely come to the flowers most at around this time. 



```r
ui <- dashboardPage(skin="black",
  dashboardHeader(title = "Why are Insect Pollinator Communities Declining?",
                  titleWidth = 450),
  dashboardSidebar(disable = T),
  dashboardBody(
  fluidRow(
  box(title = "Variable Options", width = 3,
  selectInput("x", "Select Variable:", choices = c("year", "temp", "hum", "collection_hour", "flower_sex", "flower_age"),
              selected = "year"),
  hr(),
      helpText("Reference: Baude M, Kunin W, Boatman N, Davies N, Gillespie M, Morton D, Smart S, Memmott J. Historical nectar assessment reveals the fall and rise of floral resources in Britain. 2016.")
  ), 
  box(title = "Amount of Nectar Produced", width = 6,
  plotOutput("plot", width = "600px", height = "500px")
  ) 
  ) 
  ) 
)

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
  nectar_per_flower_clean %>%
    filter(flower_sex!="NA", flower_age!="NA", sugar_in_micrograms_flower_24h!="NA", year!="NA", temp!="NA", hum!="NA", collection_hour!="NA") %>%
    ggplot(aes_string(x=input$x)) + 
    geom_bar(position="dodge", color="black", fill="pink2", alpha=0.8) +
    theme_light(base_size = 19) +
    labs(y = "Nectar Produced (mg)")
  })
  
  session$onSessionEnded(stopApp)
  }

shinyApp(ui, server)
```

`<div style="width: 100% ; height: 400px ; text-align: center; box-sizing: border-box; -moz-box-sizing: border-box; -webkit-box-sizing: border-box;" class="muted well">Shiny applications not supported in static R Markdown documents</div>`{=html}




