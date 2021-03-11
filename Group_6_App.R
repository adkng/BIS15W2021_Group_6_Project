library(tidyverse)
library(janitor)
library(here)
library(skimr)
library(ggthemes)
library(readr)
library(shiny)
library(shinydashboard)
if (!require("tidyverse")) install.packages('tidyverse')

nectar_perflower <- read_csv(here("potential_datasets", "AgriLand_Nectar_perflower.csv"))
nectar_perflower_clean <- nectar_perflower %>% 
  clean_names() %>% 
  mutate_all(tolower) %>% 
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
nectar_perflower_clean$sugar_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugar_in_micrograms_flower_24h)
nectar_perflower_clean$sugarmax_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugarmax_in_micrograms_flower_24h)
nectar_perflower_clean$hum <- as.numeric(nectar_perflower_clean$hum)
nectar_perflower_clean$temp <- as.numeric(nectar_perflower_clean$temp)
nectar_perflower_clean$year <- as.numeric(nectar_perflower_clean$year)




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
      geom_bar(position="dodge", color="black", fill="chartreuse4", alpha=0.7) +
      ggthemes::theme_pander(base_size = 13) +
      labs(y = "Nectar Produced (mg)")
  })
  
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)

