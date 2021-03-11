library(tidyverse)
library(RColorBrewer)
library(paletteer)
library(janitor)
library(here)
library(skimr)
library(ggthemes)
library(naniar)
library(readr)
library(shiny)
library(shinydashboard)
library(stringr)


nectar_perflower <- read_csv(here("potential_datasets", "AgriLand_Nectar_perflower.csv"))

nectar_perflower_clean <- nectar_perflower %>% 
  clean_names() %>% 
  mutate_all(tolower)

nectar_perflower_clean$sugar_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugar_in_micrograms_flower_24h)
nectar_perflower_clean$sugarmax_in_micrograms_flower_24h <- as.numeric(nectar_perflower_clean$sugarmax_in_micrograms_flower_24h)
nectar_perflower_clean$hum <- as.numeric(nectar_perflower_clean$hum)
nectar_perflower_clean$temp <- as.numeric(nectar_perflower_clean$temp)

ui <- dashboardPage(skin = "black",
                    dashboardHeader(title = "Nectar Collection by Habitat",
                                    titleWidth = 600),
                    dashboardSidebar(disable = T),
                    dashboardBody(
                      fluidRow(
                        box(title = "Plot Options", width = 3,
                            selectInput("x", "Select a Habitat:", choices = c("allotments", "bog", "cemetery-park", "coastland-grassland", "cultivated farmland", "dwarf shrub heath", "garden", "grassland", "grassland-carpark", "grassland-coastland", "grassland-coastland", "grassland-hedgerow", "grassland-nature reserve", "grassland-park", "grassland-wetland", "grassland-woodland", "heathland", "heathland-mooseland", "hedgerow", "hedgerow-woodland", "managed grassland-park", "moorland", "pavement", "pond", "pot", "rock", "street", "wetland", "woodland", "woodland-carpark", "woodland-nature reserve"), 
                                        selected = "wetland"),
                            selectInput("y", "Select a Year:", choices = c("2011", "2012"),
                                        selected = "2011"),
                            sliderInput("pointsize", "Select the Point Size", min = 1, max = 5, value = 2, step = 0.5),
                            hr(),
                            helpText("Reference: Baude M, Kunin W, Boatman N, Davies N, Gillespie M, Morton D, Smart S, Memmott J. 
               Historical nectar assessment reveals the fall and rise of floral resources in Britain. 2016.")
                        ), # close the first box
                        box(title = "Nectar Collection by Habitat", width = 6,
                            plotOutput("plot", width = "600px", height = "500px")
                        ) # close the second box
                      ) # close the row
                    ) # close the dashboard body
) # close the ui

server <- function(input, output, session) { 
  
  output$plot <- renderPlot({
    nectar_perflower_clean %>%
      filter(habitat==input$x & year ==input$y) %>% 
      ggplot(aes_string(x = "species", y = "sugar_in_micrograms_flower_24h", fill = "species")) +
      geom_point(size=input$pointsize, alpha=0.8) +
      geom_col()+
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(y = "Nectar Collected (micrograms)",
           x = "Species")
  })
  
  # stop the app when we close it
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)