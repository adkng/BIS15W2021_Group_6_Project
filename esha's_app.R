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


ui <- dashboardPage(
  dashboardHeader(title = "Admissions by year across UC Campuses (2010-2019)"),
  dashboardSidebar(disable = T),
  dashboardBody(
    fluidRow(
      box(title = "Plot Options", width = 3,
          selectInput("x", "Select your campus:", choices = c("Davis", "Berkeley", "Irvine", "Los_Angeles", "Merced", "Riverside", "San_Diego", "Santa_Barbara", "Santa_Cruz"),
                      selected = "Davis"), 
          selectInput("y", "Select the admissions category:", choices = c("Applicants", "Enrollees", "Admits"), selected = "Applicants"),
          selectInput("z", "Select an ethnicity:", choices = c("International", "Unknown", "White", "Asian", "Chicano/Latino", "American Indian", "African American"),
                      selected = "Asian"),
          sliderInput("pointsize", "Select the Point Size", min = 1, max = 18, value = 2, step = 0.5)
      ), 
      box(title = "Admissions across UC Campuses by year", width = 7,
          plotOutput("plot", width = "600px", height = "500px")
      ) 
    ) 
  ) 
) 

server <- function(input, output, session) { 
  output$plot <- renderPlot({
    UC_admit %>%
      filter(campus==input$x & category==input$y & ethnicity==input$z) %>% 
      ggplot(aes(x = academic_yr, y = filtered_count_fr)) + geom_point(size=input$pointsize, alpha=0.8) + geom_col(fill = "tomato") + theme_light(base_size = 18)+ theme(axis.text.x = element_text(angle = 60, hjust= 1)) + labs(x = "Year", y = "# of Individuals")
  })
  session$onSessionEnded(stopApp)
}

shinyApp(ui, server)