library(tidyverse)
library(shiny)

d = read_csv('who_covid.csv')

numeric_variables = d %>% select_if(is.numeric) %>% names()

variables_names = names(d)

ui <- fluidPage(
  
  titlePanel("Covid19 Data by Countries"),
  sidebarLayout(
    
    # Side Panel for reading inputs
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a statistics",
        choices = numeric_variables, selected = "Cumulative_cases"
      ),
      
      selectInput(
        inputId = "selected_country", 
        label = "Select Country",
        choices = unique(d$Country), selected="United States of America"),
      
      dateRangeInput(inputId = "date", 
                     strong("Date range"), 
                     start = "2020-01-01", end = "2021-11-25",
                     min = "2020-01-01", max = "2021-11-25"),
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  output$show_plot <- renderPlot({
    v1 = input$var1
    v2 = input$var2
    country = input$selected_country
    library(ggplot2)
    
    d <- d %>% filter(Country %in% country, Date_reported>input$date[1],  Date_reported<input$date[2]) 
    
    r <- d %>% ggplot(aes(x = Date_reported, y = d[[v1]], color = Country))+
      geom_point()+
      labs(x = 'Date', y = v1)
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)