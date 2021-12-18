library(tidyverse)
library(shiny)

d = read_csv('adult_modified.csv')

# get variable names
variables_names = names(d)


# get names of categorical variables
variables_category = d %>% select_if(is.character) %>% names



ui <- fluidPage(
  
  titlePanel("Density Plot for Titanic Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select Categorical Variable #1",
        choices = variables_category, selected = "sex"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select Categorical Variable #2",
        choices = variables_category,
        selected = "occupation"
      ),
      
      checkboxGroupInput(inputId = "race_category", label = "Select Race",
                         choices = names(table(d$race)), inline = TRUE),
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    v2 = input$var2
    
    d <- d %>% filter(race %in% input$race_category)
    
    library(ggplot2)
    
    ggplot(d, aes(x = as.factor(d[[v1]]), color = as.factor(d[[v2]])))+
      geom_bar()+
      labs(x = v1, color = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)