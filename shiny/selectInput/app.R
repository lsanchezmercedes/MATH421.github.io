library(tidyverse)
library(shiny)

d = read_csv('titanic.csv')
# drop some columns
d <- d %>% select(-Name, -PassengerId, -Ticket)

# convert categorical variables to character type
d <- d %>% mutate_at(c('Survived', 'Pclass'), as.character)

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
        label = "Select a Categorical Variables # 1",
        choices = variables_category, selected = "Sex"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables #2",
        choices = variables_category,
        selected = "Pclass"
      )
      
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
    d = read_csv('titanic.csv')
    v1 = input$var1
    v2 = input$var2
    
    
    library(ggplot2)
    
    r = ggplot(d, aes(x = as.factor(d[[v1]]), color = as.factor(d[[v2]])))+
      geom_bar()+
      labs(x = v1, color = v2)
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)