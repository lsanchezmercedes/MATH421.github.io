library(tidyverse)
library(shiny)

d = read_csv('adult_modified.csv')

# get variable names
variables_names = names(d)

# get names of numeric variables
variables_numeric = d %>% select_if(is.numeric) %>% names

# get names of categorical variables
variables_category = d %>% select_if(is.character) %>% names


ui <- fluidPage(
  
  titlePanel("Density Plot for Titanic Data"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        inputId ="var1",
        label = "Select a Numeric Variables",
        choices = variables_numeric, selected = "hours_per_week"
      ),
      
      selectInput(
        inputId ="var2",
        label = "Select a Categorical Variables",
        choices = variables_category,
        selected = "sex"
      ), 
      
      sliderInput(inputId = "age",
                  "Select Age Range:",
                  min = min(d$age, na.rm=TRUE),
                  max = max(d$age, na.rm=TRUE),
                  value= c(30, 50))
      
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
    
    
    library(ggplot2)
    
    d <- d %>% filter(age>input$age[1], age<input$age[2])
    
    ggplot(d, aes(x = d[[v1]], color = as.factor(d[[v2]])))+
      geom_density()+
      labs(x = v1, color = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)