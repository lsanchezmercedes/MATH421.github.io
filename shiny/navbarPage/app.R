library(tidyverse)
library(shiny)

d = read_csv('adult_modified.csv')

# get variable names
variables_names = names(d)

# get names of numeric variables
variables_numeric = d %>% select_if(is.numeric) %>% names

# get names of categorical variables
variables_category = d %>% select_if(is.character) %>% names

ui <- navbarPage("Navigation !",
                 tabPanel("Numeric Variables",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput(
                                inputId ="var1",
                                label = "Select a Numeric Variables",
                                choices = variables_numeric, selected = "age"
                              )
                            ),
                            
                            mainPanel(
                              plotOutput(outputId = 'show_plot')
                            )
                          )
                 ),
                 
                 
                 tabPanel("Categorical Vairables",
                          
                          sidebarLayout(
                            sidebarPanel(
                              
                              selectInput(
                                inputId ="var2",
                                label = "Select a Categorical Variables",
                                choices = variables_category, selected = "Sex"
                              )
                            ),
                            
                            mainPanel(
                              plotOutput(outputId = 'show_plot2')
                            )
                          )
                 )
)

# server is a function! 
server <- function(input, output) {
  
  
  output$show_plot <- renderPlot({
    
    v1 = input$var1
    
    library(ggplot2)
    
    ggplot(d, aes(x = d[[v1]]))+
      geom_density()+
      labs(x = v1)
    
    
  })
  
  output$show_plot2 <- renderPlot({
    
    v2 = input$var2
    
    library(ggplot2)
    
    ggplot(d, aes(x = d[[v2]]))+
      geom_bar()+
      labs(x = v2)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)