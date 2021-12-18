library(tidyverse)
library(shiny)
library(DT)

ui <- fluidPage(
  
  titlePanel("Question 9"),
  
  sidebarLayout(
    
    sidebarPanel(
      fileInput('f1', label = 'Upload data for visualization', accept = '.csv'),
      
      selectInput('v1', label='Select Numeric Variable #1', choices=''),
      selectInput('v2', label='Select Numeric Variable #2', choices=''),
      selectInput('v3', label='Select Categorical Variable', choices='')
      
      
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      plotOutput(outputId = 'show_plot')
    )
  )
)

# server
server <- function(input, output, session) {
  
  myData <- reactive({
    inFile = input$f1
    if (is.null(inFile)) return(NULL)
    data <- read.csv(inFile$datapath, header = TRUE)
    data
  }
  )
  
  output$show_plot <- renderPlot({
    
    inFile = input$f1
    if (!is.null(inFile))
    {
      d <- read.csv(inFile$datapath, header = TRUE)
      v1 = input$v1
      v2 = input$v2
      v3 = input$v3
      library(ggplot2)
      r = ggplot(d, aes(x = d[[v1]], y=d[[v2]],  color = as.factor(d[[v3]])))+
        geom_point()+
        labs(x = v1, y=v2, color = v3)
      return(r)
    }
    
    else{
      return(NULL)
    }
  })
  
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE) 
    variables_numeric = data %>% select_if(is.numeric) %>% names()
    updateSelectInput(session, 'v1', choices = variables_numeric)}
  )
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE)
    variables_numeric = data %>% select_if(is.numeric) %>% names()
    updateSelectInput(session, 'v2', choices = variables_numeric)}
  )
  
  observeEvent(input$f1,{ 
    inFile = input$f1
    data <- read.csv(inFile$datapath, header = TRUE)
    variables_category = data %>% select_if(is.character) %>% names()
    updateSelectInput(session, 'v3', choices = variables_category)}
  )
  
}


shinyApp(ui = ui, server = server)