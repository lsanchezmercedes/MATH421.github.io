library(tidyverse)
library(shiny)

d = read_csv('all_stocks_5yr.csv')

ui <- fluidPage(
  
  titlePanel("Stock Data"),
  sidebarLayout(
    
    # Side Panel for reading inputs
    sidebarPanel(
      radioButtons(inputId = "increase", 
                   label = h3("Select if Increase:"),
                   choices = c("Yes" = "TRUE",
                               "No" = "FALSE"),
                   selected = 'Yes'),
      
      selectInput(
        inputId = "selected_company", 
        label = "Select Company",
        choices = unique(d$Name), selected="AAL"),
      
      dateRangeInput(inputId = "date", 
                     strong("Date range"), 
                     start = "2018-01-02", end = "2018-02-08",
                     min = "2013-02-08", max = "2018-02-08"),
      
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
    company = input$selected_company
    library(ggplot2)
    
    d <- d %>% filter(Name %in% company, date>input$date[1],  date<input$date[2],
                      Increase==input$increase) 
    
    r <- d %>% ggplot(aes(x = date, y = d[["close"]]))+
      geom_bar()+
      labs(x = 'Date', y = 'Volume')
    
    return(r)
    
    
  })
  
}
# app
shinyApp(ui = ui, server = server)