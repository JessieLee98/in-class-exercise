library(shiny)
library(tidyverse)
library(plotly)

exam<-read_csv("data/Exam_data.csv")
  
ui <- fluidPage(
  titlePanel("People Profile"),
  mainPanel(
    plotlyOutput("race"),
    plotlyOutput("gender")
  )
)


server <- function(input, output) {
  output$race <- renderPlotly({
    p <- exam %>%
      plot_ly(x = ~RACE)
  })
  
  output$gender <- renderPlotly({
    d <- event_data("plotly_click")
    if(is.null(d)) return(NULL)
    
    p <- exam %>%
      filter(RACE %in% d$x) %>%
      ggplot(aes(x = GENDER))+
      geom_bar()
    ggplotly(p) %>%
      layout(xaixs = list(title = d$x))
  })
}


shinyApp(ui = ui, server = server)
