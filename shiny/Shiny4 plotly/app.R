library(shiny)
library(tidyverse)
library(plotly)

exam<-read_csv("data/Exam_data.csv")
  
ui <- fluidPage(
  titlePanel("People Profile"),
  mainPanel(
    plotlyOutput("race")
  )
)


server <- function(input, output) {
  output$race <- renderPlotly({
    p <- ggplot(data=exam,
                aes(x=RACE))+
      geom_bar()
    ggplotly(p)
  })
}


shinyApp(ui = ui, server = server)
