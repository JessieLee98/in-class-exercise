library(shiny)
library(tidyverse)

exam <- read_csv("data/Exam_data.csv")


ui <- fluidPage(
  titlePanel("Subject Correlation Analysis"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "yvariable",
                  label = "y Variable:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "ENGLISH"),
      selectInput(inputId = "xvariable",
                  label = "x Variable:",
                  choices = c("English" = "ENGLISH",
                              "Maths" = "MATHS",
                              "Science" = "SCIENCE"),
                  selected = "MATHS"),
      submitButton(text="Apply changes")
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    ggplot(data = exam,
           aes_string(x = input$xvariable,
                      y = input$yvariable)) +
      geom_point()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
