packages <- c('ggiraph', 'plotly', 'shiny',
              'DT', 'patchwork','matrixStats',
              'gganimate', 'tidyverse', 'MASS',
              'readxl', 'gifski', 'gapminder', 
              'treemap', 'treemapify', 'rstantools',
              'rPackedBar', 'lubridate', 'ggstatsplot',
              'shinydashboard')

for (p in packages){
  library(p, character.only=T)
}


WageExpenditureMerged <- read_rds("data/WageExpenditureMerged.rds")
FinancialJournalWithDate <- read_rds("data/FinancialJournalWithDate.rds")

sidebar <- dashboardSidebar(
    sidebarMenu(
      menuItem("Health of Business",tabName = "Health of Business"),
      menuItem("Health of Residents",tabName = "Health of Residents"),
      menuItem("Health of Employers",tabName = "Health of Employers")
    )
)


body <- dashboardBody(
    tabItem(
        tabName = "Health of Residents",
        fluidPage(
            titlePanel("Health of Residents"),
            fluidRow(
                column(
                    width=12,
                    tabsetPanel(
                      tabPanel("Wage and Expenditure Pattern",
                               box(plotOutput("dotplot"),
                                   width = 10)
                               ),
                      tabPanel("Wage/Expenditure v.s. Joviality",
                               box(
                                 plotOutput("scatterplot"),
                                 width = 10
                               )
                      ),
                        tabPanel("ANOVA",
                                 selectInput("variable_y", "Dependent Variable:",
                                             c("Savings" = "Savings",
                                               "Wage" = "Wage",
                                               "Education" = "Education",
                                               "Food" = "Food",
                                               "Shelter" = "Shelter",
                                               "Recreation" = "Recreation")
                                 ),
                                 selectInput("variable_x", "Factor",
                                             c("Education Level"  = "educationLevel",
                                               "Age" = "AgeGroup",
                                               "Joviality" = "JovialityGroup")
                                 ),
                                 box(
                                   plotOutput("boxplot"),
                                   width = 10
                                 )
                        )
                        )
                    )
                )
            )
        )
)

# Put them together into a dashboardPage
ui <- dashboardPage(
    dashboardHeader(title = "Financial Health of Engagement City, Ohio, USA",
                    titleWidth = 500),
    sidebar,
    body
)



server <- function(input, output) { 
  output$scatterplot <- renderPlot({
    d <- highlight_key(WageExpenditureMerged)
    p1 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Savings',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1) +
      coord_cartesian(xlim=c(0,1),ylim=c(0,260000)) 
    p2 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Wage',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1) +
      coord_cartesian(xlim=c(0,1),ylim=c(0,260000))
    p3 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Shelter',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1) +
      coord_cartesian(xlim=c(0,1),ylim=c(0,24000))
    p4 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Food',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1)+
      coord_cartesian(xlim=c(0,1),ylim=c(0,10000))
    p5 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Education',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1)+
      coord_cartesian(xlim=c(0,1),ylim=c(0,1500))
    p6 <- ggplot(data=d,
                 aes_string(x = 'joviality',y = 'Recreation',
                            color = 'educationLevel')) +
      geom_point(size=0.5) +
      geom_smooth(method = lm, size=0.1) +
      coord_cartesian(xlim=c(0,1),ylim=c(0,13000))
    subplot(style(ggplotly(p1), showlegend = FALSE),
            style(ggplotly(p2), showlegend = FALSE),
            style(ggplotly(p3), showlegend = FALSE),
            style(ggplotly(p4), showlegend = FALSE),
            style(ggplotly(p5), showlegend = FALSE),
            style(ggplotly(p6), showlegend = FALSE),
            nrows = 3, margin = 0.05, titleX = TRUE, titleY = TRUE)
  })
  output$boxplot <- renderPlot({
    ggbetweenstats(
      data = WageExpenditureMerged,
      x = input$variable_x, 
      y = input$variable_y,
      type = "p",
      mean.ci = TRUE, 
      pairwise.comparisons = TRUE, 
      pairwise.display = "s",
      p.adjust.method = "fdr",
      messages = FALSE)
  })
}

shinyApp(ui, server)