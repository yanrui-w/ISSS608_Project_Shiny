pacman::p_load(shiny,tidyverse)

exam <- read_csv("data/Exam_data.csv")


library(shiny)
library(ggplot2)

# Define UI for application that includes tabs
ui <- fluidPage(
  titlePanel("Electricity Consumption In Singapore"),
  
  tags$head(
    tags$style(HTML("
      /* Add color to the title panel */
      .titlePanel {
        background-color: #3366CC; /* You can use any color code or name */
        color: white; /* Text color */
      }

      /* Add color to the tabset panel */
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        background-color: #3366CC; /* Background color of active tab */
        color: white; /* Text color of active tab */
      }

      /* Add color to the content area */
      .tab-content {
        background-color: #F0F0F0; /* Background color of content area */
      }
    "))
  ),
  
  tabsetPanel(
    tabPanel("Overview", "Content for Tab 1"),
    tabPanel("By Dwelling Type", "Content for Tab 2"),
    tabPanel("By Region", "Content for Tab 3"),
    
    # Tab for Pupils examination results
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 selectInput(inputId = "variable",
                             label = "Subject:",
                             choices = c("English" = "ENGLISH",
                                         "Maths" = "MATHS",
                                         "Science" = "SCIENCE"),
                             selected = "ENGLISH"),
                 sliderInput(inputId = "bins",
                             label = "Number of Bins",
                             min = 5,
                             max = 20,
                             value= 10)
               ),
               mainPanel(plotOutput("distPlot"))
             )
    )
  )
)

# Define server logic for histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Sample data for demonstration
    set.seed(123)
    exam <- data.frame(
      ENGLISH = rnorm(100, mean = 70, sd = 10),
      MATHS = rnorm(100, mean = 65, sd = 8),
      SCIENCE = rnorm(100, mean = 75, sd = 12)
    )
    
    # Histogram plot
    ggplot(exam, aes(x = exam[[input$variable]])) +
      geom_histogram(bins = input$bins,
                     color = "black",  
                     fill = "light blue") +
      labs(x = input$variable, y = "Frequency") +
      theme_minimal()
  }) 
}

# Run the application 
shinyApp(ui = ui, server = server)
