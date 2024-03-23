library(shiny)
library(tidyverse)
library(tmap)
library(dplyr)


mpsz_SES_filtered <- read_rds("data/mpsz_SES_filtered.rds")



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
    tabPanel("Overview",
             sidebarLayout(
               sidebarPanel(
                 # Add inputs for Tab 1
               ),
               mainPanel(
                 # Add outputs for Tab 1
               )
             )
    ),
    tabPanel("By Dwelling Type",
             sidebarLayout(
               sidebarPanel(
                 # Add inputs for Tab 2
               ),
               mainPanel(
                 # Add outputs for Tab 2
               )
             )
    ),
    tabPanel("By Region",
             sidebarLayout(
               sidebarPanel(
                 selectInput("dwelling", "Dwelling Type:", choices = unique(mpsz_SES_filtered$dwelling_type)),
                 selectInput("year", "Year:", choices = unique(mpsz_SES_filtered$year)),
                 selectInput("month", "Month:", choices = unique(mpsz_SES_filtered$month))
               ),
               mainPanel(
                 uiOutput("map")
               )
             )
    ),
    tabPanel("Time Series",
             sidebarLayout(
               sidebarPanel(
                 # Add inputs for Tab 4
               ),
               mainPanel(
                 # Add outputs for Tab 4
               )
             )
    )
  )
)

# Define server logic
# Define server logic
server <- function(input, output) {
  filtered_data <- reactive({
    filtered <- mpsz_SES_filtered
    if (!is.null(input$dwelling)) {
      filtered <- filtered %>% filter(dwelling_type == input$dwelling)
    }
    if (!is.null(input$year)) {
      filtered <- filtered %>% filter(year == input$year)
    }
    if (!is.null(input$month)) {
      filtered <- filtered %>% filter(month == input$month)
    }
    return(filtered)
  })
  
  output$map <- renderUI({
    req(filtered_data())
    if (nrow(filtered_data()) == 0) {
      return(NULL)  # Return NULL if filtered data is empty
    }
    
    tmap_mode("view")
    tmap_options(check.and.fix = TRUE)
    
    elecmap <- tm_shape(filtered_data())+
      tm_fill("kwh_per_acc", 
              style = "quantile", 
              palette = "Blues",
              title = "Electricity Consumption in kwh_per_acc") +
      tm_layout(main.title = "Total Household Electricity Consumption by Household in 2023",
                main.title.position = "center",
                main.title.size = 1.2,
                legend.height = 0.45, 
                legend.width = 0.35,
                frame = TRUE) +
      tm_borders(alpha = 0.5) +
      tm_scale_bar() +
      tm_grid(alpha =0.2)
    
    print(elecmap)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
