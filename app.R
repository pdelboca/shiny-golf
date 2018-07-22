library(shiny)
library(dygraphs)
library(xts)
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)

source("data/golf_data.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Personal Golf Statistics"),
  sidebarLayout(
    sidebarPanel(
      helpText("Select Filters:"),
      dateRangeInput("date", 
                "Date"),
      selectInput("course",
                  "Golf Course",
                  c("All", cards$course))
      
    ,width = 2),
    mainPanel(
      fluidRow(
        column(dygraphOutput("historicScores"), width = 8),
        column(tableOutput("historicTable"), width = 4)
      )
    , width = 10)
  )
)

# Define server logic ----
server <- function(input, output) {
  
  scores <- reactive({
    cards %>%
    group_by(date, course) %>% # TODO: Review if multiple matchs in 1 day
    filter(if(input$course != "All") course == input$course else TRUE) %>%
    summarise(total = sum(shots, na.rm = TRUE)) %>%
    ungroup()
    }) 
  
  output$historicScores <- renderDygraph({
    scores() %>% 
    xts(x = .$total, order.by = .$date) %>%
    dygraph(main = "Total Scores per Date (for 9 Holes)") %>%
      dySeries("V1", label = "Score") %>%
      dyAxis("y", label = "Score (Total Hits)", valueRange = c(0, 100)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE, drawPoints = TRUE, pointSize = 3)
  })
  
  output$historicTable <- renderTable(
    scores() %>% mutate(date = as.character(date)) # Needed due to a bug
    )
}

# Run the app ----
shinyApp(ui = ui, server = server)