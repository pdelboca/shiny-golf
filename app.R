library(shiny)
library(dygraphs)
library(xts)

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
                  c("Alta Gracia Golf Club", "San Miguel"))
      
    ,width = 2),
    mainPanel(
      fluidRow(
        column(dygraphOutput("historicScores"), width = 10),
        column(tableOutput("historicTable"), width = 2)
      )
    , width = 8)
  )
)

# Define server logic ----
server <- function(input, output) {
  scores <- cards %>%
    group_by(Fecha) %>%
    summarise(Total = sum(Shots, na.rm = TRUE)) 
  
  scoresTS <- scores %>%
    xts(x = .$Total, order.by = .$Fecha)
  
  output$historicScores <- renderDygraph({
    dygraph(scoresTS, main = "Total Scores per Date (for 9 Holes)") %>%
      dySeries("V1", label = "Score") %>%
      dyAxis("y", label = "Score (Total Hits)", valueRange = c(0, 100)) %>%
      dyOptions(axisLineWidth = 1.5, fillGraph = TRUE, drawGrid = FALSE, drawPoints = TRUE, pointSize = 3)
  })
  
  output$historicTable <- renderTable(tail(scores),10)
}

# Run the app ----
shinyApp(ui = ui, server = server)