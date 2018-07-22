library(shiny)
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
      plotOutput("historicScores")
      , width = 10
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$historicScores <- renderPlot(
    hist(rnorm(100))
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)