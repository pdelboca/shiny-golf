library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)

source("data/golf_data.R")

ui <- dashboardPage(
  dashboardHeader(title="Personal Golf Statistics"),
  dashboardSidebar(),
  dashboardBody(
    fluidRow(
      valueBoxOutput("totalCourses",width = 2),
      valueBoxOutput("totalGames",width = 2),
      valueBoxOutput("totalHoles",width = 2),
      valueBoxOutput("totalShots", width = 2),
      valueBoxOutput("avgShots",width = 2),
      valueBoxOutput("daysSince",width = 2)
    ),
    fluidRow(
      box(dygraphOutput("historicScores"), width = 8),
      box(tableOutput("historicTable"), title = "Last 10 Games", width = 4)
    ),
    fluidRow(
      box(tableOutput("historicCards"), title = "Historic Cards", width = 12)
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$totalCourses <- renderValueBox({
    valueBox(
      length(unique(courses$course)), "Courses"
    )
  })
  
  output$totalGames <- renderValueBox({
    valueBox(
      length(unique(cards$date)), "Games Played"
    )
  })
  
  output$totalHoles <- renderValueBox({
    valueBox(
      sum(!is.na(cards$shots)), "Holes Played"
    )
  })
  
  output$totalShots <- renderValueBox({
    valueBox(
      sum(cards$shots, na.rm = TRUE), "Total Shots")
    )
  })
  
  output$avgShots <- renderValueBox({
    avgShots <- cards %>%
      group_by(date) %>%
      summarise(sum=sum(shots, na.rm = TRUE)) %>%
      summarise(avg=mean(sum))
    valueBox(
      avgShots$avg, "Avg Shots per Game"
    )
  })
  
  output$daysSince <- renderValueBox({
    daysSince <- Sys.Date() - max(cards$date)
    valueBox(
      daysSince, "Days Since Last Game"
    )
  })
  
  scores <- reactive({
    cards %>%
      group_by(date, course) %>% # TODO: Review if multiple matchs in 1 day
      #filter(if(input$course != "All") course == input$course else TRUE) %>%
      summarise(holes = sum(!is.na(shots)),
        total = sum(shots, na.rm = TRUE)) %>%
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
    scores() %>%
      arrange(desc(date)) %>% 
      head(10) %>%
      mutate(date = as.character(date)), # Needed due to a bug
    width = "100%"
  )
  
  output$historicCards <- renderTable(
    cards %>%
      mutate(date = as.character(date),
             shots = as.integer(ifelse(is.na(shots), 0, shots))) %>%
      spread(hole, shots,sep="_") %>%
      mutate(total_shots = rowSums(select_if(., is.numeric), na.rm = TRUE)),
    width = "100%"
  )
}

# Run the app ----
shinyApp(ui = ui, server = server)