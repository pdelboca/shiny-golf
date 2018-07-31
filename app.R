library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)
library(purrr)

source("data/golf_data.R")

ui <- dashboardPage(
  dashboardHeader(title = "Personal Golf Statistics"),
  dashboardSidebar(sidebarMenu(
    menuItem(
      "Historic Data",
      tabName = "historicData",
      icon = icon("dashboard")
    ),
    menuItem(
      "Course Statistics",
      tabName = "courseStatistics",
      icon = icon("th")
    )
  )),
  dashboardBody(tabItems(
    # Historic Data Tab -----------------------------------------------------
    tabItem(
      tabName = "historicData",
      fluidRow(
        valueBoxOutput("totalCourses", width = 2),
        valueBoxOutput("totalGames", width = 2),
        valueBoxOutput("totalHoles", width = 2),
        valueBoxOutput("totalShots", width = 2),
        valueBoxOutput("avgShots", width = 2),
        valueBoxOutput("daysSince", width = 2)
      ),
      fluidRow(
        box(dygraphOutput("historicScores"), width = 8),
        box(
          tableOutput("historicTable"),
          title = "Last 10 Games",
          width = 4
        )
      ),
      fluidRow(box(
        tableOutput("historicCards"),
        title = "Historic Cards",
        width = 12
      ))
      
    ),
    # Course Statistics Tab -----------------------------------------------------
    tabItem(
      tabName = "courseStatistics",
      fluidRow(box(
        selectInput("courseInput", "Select Course:", courses$course, selected = "ALL"),
        width = 2
      )),
      fluidRow(
        valueBoxOutput("courseTotalGames", width = 2),
        valueBoxOutput("courseTotalHoles", width = 2),
        valueBoxOutput("courseTotalShots", width = 2),
        valueBoxOutput("courseAvgShots", width = 2),
        valueBoxOutput("courseDaysSince", width = 2)
      )
    )
  ))
)

server <- function(input, output) {
  # Historic Data Tab -----------------------------------------------------
  
  output$totalCourses <- renderValueBox({
    valueBox(length(unique(courses$course)), "Courses")
  })
  
  output$totalGames <- renderValueBox({
    valueBox(length(unique(cards$date)), "Games Played")
  })
  
  output$totalHoles <- renderValueBox({
    valueBox(sum(!is.na(cards$shots)), "Holes Played")
  })
  
  output$totalShots <- renderValueBox({
    valueBox(sum(cards$shots, na.rm = TRUE), "Total Shots")
  })
  
  output$avgShots <- renderValueBox({
    avgShots <- cards %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(avgShots$avg, "Avg Shots per Game")
  })
  
  output$daysSince <- renderValueBox({
    daysSince <- Sys.Date() - max(cards$date)
    valueBox(daysSince, "Days Since Last Game")
  })
  
  scores <- reactive({
    cards %>%
      group_by(date, course) %>% # TODO: Review if multiple matchs in 1 day
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
      dyOptions(
        axisLineWidth = 1.5,
        fillGraph = TRUE,
        drawGrid = FALSE,
        drawPoints = TRUE,
        pointSize = 3
      )
  })
  
  output$historicTable <- renderTable(scores() %>%
                                        arrange(desc(date)) %>%
                                        head(10) %>%
                                        mutate(date = as.character(date)),
                                      # Needed due to a bug
                                      width = "100%")
  
  output$historicCards <- renderTable(
    cards %>%
      mutate(date = as.character(date),
             shots = as.integer(ifelse(
               is.na(shots), 0, shots
             ))) %>%
      spread(hole, shots, sep = "_") %>%
      mutate(total_shots = rowSums(select_if(., is.numeric), na.rm = TRUE)),
    width = "100%"
  )
  
  # Course Statistics Tab -----------------------------------------------------
  
  courseData <- reactive({
    courseData <- cards %>%
      left_join(courses) %>%
      filter(course == input$courseInput)
    courseData
  })
  
  output$courseTotalGames <- renderValueBox({
    valueBox(length(unique(courseData()$date)), "Games Played")
  })
  
  output$courseTotalHoles <- renderValueBox({
    valueBox(sum(!is.na(courseData()$shots)), "Holes Played")
  })
  
  output$courseTotalShots <- renderValueBox({
    valueBox(sum(courseData()$shots, na.rm = TRUE), "Total Shots")
  })
  
  output$courseAvgShots <- renderValueBox({
    avgShots <- courseData() %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(avgShots$avg, "Avg Shots per Game")
  })
  
  output$courseDaysSince <- renderValueBox({
    daysSince <- Sys.Date() - max(courseData()$date)
    valueBox(daysSince, "Days Since Last Game")
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)