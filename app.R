library(shiny)
library(shinydashboard)
library(dygraphs)
library(xts)
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(leaflet)
library(shinydashboardPlus)
library(zoo)

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
        valueBoxOutput("totalGames", width = 2),
        valueBoxOutput("totalHoles", width = 2),
        valueBoxOutput("totalShots", width = 2),
        valueBoxOutput("avgShots9Holes", width = 2),
        valueBoxOutput("avgShots18Holes", width = 2),
        valueBoxOutput("daysSince", width = 2)
      ),
      fluidRow(
        box(dygraphOutput("historicScores9Holes"), width = 4),
        box(dygraphOutput("historicScores18Holes"), width = 4),
        box(
          tableOutput("historicTable"),
          title = "Last 10 Games",
          width = 4
        )
      ),
      fluidRow(
        box(
          title="2018 Goals", width = 4,
          "My goal for 2018 is to play 4 matches on a row without any 10 on the card."
        ),
        infoBoxOutput("goalFulfilled", width = 2),
        infoBoxOutput("last4GamesAverageShots", width = 2)
        
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
      ),
      box(leafletOutput("courseMap"),title = "Course Map", width = 5),
      gradientBox(
        title = "Course Historic Games",
        boxToolSize = "sm",
        width = 5,
        collapsible = FALSE,
        footer = column(
          width = 12,
          align = "center",
          radioButtons(
            "holesCourseInput",
            "Holes Played:",
            choices = list(9, 18)
          )
        ),
        dygraphOutput("historicScoresCourseStatistics")
      )
      ),
      fluidRow(
        valueBoxOutput("courseTotalGames", width = 2),
        valueBoxOutput("courseTotalHoles", width = 2),
        valueBoxOutput("courseTotalShots", width = 2),
        valueBoxOutput("courseAvgShots9Holes", width = 2),
        valueBoxOutput("courseAvgShots18Holes", width = 2),
        valueBoxOutput("courseDaysSince", width = 2)
      ),
      fluidRow(box(
        plotOutput("historicDispersion"), width = 12
      ))
    )
  ))
)

server <- function(input, output) {
  # Historic Data Tab -----------------------------------------------------
  output$totalGames <- renderValueBox({
    valueBox(length(unique(cards$date)), "Games Played")
  })
  
  output$totalHoles <- renderValueBox({
    valueBox(sum(!is.na(cards$shots)), "Holes Played")
  })
  
  output$totalShots <- renderValueBox({
    valueBox(sum(cards$shots, na.rm = TRUE), "Total Shots")
  })
  
  output$avgShots9Holes <- renderValueBox({
    avgShots <- cards %>%
      filter(holes_played == 9) %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(round(avgShots$avg,2), "Avg Shots per Game (9 Holes)")
  })
  
  output$avgShots18Holes <- renderValueBox({
    avgShots <- cards %>%
      filter(holes_played == 18) %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(round(avgShots$avg,2), "Avg Shots per Game (18 Holes)")
  })
  
  output$daysSince <- renderValueBox({
    daysSince <- Sys.Date() - max(cards$date)
    valueBox(daysSince, "Days Since Last Game")
  })
  
  scores <- reactive({
    cards %>%
      group_by(date, course, holes_played) %>% # TODO: Review if multiple matchs in 1 day
      summarise(total = sum(shots, na.rm = TRUE)) %>%
      ungroup()
  })
  
  output$historicScores9Holes <- renderDygraph({
    scores() %>%
      filter(holes_played == 9) %>%
      xts(x = .$total, order.by = .$date) %>%
      dygraph(main = "Total Scores per Game (for 9 Holes)") %>%
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
  
  output$historicScores18Holes <- renderDygraph({
    scores() %>%
      filter(holes_played == 18) %>%
      xts(x = .$total, order.by = .$date) %>%
      dygraph(main = "Total Scores per Game (for 18 Holes)") %>%
      dySeries("V1", label = "Score") %>%
      dyAxis("y", label = "Score (Total Hits)", valueRange = c(0, 200)) %>%
      dyOptions(
        axisLineWidth = 1.5,
        fillGraph = TRUE,
        drawGrid = FALSE,
        drawPoints = TRUE,
        pointSize = 3
      )
  })
  
  output$goalFulfilled <- renderInfoBox({
    worsts <- cards %>%
      group_by(id_game) %>%
      summarise(worst_hole_shots = max(shots, na.rm = TRUE))
    
    # If there is a sequence of 4 without any 10, return TRUE
    goal_fulfilled <- any(rollapply(worsts$worst_hole_shots, 4, function(x) !any(x >= 10)))
    message <- ifelse(goal_fulfilled, "Yes! :D", "Nope! :_D")
    color <- ifelse(goal_fulfilled, "green", "red")
    
    infoBox("Goal Fulfilled?", message , icon = icon("flag"), color = color)
  })
  
  output$last4GamesAverageShots <- renderInfoBox({
    
    average_last_4 <- scores() %>%
      filter(holes_played == 9) %>%
      tail(4) %>%
      summarise(average = mean(total))
    
    infoBox("Last 4 Games Avg", average_last_4$average , icon = icon("stats", lib = "glyphicon"))
  })
  
  output$historicTable <- renderTable(
    scores() %>%
      arrange(desc(date)) %>%
      head(10) %>%
      mutate(date = as.character(date)),
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
      mutate(hole = factor(hole, levels = 1:18)) %>% # For plots
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
  
  output$courseAvgShots9Holes <- renderValueBox({
    avgShots <- courseData() %>%
      filter(holes_played == 9) %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(round(avgShots$avg,2), "Avg Shots per Game (9 Holes)")
  })
  
  output$courseAvgShots18Holes <- renderValueBox({
    avgShots <- courseData() %>%
      filter(holes_played == 18) %>%
      group_by(date) %>%
      summarise(sum = sum(shots, na.rm = TRUE)) %>%
      summarise(avg = mean(sum))
    valueBox(round(avgShots$avg,2), "Avg Shots per Game (18 Holes)")
  })
  
  output$courseDaysSince <- renderValueBox({
    daysSince <- Sys.Date() - max(courseData()$date)
    valueBox(daysSince, "Days Since Last Game")
  })
  
  output$historicDispersion <- renderPlot({
    courseData() %>%
      filter(!is.na(shots)) %>%
      ggplot(aes(x=hole, y=shots)) + 
      geom_point(shape="x", size = 3, color = "darkgreen") +
      geom_point(aes(x=hole,y=par), color="black", size=3) + 
      scale_y_continuous(limits=c(0,15), breaks=0:15, minor_breaks = FALSE) + 
      labs(
        title = "Holes Par vs Holes Shots",
        subtitle = "Historic dispersion of shots for each hole"
      ) +
      theme_minimal()
  })
  
  coursesLocation <- reactive({
    courses_location %>%
      filter(course == input$courseInput)
  })
  
  output$courseMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Esri.WorldImagery) %>%
      addMarkers(lng=coursesLocation()$lon, lat=coursesLocation()$lat, popup=coursesLocation()$course)
      #setView(lng=coursesLocation()$lon, lat=coursesLocation()$lat, zoom = 16)
  })
  
  output$historicScoresCourseStatistics <- renderDygraph({
    holes_played_input <- as.integer(input$holesCourseInput)

    courseData() %>%
      group_by(date, course, holes_played) %>% # TODO: Review if multiple matchs in 1 day
      summarise(total = sum(shots, na.rm = TRUE)) %>%
      filter(holes_played == holes_played_input) %>%
      xts(x = .$total, order.by = .$date) %>%
      dygraph(main = paste("Total Scores per Game (for", holes_played_input, "Holes)")) %>%
      dySeries("V1", label = "Score") %>%
      dyAxis("y", label = "Score (Total Hits)", valueRange = c(0, holes_played_input * 10)) %>%
      dyOptions(
        axisLineWidth = 1.5,
        fillGraph = TRUE,
        drawGrid = FALSE,
        drawPoints = TRUE,
        pointSize = 3
      )
    
  })
}

# Run the app ----
shinyApp(ui = ui, server = server)