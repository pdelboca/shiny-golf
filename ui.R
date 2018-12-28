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
library(geosphere)

source("./watchpos.R")
source("data/golf_data.R")


shinyUI(
  dashboardPage(
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
    ),
    menuItem(
      "Distance to Hole",
      tabName = "distanceToHole",
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
      fluidRow(
        box(dygraphOutput("parsPerGame"), width = 4)
      )
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
      fluidRow(
        box(radioButtons("holeInput", choices = unique(courses$hole), label = "Select Course Hole: ", inline = TRUE),width = 12)
      ),
      fluidRow(
        box(dygraphOutput("holeStrokesHistory"), width = 6)
      ),
      fluidRow(box(
        plotOutput("historicDispersion"), width = 12
      ))
    ),
    # Distance to Hole -----------------------------------------------------
    tabItem(
      tabName = "distanceToHole",
      fluidRow(
      column(2, 
             fluidRow(watchpos,
               box(selectInput("courseInputDistance", "Select Course:", courses$course, selected = "ALL"), width = 12)),
      fluidRow(box(radioButtons("holeInputDistance", choices = unique(courses$hole), label = "Select Course Hole: "), width = 12))
      ),
      column(10, 
             fluidRow(box(textOutput("distance"), width = 12)),
             fluidRow(box(leafletOutput("courseMapDistance"), title = "Course Map", width = 12)))
      )
    )
  ))
)
)