shinyServer(
  function(input, output, session) {
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
  
  output$parsPerGame <- renderDygraph({
    cards %>% 
      left_join(courses) %>% 
      mutate(made_par = ifelse(shots == par,1,0)) %>% 
      group_by(id_game, date) %>% 
      summarise(cant_par = sum(made_par, na.rm = TRUE)) %>%
      xts(x = .$cant_par, order.by = .$date) %>%
      dygraph(main = "Pars per Game") %>%
      dySeries("V1", label = "Pars") %>%
      dyAxis("y", label = "Pars", valueRange = c(0, 18)) %>%
      dyOptions(
        axisLineWidth = 1.5,
        fillGraph = TRUE,
        drawGrid = FALSE,
        drawPoints = TRUE,
        pointSize = 3
      )
  })
  
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
  
  output$holeStrokesHistory <- renderDygraph({
    history <- cards %>% 
      filter(course == input$courseInput, hole == input$holeInput, !is.na(shots)) %>% 
      select(date, shots)
    
    hole_par <- courses %>% 
      filter(course == input$courseInput, hole == input$holeInput) %>% 
      select(par) %>% 
      unique() %>%
      as.integer()
    
    history %>% 
      xts(x = .$shots, order.by = .$date) %>%
      dygraph(main = paste("Strokes for hole", input$holeInput, "at", input$courseInput, "course.")) %>%
      dySeries("V1", label = "Strokes") %>%
      dyLimit(hole_par, label = "Par", color = "red") %>% 
      dyAxis("y", label = "Strokes", valueRange = c(0, max(cards$shots, na.rm = TRUE) + 1)) %>%
      dyOptions(
        axisLineWidth = 1.5,
        fillGraph = TRUE,
        drawGrid = FALSE,
        drawPoints = TRUE,
        pointSize = 3
      )
    
    
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
    
    # Distance to Hole -------------------------------------------
    output$courseMapDistance <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles("Esri.WorldImagery")
    })
    
    observe({
      if(!is.null(input$lat)){
        
        point_lat <- holes_points[holes_points$hole == input$holeInputDistance & holes_points$course == input$courseInputDistance, ]$lat
        point_lon <- holes_points[holes_points$hole == input$holeInputDistance & holes_points$course == input$courseInputDistance, ]$lon
        
        lat <- input$lat
        lng <- input$long
        #lat <- -31.645453
        #lng <- -64.446854
        acc <- input$accuracy
        time <- input$time
        
        proxy <- leafletProxy("courseMapDistance")
        
        proxy  %>% 
          clearGroup(group=c("pos","hole")) %>% 
          addMarkers(lng=lng, lat=lat, popup=paste("My location is:","<br>",
                                                   lng,"Longitude","<br>",
                                                   lat,"Latitude", "<br>",
                                                   "My accuracy is:",  "<br>",
                                                   acc, "meters"),
                     group="pos") %>%
          addCircles(lng=lng, lat=lat, radius=acc, group="pos") %>%
          addMarkers(lng = point_lon, lat=point_lat, group = "hole") %>% 
          setView(lng=lng, lat=lat, zoom = 16)
      }
      
      output$distance <- renderText({
        point_lat <- holes_points[holes_points$hole == input$holeInputDistance & holes_points$course == input$courseInputDistance, ]$lat
        point_lon <- holes_points[holes_points$hole == input$holeInputDistance & holes_points$course == input$courseInputDistance, ]$lon
        lat <- input$lat
        lng <- input$long
        # lat <- -31.645453
        # lng <- -64.446854
        paste("Distance to Hole",
              input$holeInputDistance,
              ":",
              round(distm(c(lng, lat), c(point_lon, point_lat), fun = distHaversine) * 1.09361,2),
              "yards.")
      })
      
    })
  })
}
)