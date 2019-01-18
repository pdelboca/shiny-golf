# Script to load data from GoogleSheet
get_cards <- function(){
  tidy_cards <- gs_title("datos_golf") %>%
    gs_read("cards") %>%
    mutate(id_game = row_number()) %>%
    select(id_game, "date":"18") %>% 
    mutate(holes_round = case_when(!is.na(`1`) & !is.na(`18`) ~ "18 Holes",
                                   !is.na(`1`)  ~ "First 9",
                                   !is.na(`18`) ~ "Back 9")) %>%  
    gather("hole", "shots", "1":"18") %>%
    mutate(date = dmy(date),
           hole = as.numeric(hole)) %>%
    group_by(id_game) %>%
    mutate(holes_played = sum(!is.na(shots))) %>%
    ungroup() %>%
    arrange(date, course, hole)
}

cards <- get_cards()

get_courses <- function(){
  gs_title("datos_golf") %>%
    gs_read("courses") %>%
    select("course":"18") %>%
    gather("hole", "par", "1":"18") %>%
    mutate(hole = as.numeric(hole)) %>%
    arrange(course, hole)
}

courses <- get_courses()

get_course_location <- function(){
  gs_title("datos_golf") %>%
    gs_read("courses") %>%
    select(course, lat, lon, map_zoom)
}

courses_location <-get_course_location()

get_holes_points <- function(){
  gs_title("datos_golf") %>%
    gs_read("holes_points") %>%
    select(course, hole, lat, lon) %>%
    mutate(course = as.character(course))
}

holes_points <- get_holes_points()
