# Script to load data from GoogleSheet
get_cards <- function(){
  tidy_cards <- gs_title("datos_golf") %>%
    gs_read("cards") %>%
    mutate(id_game = row_number()) %>%
    select(id_game, "date":"18") %>%
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