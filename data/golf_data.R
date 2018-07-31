# Script to load data from GoogleSheet
get_cards <- function(){
  gs_title("datos_golf") %>%
    gs_read("cards") %>%
    select("date":"18") %>%
    gather("hole", "shots", "1":"18") %>%
    mutate(date = dmy(date),
           hole = as.numeric(hole)) %>%
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
