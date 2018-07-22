# Script to load data from GoogleSheet
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)

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