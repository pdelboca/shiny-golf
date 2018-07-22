# Script to load data from GoogleSheet
library(googlesheets)
library(dplyr)
library(tidyr)
library(lubridate)

get_cards <- function(){
  gs_title("datos_golf") %>%
    gs_read("Tarjetas") %>%
    select("Fecha":"18") %>%
    gather("Hole", "Shots", "1":"18") %>%
    mutate(Fecha = dmy(Fecha),
           Hole = as.numeric(Hole)) %>%
    arrange(Fecha, Cancha, Hole)
}

cards <- get_cards()