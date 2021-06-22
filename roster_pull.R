library(RCurl); library(xml2); library(rvest); library(jsonlite); library(foreach)
library(lubridate)
library(tidyverse)

devtools::source_url("https://raw.githubusercontent.com/evolvingwild/evolving-hockey/master/EH_scrape_functions.R")

schedule <- sc.scrape_schedule(end_date = "2021-06-21", start_date = "2021-06-21")
roster <- sc.scrape_pbp(games = schedule$game_id, scrape_type = 'rosters')





wrangleRoster <- function(roster){
  teams <- unique(roster$roster_df$team)
  roster1 <- roster$roster_df$num_last_first[roster$roster_df$team == teams[1]]
  roster2 <- roster$roster_df$num_last_first[roster$roster_df$team == teams[2]]
  
  roster_radio_1 <- sapply(1:length(roster1), function(x)paste('player',x,sep='_'))
  roster_radio_2 <- sapply(1:length(roster2), function(x)paste('player',x,sep='_'))
  
  names(roster_radio_1) = roster1
  names(roster_radio_2) = roster2
  
  names(roster1) = roster1
  names(roster2) = roster2
  
  return(
    list(teams = teams, 
       roster_1 = roster1,
       roster_2 = roster2
       ))
  
}
roster$roster_df$num_last_first
wrangleRoster(roster)


