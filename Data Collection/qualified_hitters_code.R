## Qualified Hitters from Statcast

## libraries
library(tidyverse)
# devtools::install_github("BillPetti/baseballr")
library(baseballr)

## read in the data
### qualified hitters from Baseball Savant
statcast_qual <- read_csv("statcast_qualified_hitters.csv")

### 2021-24 statcast pitch-by-pitch data
statcast <- read_csv("statcast.csv") |> 
  # remove index column
  select(-1)

## glimpse
glimpse(statcast_qual)


# Qualified Hitters Pitch-by-Pitch Data -----------------------------------

## qualified hitters
statcast_qual <- {
  statcast_qual |> 
    # create a variable of player_id and year
    mutate(
      id_year = paste0(player_id, "_", year)
    )
}
  
## add same variable to statcast dataset
statcast_qh <- {
  statcast |> 
    # create id year variable
    mutate(
      id_year = paste0(batter, "_", game_year)
    ) |> 
    filter(
      # only have qualified hitters
      id_year %in% statcast_qual$id_year,
      # remove all-star game and exhibition games
      !(home_team %in% c("AL", "NL", "MEX", "NWA")),
      !(away_team %in% c("SAC", "SUG", "WNP")),
    )
}


# Get MLB Game Information ------------------------------------------------
## get time code for a game
test_game <- baseballr::mlb_game_timecodes(634612)

## only take first observation for first pitch (may be able to do each timestamp throughout the game)
first_pitch <- test_game[3,]

## full date and time in a single string
first_pitch_time <- paste0(substr(first_pitch, 1, 8), " ", substr(first_pitch, 10, 15))

## convert to a time object
first_pitch_time <- first_pitch_time |> ymd_hms()

## change time zone (OlsonNames() to see every time zone)
first_pitch_tz <- first_pitch_time |> with_tz("US/Central")

## convert to 12-hour format (with AM/PM)
formatted_time <- first_pitch_tz |> format("%I:%M %p")


## first pitch function
first_pitch_time_function <- function(game_pk, home_team){
  ## get time code for a game
  time_code <- mlb_game_timecodes(game_pk)
  
  ## first pitch time frame
  first_pitch <- time_code[3, ]
  
  ## full date and time in a single string
  first_pitch_time <- paste0(substr(first_pitch, 1, 8), " ", substr(first_pitch, 10, 15))
  
  ## convert to a time object
  time_object <- first_pitch_time |> ymd_hms()
  
  ## change time zone based on home team
  ### Eastern time zone teams
  if (home_team %in% c("ATL", "BAL", "BOS", "CIN", "CLE", "DET", "MIA", "NYM", "NYY", "PHI", "PIT", "TB", "TOR", "WSH")) {
    ## change time zone
    first_pitch_tz <- time_object |> with_tz("US/Eastern")
  } # Central time zone teams
  else if (home_team %in% c("CHC", "CWS", "HOU", "KC", "MIL", "MIN", "STL", "TEX")) {
    ## change time zone
    first_pitch_tz <- time_object |> with_tz("US/Central")
  } # Mountain time zone teams
  else if (home_team == "COL") {
    first_pitch_tz <- time_object |> with_tz("US/Mountain")
  } # Arizona (because they do not observe daylight savings)
  else if (home_team == "AZ") {
    first_pitch_tz <- time_object |> with_tz("US/Arizona")
  } # Pacific time zone teams
  else {
    first_pitch_tz <- time_object |> with_tz("US/Pacific")
  }
  
  ## convert to 12-hour format (with AM/PM)
  formatted_time <- first_pitch_tz |> format("%I:%M %p")
  
  ## return first pitch time
  return(formatted_time)
}

## try out function
# first_pitch_time_function(statcast_qh$game_pk[4], statcast_qh$home_team[4])


## apply to dataset
first_pitch_df <- tibble(
  # game ID
  game_pk = statcast_qh$game_pk,
  # game date
  game_date = statcast_qh$game_date,
  # home team
  home_team = statcast_qh$home_team
) |> 
  # only keep distinct pairings
  distinct(game_pk, game_date, home_team) |> 
  # apply the first pitch time function
  mutate(
    first_pitch = pmap(list(game_pk, home_team), first_pitch_time_function)
  )
