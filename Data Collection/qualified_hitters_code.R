## Qualified Hitters from Statcast

## libraries
library(tidyverse)
# devtools::install_github("BillPetti/baseballr")
library(baseballr)

## read in the data
statcast_qual <- read_csv("statcast_qualified_hitters.csv")

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
    # only have qualified hitters
    filter(
      id_year %in% statcast_qual$id_year
    )
}


# Get MLB Game Information ------------------------------------------------
test_game <- baseballr::mlb_game_timecodes(statcast_qh$game_pk[1])

glimpse(statcast_qh)

