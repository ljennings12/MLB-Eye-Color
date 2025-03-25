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
      # only regular season
      game_type == "R",
      # remove indoor stadiums (including retractable roofs)
      !(home_team %in% c("TB", "TOR", "AZ", "HOU", "MIL", "MIA", "TEX"))
    )
}


# Get MLB Game Information ------------------------------------------------

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

## unlist first pitch variable
first_pitch_clean <- {
  first_pitch_df |> 
    mutate(first_pitch = unlist(first_pitch_df$first_pitch)) |> 
    as.data.frame()
}

## manually fix international games and other errors
first_pitch_clean <- first_pitch_clean |> 
  mutate(
    # Oakland A's vs. Detroit Tigers on May 10, 2022
    home_team = if_else(game_date == "2022-05-10" & home_team == "OAK", "DET", home_team),
    first_pitch = if_else(game_date == "2022-05-10" & home_team == "DET", "01:10 PM", first_pitch),
    
    # # San Francisco Giants vs. San Diego Padres on April 29, 2023 in Mexico City
    # first_pitch = if_else(game_date == "2023-04-29" & home_team == "SD", "05:05 PM", first_pitch),
    # 
    # # San Francisco Giants vs. San Diego Padres on April 30, 2023 in Mexico City
    # first_pitch = if_else(game_date == "2023-04-30" & home_team == "SD", "03:05 PM", first_pitch),
    # 
    # # Chicago Cubs vs. St. Louis Cardinals on June 24, 2023 in London
    # first_pitch = if_else(game_date == "2023-06-24" & home_team == "STL", "06:10 PM", first_pitch),
    # 
    # # Chicago Cubs vs. St. Louis Cardinals on June 25, 2023 in London
    # first_pitch = if_else(game_date == "2023-06-25" & home_team == "STL", "03:10 PM", first_pitch),
    # 
    # # Los Angeles Dodgers vs. San Diego Padres on March 20, 2024 in Seoul
    # first_pitch = if_else(game_date == "2024-03-20" & home_team == "SD", "07:05 PM", first_pitch),
    # 
    # # San Diego Padres vs. Los Angeles Dodgers on March 21, 2024 in Seoul
    # first_pitch = if_else(game_date == "2024-03-21" & home_team == "LAD", "07:05 PM", first_pitch),
    # 
    # # Houston Astros vs. Colorado Rockies on April 27, 2024 in Mexico City
    # first_pitch = if_else(game_date == "2024-04-27" & home_team == "COL", "05:05 PM", first_pitch),
    # 
    # # Houston Astros vs. Colorado Rockies on April 28, 2024 in Mexico City
    # first_pitch = if_else(game_date == "2024-04-28" & home_team == "COL", "03:05 PM", first_pitch),
    # 
    # # Philadelphia Phillies vs. New York Mets on June 8, 2024 in London
    # first_pitch = if_else(game_date == "2024-06-08" & home_team == "NYM", "06:10 PM", first_pitch),
    # 
    # # New York Mets vs. Philadelphia Phillies on June 9, 2024 in London
    # first_pitch = if_else(game_date == "2024-06-09" & home_team == "PHI", "03:10 PM", first_pitch),
  ) |> 
  # remove international games
  slice(
    -c(4074, 4057, 4619, 4608, 5590, 5589, 5873, 5872, 6365, 6353)
  )
  


# Create Day and Night Data -----------------------------------------------

## Day (before 2:15 PM) and Night (after 6:00 PM) Variables

## filter dataset
day_night_df <- first_pitch_clean |> 
  mutate(
    # parse into POSIXct while looking through multiple formats
    first_pitch = parse_date_time(first_pitch, orders = c("I pM", "H:M p")),
    # convert to 12-hour format without date
    first_pitch_string = format(first_pitch, "%I:%M %p"),
    # create a day indicator
    DayNight = case_when(
      first_pitch <= as.POSIXct("0000-01-01 14:15:00", tz = "UTC") ~ "Day",
      first_pitch >= as.POSIXct("0000-01-01 18:0:00", tz = "UTC") ~ "Night",
      first_pitch > as.POSIXct("0000-01-01 14:15:00", tz = "UTC") & first_pitch < as.POSIXct("0000-01-01 18:00:00", tz = "UTC") ~ "Late Afternoon"
    )
  ) |> 
  # remove late afternoon games (removes 616 observations)
  filter(
    DayNight != "Late Afternoon"
  )


## theme set
theme_set(theme_bw())

## barplot of day and night games
day_night_df |> 
  # count day and night
  count(DayNight) |> 
  # calculate proportions
  mutate(prop = n / sum(n)) |> 
  # prop
  ggplot(aes(DayNight, prop, fill = DayNight)) +
  geom_col(
    color = "black"
  ) +
  # labels
  labs(
    y = "Percentage",
    title = "MLB Time of Day Breakdown"
  ) +
  # scale age colors
  scale_fill_manual(
    values = c("black", "#FFB612")
  ) +
  # y axis percentages
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(
    # center and bold title
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    # remove legend
    legend.position = "none"
  )


## save first pitch data (will need to manually change international games and check AM games for accuracy)
# write.csv(first_pitch_clean, "first_pitch_times.csv")






# Draft Code --------------------------------------------------------------

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

