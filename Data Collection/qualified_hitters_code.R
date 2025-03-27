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
    x = "Time of Day",
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


# Look at Plate Appearances in Each Time of Day ---------------------------
## day time
statcast_day <- {
  statcast_qh |> 
    # filter for day time
    filter(game_pk %in% c(day_night_df |> filter(DayNight == "Day") |> pull(game_pk))) |> 
    
    # mutate
    mutate(
      # change game_year to Season
      season = factor(game_year, levels = c(2021, 2022, 2023, 2024)),
      
      # Create a swing indicator
      swing = ifelse(description %in%
                       c("bunt_foul_tip",
                         "foul", "foul_bunt",
                         "foul_pitchout",
                         "foul_tip", "hit_into_play",
                         "missed_bunt", "swinging_strike",
                         "swinging_strike_blocked"), 
                     1, 0),
      
      
      # Create an indicator for a missed swing attempt
      miss = ifelse(description %in%
                      c("missed_bunt", "swinging_strike",
                        "swinging_strike_blocked", "foul_tip",
                        "bunt_foul_tip"), 
                    1, 0)
      
      # not worrying about platoon advantages / handedness standardization
    ) |> 
    
    # group by batter and season
    group_by(
      batter, 
      player_name,
      season
    ) |> 
    
    # hitter summary stats
    summarize(
      # counting stats
      n_pitches = n(),
      n_swings = sum(swing, na.rm = TRUE),
      n_miss = sum(miss, na.rm = TRUE),
      swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
      n_in_zone_swing = sum(swing == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing = sum(swing == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_contact = sum(swing == 1 & miss == 0 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_contact = sum(swing == 1 & miss == 0 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_swing_and_miss = sum(swing == 1 & miss == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing_and_miss = sum(swing == 1 & miss == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_pitch_in_zone = sum(zone %in% c(1:9), na.rm = TRUE),
      n_pitch_out_zone = sum(!(zone %in% c(1:9)), na.rm = TRUE),
      
      # metrics
      xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
      
    ) |> 
    
    # calculate common baseball stats
    mutate(
      # Zone %
      zone_pct = round(n_pitch_in_zone / n_pitches, 6) * 100,
      
      # Zone Swing %
      zone_swing_pct = round(n_in_zone_swing / n_pitch_in_zone, 6) * 100,
      
      # Zone Contact %
      zone_contact_pct = round(n_in_zone_contact / n_in_zone_swing, 6) * 100,
      
      # Zone Swing and Miss %
      zone_swing_and_miss_pct = round(n_in_zone_swing_and_miss / n_in_zone_swing, 6) * 100,
      
      # Chase %
      chase_pct = round(n_out_zone_swing / n_pitch_out_zone, 6) * 100,
      
      # Chase Contact %
      chase_contact_pct = round(n_out_zone_contact / n_out_zone_swing, 6) * 100,
      
      # Chase swing and miss pct
      chase_swing_and_miss_pct = round(n_out_zone_swing_and_miss / n_out_zone_swing, 6) * 100,
      
      # Swing %
      swing_pct = round(n_swings / n_pitches, 6) * 100,
      
      # Whiff %
      whiff_pct = round(n_miss / n_swings, 6) * 100,
      
      # Swing and miss Rate
      swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100
    ) |> 
    
    # ungroup
    ungroup()
}

## night time
statcast_night <- {
  statcast_qh |> 
    # filter for day time
    filter(game_pk %in% c(day_night_df |> filter(DayNight == "Night") |> pull(game_pk))) |> 
    
    # mutate
    mutate(
      # change game_year to Season
      season = factor(game_year, levels = c(2021, 2022, 2023, 2024)),
      
      # Create a swing indicator
      swing = ifelse(description %in%
                       c("bunt_foul_tip",
                         "foul", "foul_bunt",
                         "foul_pitchout",
                         "foul_tip", "hit_into_play",
                         "missed_bunt", "swinging_strike",
                         "swinging_strike_blocked"), 
                     1, 0),
      
      
      # Create an indicator for a missed swing attempt
      miss = ifelse(description %in%
                      c("missed_bunt", "swinging_strike",
                        "swinging_strike_blocked", "foul_tip",
                        "bunt_foul_tip"), 
                    1, 0)
      
      # not worrying about platoon advantages / handedness standardization
    ) |> 
    
    # group by batter and season
    group_by(
      batter, 
      player_name,
      season
    ) |> 
    
    # hitter summary stats
    summarize(
      # counting stats
      n_pitches = n(),
      n_swings = sum(swing, na.rm = TRUE),
      n_miss = sum(miss, na.rm = TRUE),
      swing_strikes = sum(swing == 1 & miss == 1, na.rm = TRUE),
      n_in_zone_swing = sum(swing == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing = sum(swing == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_contact = sum(swing == 1 & miss == 0 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_contact = sum(swing == 1 & miss == 0 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_in_zone_swing_and_miss = sum(swing == 1 & miss == 1 & zone %in% c(1:9), na.rm = TRUE),
      n_out_zone_swing_and_miss = sum(swing == 1 & miss == 1 & !(zone %in% c(1:9)), na.rm = TRUE),
      n_pitch_in_zone = sum(zone %in% c(1:9), na.rm = TRUE),
      n_pitch_out_zone = sum(!(zone %in% c(1:9)), na.rm = TRUE),
      
      # metrics
      xwOBA = mean(estimated_woba_using_speedangle, na.rm = TRUE),
      
    ) |> 
    
    # calculate common baseball stats
    mutate(
      # Zone %
      zone_pct = round(n_pitch_in_zone / n_pitches, 6) * 100,
      
      # Zone Swing %
      zone_swing_pct = round(n_in_zone_swing / n_pitch_in_zone, 6) * 100,
      
      # Zone Contact %
      zone_contact_pct = round(n_in_zone_contact / n_in_zone_swing, 6) * 100,
      
      # Zone Swing and Miss %
      zone_swing_and_miss_pct = round(n_in_zone_swing_and_miss / n_in_zone_swing, 6) * 100,
      
      # Chase %
      chase_pct = round(n_out_zone_swing / n_pitch_out_zone, 6) * 100,
      
      # Chase Contact %
      chase_contact_pct = round(n_out_zone_contact / n_out_zone_swing, 6) * 100,
      
      # Chase swing and miss pct
      chase_swing_and_miss_pct = round(n_out_zone_swing_and_miss / n_out_zone_swing, 6) * 100,
      
      # Swing %
      swing_pct = round(n_swings / n_pitches, 6) * 100,
      
      # Whiff %
      whiff_pct = round(n_miss / n_swings, 6) * 100,
      
      # Swing and miss Rate
      swing_and_miss_pct = round(swing_strikes / n_pitches, 6) * 100
    ) |> 
    
    # ungroup
    ungroup()
}



## plot
tibble(
  xwOBA = c(statcast_day$xwOBA, statcast_night$xwOBA),
  dayNight = c(rep("Day", 525), rep("Night", 525))
) |> 
  mutate(
    dayNight = factor(dayNight)
  ) |> 
  ggplot(aes(xwOBA, dayNight, fill = dayNight)) +
  # scatterplot
  # geom_point(
  #   position = "jitter",
  #   size = 1.5,
  #   alpha = 0.5
  # ) +
  # boxplot
  geom_boxplot(
    color = "black",
    alpha = 0.60
  ) +
  # violin plot
  geom_violin(
    alpha = 0.10
  ) +
  # labels
  labs(
    y = "Time of Day",
    title = "xwOBA by Time of Day"
  ) +
  # scale age colors
  scale_fill_manual(
    values = c("#A5ACAF", "#FFB612")
  ) +
  theme(
    # center and bold title
    plot.title = element_text(face = "bold", size = 18, hjust = 0.5),
    # remove legend
    legend.position = "none"
  )


## histogram
### bin width
bw <- 2 * IQR(statcast_day$xwOBA - statcast_night$xwOBA) / 525 ^ (1/3)

### plot
tibble(
  xwOBA_diff = statcast_day$xwOBA - statcast_night$xwOBA
) |> 
  ggplot(aes(xwOBA_diff)) +
  geom_histogram(
    aes(
      y = after_stat(density)
    ),
    # bar outline color
    color = "grey50",
    # fill color
    fill = "dodgerblue4",
    # freedman-diaconis rule
    binwidth = bw, 
    # set boundary to 0
    boundary = 0
  ) +
  # normal density curve
  stat_function(
    fun = dnorm,
    n = 100,
    args = list(mean = mean(statcast_day$xwOBA - statcast_night$xwOBA), sd = sd(statcast_day$xwOBA - statcast_night$xwOBA)),
    linewidth = 1.15,
    col = "firebrick"
  ) +
  # labels
  labs(
    x = "xwOBA Difference",
    y = "Density",
    title = "xwOBA Difference Between Day and Night"
  ) +
  # black and white theme
  theme_bw() +
  # theme
  theme(
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.caption = element_text(size = 10),
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.text.x = element_text(size = 12, hjust = 0.5),
    axis.text.y = element_text(size = 12, hjust = 0.5),
  )


## manually enter eye color (brown, amber, green, hazel, blue, and gray)
tibble(
  player_name = unique(statcast_day$player_name),
  eye_color = c(
    "Brown", "Brown", "Amber", "Green",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Gray", "Amber", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Amber", "Brown", "Brown", "Brown",
    "Brown", "Blue", "Blue", "Blue",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Blue", "Brown", "Blue", "Brown",
    "Amber", "Brown", "Brown", "Brown",
    "Gray", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Gray",
    "Amber", "Brown", "Blue", "Blue",
    "Blue", "Brown", "Blue", "Hazel",
    "Brown", "Brown", "Green", "Brown",
    "Brown", "Gray", "Brown", "Brown",
    "Blue", "Blue", "Brown", "Brown",
    "Hazel", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Blue", "Brown", "Brown", "Brown",
    "Hazel", "Brown", "Brown", "Brown",
    "Brown", "Gray", "Green", "Brown",
    "Hazel", "Brown", "Blue", "Hazel",
    "Brown", "Brown", "Blue", "Gray",
    "Brown", "Blue", "Blue", "Amber",
    "Brown", "Brown", "Gray", "Amber",
    "Brown", "Blue", "Blue", "Brown",
    "Hazel", "Brown", "Blue", "Brown",
    "Gray", "Brown", "Brown", "Amber",
    "Brown", "Brown", "Brown", "Amber",
    "Brown", "Amber", "Gray", "Brown",
    "Brown", "Hazel", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Blue", "Hazel",
    "Brown", "Amber", "Blue", "Brown",
    "Blue", "Amber", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Blue",
    "Brown", "Blue", "Blue", "Blue",
    "Brown", "Green", "Brown", "Green",
    "Brown", "Brown", "Blue", "Gray",
    "Amber", "Hazel", "Brown", "Brown",
    "Blue", "Blue", "Blue", "Brown",
    "Blue", "Hazel", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Amber", "Brown",
    "Brown", "Brown", "Brown", "Amber",
    "Brown", "Brown", "Brown", "Amber",
    "Gray", "Blue", "Brown", "Brown",
    "Blue", "Gray", "Amber", "Brown",
    "Hazel", "Blue", "Blue", "Hazel",
    "Amber", "Hazel", "Brown", "Brown",
    "Blue", "Brown", "Brown", "Brown",
    "Brown", "Amber", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Blue",
    "Brown", "Blue", "Amber", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Blue", "Blue", "Blue",
    "Brown", "Brown", "Amber", "Brown",
    "Brown", "Brown", "Brown", "Blue",
    "Brown", "Blue", "Brown", "Amber",
    "Brown", "Brown", "Brown", "Brown",
    "Brown", "Brown", "Brown", "Brown",
    "Blue", "Amber", "Green", "Brown"
  )
)
