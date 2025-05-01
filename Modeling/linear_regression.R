## Liam Jennings
## Honors Capstone


# Libraries and Functions -------------------------------------------------

## libraries
library(tidyverse)
library(broom)


# Linear Regression Model -------------------------------------------------------

linear_model <- lm(
  # formula
  wOBA_diff ~
    eye_color +
    launch_speed_diff +
    launch_angle_diff +
    zone_contact_pct_diff +
    whiff_pct_diff,
    
    # weights
    weights = c(
      PA_day + PA_night
    ),
  
  # data
  data = statcast_combined
)

## coefficients
linear_model |> 
  tidy()


## goodness-of-fit
linear_model |> 
  glance()



# Linear Regression Model -------------------------------------------------------

linear_model <- lm(
  # formula
  wOBA_diff ~
    eye_color +
    launch_speed_diff +
    zone_contact_pct_diff +
    whiff_pct_diff,
  
  # weights
  weights = c(
    PA_day + PA_night
  ),
  
  # data
  data = statcast_combined
)

## coefficients
linear_model |> 
  tidy()


## goodness-of-fit
linear_model |> 
  glance()
