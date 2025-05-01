## Liam Jennings
## Honors Capstone


# Libraries and Functions -------------------------------------------------

## libraries
library(tidyverse)
library(ranger)
library(vip)



# Random Forest Model -----------------------------------------------------


## set a seed
set.seed(8029)


## filter for eye color
eye_color_filter <- statcast_combined |> 
  filter(
    eye_color != "Hazel"
  )


## count blue-eyed players
blue_eyes <- eye_color_filter |> 
  filter(
    eye_color == "Blue"
  ) |> 
  nrow()


## sample brown-eye players to match blue (random undersampling)
brown_eyes <- eye_color_filter |> 
  filter(
    eye_color == "Brown"
  ) |> 
  slice_sample(
    n = blue_eyes
  )


## combine
balanced_data <- bind_rows(
  # blue-eyed players
  eye_color_filter |> 
    filter(
      eye_color == "Blue"
    ),
  
  # brown-eyed players
  brown_eyes
)


## training data
train <- balanced_data |> 
  slice_sample(
    prop = 0.80
  )


## testing set
test <- balanced_data |> 
  anti_join(train)


## model
eye_color_rf_full_model <- ranger(
  wOBA_diff ~
    eye_color +
    launch_speed_diff +
    launch_angle_diff +
    zone_contact_pct_diff +
    whiff_pct_diff,
  num.trees = 1000,
  importance = "impurity",
  data = train
)


## model summary
eye_color_rf_full_model


## variable importance
eye_color_rf_full_model |> 
  vip()
