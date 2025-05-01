## Liam Jennings
## Honors Capstone


# Libraries and Functions -------------------------------------------------

## libraries
library(tidyverse)

## create theme
mlb_plot_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # adjust x axis title
    axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust x axis text
    axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}

## set theme
theme_set(theme_bw())



# Data Table --------------------------------------------------------------

statcast_combined |>
  # mutate
  mutate(
    name = str_trim(
      # join multiple strings together
      str_c(
        # extract first name(s) after comma
        str_extract(
          # variable
          player_name,
          
          # pattern
          "(?<=, ).*"
        ),
        
        # extract last name before comma
        str_extract(
          # variable
          player_name,
          
          # pattern
          "^[^,]+"
        ),
        
        # separator
        sep = " "
      )
    )
  ) |> 
  # select columns
  select(
    name, 
    season,
    eye_color,
    ends_with("_diff")
  ) |>
  # get players
  filter(
    name %in% c("Austin Riley", "Marcus Semien", "Cody Bellinger")
  ) |> 
  # gt table
  gt(
    rowname_col = "name"
  ) |> 
  # align columns
  cols_align(
    align = "center"
  ) |> 
  # label columns
  cols_label(
    season = "Season",
    eye_color = "Eye Color",
    wOBA_diff = "wOBA Difference",
    launch_speed_diff = "Exit Velocity Difference",
    launch_angle_diff = "Launch Angle Difference",
    whiff_pct_diff = "Whiff % Difference",
    zone_contact_pct_diff = "Zone Contact % Difference"
  ) |> 
  # format numerical columns
  fmt_number(
    columns = c(
      launch_speed_diff,
      launch_angle_diff,
      whiff_pct_diff,
      zone_contact_pct_diff
    ),
    decimals = 2
  ) |> 
  # format wOBA
  fmt_number(
    columns = c(wOBA_diff),
    decimals = 3
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      eye_color
    ),
    # scale
    fn = scales::col_factor(
      palette = c("#603101", "#8E7618", "#a1caf1"), 
      domain = statcast_combined$eye_color
    )
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      wOBA_diff
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"), 
      domain = statcast_combined$wOBA_diff
    )
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      launch_speed_diff
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"), 
      domain = statcast_combined$launch_speed_diff
    )
  ) |> 
  # add color
  data_color(
    # columns
    columns = c(
      zone_contact_pct_diff
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("dodgerblue4", "white", "goldenrod"), 
      domain = statcast_combined$zone_contact_pct_diff
    )
  ) |> 
  # this needs to be in reverse since it is a defensive statistic (lower = better)
  data_color(
    # columns
    columns = c(
      whiff_pct_diff
    ),
    # scale
    fn = scales::col_numeric(
      palette = c("goldenrod", "white", "dodgerblue4"), 
      domain = NULL
    )
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**MLB Pitch-by-Pitch Aggregated Stats**"),
    subtitle = md("*Data: Baseball Savant*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Difference = Day - Night<br>
                  Gold = 'Good'<br>
                  Blue = 'Bad'*")
  ) |> 
  # theme
  gt_theme_espn()





# Linear Regression Table -------------------------------------------------

# gt table
linear_model |> 
  broom::tidy() |> 
  # mutate
  mutate(
    pred = c(
      "Intercept",
      "Hazel Eyes",
      "Blue Eyes",
      "Exit Velocity Difference",
      "Zone Contact % Difference",
      "Whiff % Difference"
    )
  ) |> 
  select(
    pred,
    estimate,
    std.error,
    statistic,
    p.value
  ) |> 
  gt() |> 
  # align columns
  cols_align(
    columns = c(pred),
    align = "left"
  ) |> 
  # label columns
  cols_label(
    pred = "Predictors",
    estimate = "Slope",
    std.error = "Std Error",
    statistic = "t-value",
    p.value = "p-value"
  ) |> 
  # round for scientific notation
  fmt_scientific(
    columns = c(
      estimate,
      std.error
    )
  ) |> 
  # round 3 decimals for t-values
  fmt_number(
    columns = c(
      statistic
    ),
    decimals = 3
  ) |> 
  # round 4 decimals for p-values
  fmt_number(
    columns = c(
      p.value
    ),
    decimals = 4
  ) |> 
  # color insignificant variables
  tab_style(
    style = cell_fill(color = "goldenrod"),
    locations = cells_body(
      columns = c(
        p.value
      ),
      rows = p.value > 0.49
    )
  ) |> 
  # color insignificant variables
  tab_style(
    style = cell_fill(color = "goldenrod"),
    locations = cells_body(
      columns = c(
        statistic
      ),
      rows = statistic < 1 & statistic > -1
    )
  ) |> 
  # title and subtitle
  tab_header(
    title = md("**Linear Regression Model Coefficients**")#,
    #subtitle = md("*Best Model: Logistic Regression*")
  ) |> 
  # footnote
  tab_footnote(
    footnote = md("*Note: Brown is the reference level for the eye color variable*")
  ) |> 
  # theme
  gt_theme_espn()
