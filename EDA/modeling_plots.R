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



# Random Forest Plot ------------------------------------------------------

tibble(
  variable = c(
    "Eye Color",
    "Exit Velocity Difference",
    "Launch Angle Difference",
    "Zone Contact % Difference",
    "Whiff % Difference"
  ),
  var_imp = eye_color_rf_full_model$variable.importance * 100
) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = var_imp,
      # y axis
      y = fct_reorder(variable, var_imp)
    )
  ) +
  # geom col
  geom_col(
    # color
    color = "black",
    # fill
    fill = "#002D72"
  ) +
  # labels
  labs(
    x = "Variable Importance",
    y = "Predictor",
    title = "Random Forest Variable Importance",
    caption = "Sampled to have same amount of blue- and brown-eyed players"
  ) +
  # custom theme
  mlb_plot_theme()
