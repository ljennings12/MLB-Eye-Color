## Liam Jennings
## Honors Capstone


# Libraries and Functions -------------------------------------------------

library(nflreadr)
library(tidyverse)
library(patchwork)
library(gt)
library(gtExtras)


## create theme
nfl_bar_theme <- function(){
  # theme
  theme(
    # adjust plot title
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    # adjust plot subtitle
    plot.subtitle = element_text(size = 16, face = "bold", hjust = 0.5),
    # adjust plot caption
    plot.caption = element_text(size = 10),
    # # adjust x axis title
    # axis.title.x = element_text(size = 14, face = "bold", hjust = 0.5),
    # it's obvious what the x-axis is so we remove the title
    axis.title.x = element_blank(),
    # adjust y axis title
    axis.title.y = element_text(size = 14, face = "bold", hjust = 0.5),
    # # adjust x axis text
    # axis.text.x = element_text(size = 12, hjust = 0.5),
    # adjust y axis text
    axis.text.y = element_text(size = 12, hjust = 0.5),
    # adjust legend position
    legend.position = "bottom",
    # adjust legend title text
    legend.title = element_text(size = 14, face = "bold", hjust = 0.5),
    # adjust legend text
    legend.text = element_text(size = 12, hjust = 0.5),
    # replacement of team abbreviations with logos
    axis.text.x = nflplotR::element_nfl_logo(size = 1),
    # adjust the strip text size
    strip.text = element_text(face = "bold", size = rel(1.5), color = "white"),
    # adjust the strip text background color
    strip.background = element_rect(fill = "#013369", color = "black", linewidth = 1),
  )
}


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



# General Trends -------------------------------------------------------------------

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
  # custom theme
  mlb_plot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )



## xwOBA by time of daya
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
  # custom theme
  mlb_plot_theme() +
  theme(
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
  # custom theme
  mlb_plot_theme()



## eye color bar chart
eye_colors |> 
  # have eye color as a factor
  mutate(
    eye_color = factor(eye_color)
  ) |> 
  # count by eye color
  count(
    eye_color
  ) |> 
  # mutate
  mutate(
    prop = n / sum(n)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = fct_reorder(eye_color, prop),
      # y axis
      y = prop
    )
  ) +
  # column
  geom_col() +
  # scale y axis
  scale_y_continuous(
    # breaks
    breaks = seq(0, 0.70, 0.10),
    # limits
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # labels
  labs(
    x = "Eye Color",
    y = "Proportion",
    title = "Eye Color Breakdown"
  ) +
  # custom theme
  mlb_plot_theme() 



## combined eye color bar chart
eye_colors |> 
  # have eye color as a factor
  mutate(
    eye_color = recode(
      eye_color,
      "Gray" = "Blue",
      "Green" = "Hazel",
      "Amber" = "Brown"
    ),
    
    eye_color = factor(
      eye_color,
      # levels 
      levels = c(
        "Blue",
        "Hazel",
        "Brown"
      )
    )
  ) |> 
  # count by eye color
  count(
    eye_color
  ) |> 
  # mutate
  mutate(
    prop = n / sum(n)
  ) |> 
  # plot
  ggplot(
    aes(
      # x axis
      x = eye_color,
      # y axis
      y = prop,
      # color
      fill = eye_color
    )
  ) +
  # column
  geom_col(
    color = "black"
  ) +
  # scale y axis
  scale_y_continuous(
    # breaks
    breaks = seq(0, 0.70, 0.10),
    # limits
    # convert to a percentage
    labels = scales::percent_format(accuracy = 1)
  ) +
  # scale color
  scale_fill_manual(
    values = c("#0D5176", "#8E7618", "#603101")
  ) +
  # labels
  labs(
    x = "Eye Color",
    y = "Proportion",
    title = "Eye Color Breakdown"
  ) +
  # custom theme
  mlb_plot_theme() +
  # remove legend
  theme(
    legend.position = "none"
  )
