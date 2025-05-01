## Liam Jennings
## Honors Capstone


# Libraries and Functions -------------------------------------------------

## libraries
library(tidyverse)
library(lme4)
library(rstanarm)
library(bayesrules)
library(tidybayes)
library(ggdist)


# Initial Hierarchical Model ----------------------------------------------

hierarchical_eye_color <- lmer(
  wOBA_diff ~ 
    eye_color + 
    # control for plate appearance imblanace
    scale(PA_day) + scale(PA_night) +  
    # random effects for year and player
    ## controls for player baselines and year-specific factors
    (1|player_name) + (1|season),
  data = year_statcast
)


hierarchical_eye_color


# Filter PA ---------------------------------------------------------------

# Filter for sufficient PA
analysis_data <- year_statcast |> 
  filter(PA_day >= 50, PA_night >= 50)

# Final model
final_model <- lmer(
  wOBA_diff ~ eye_color + 
    scale(log(PA_day)) + scale(log(PA_night)) +
    (1|player_name),
  data = analysis_data
)

summary(final_model)




# Hierarchical Model with Random Effects for Player and Year --------------

## fit model
hierarchical_model <- stan_lmer(
  wOBA_diff ~ 
    eye_color + scale(PA_day) + scale(PA_night) + 
    (1 | player_name) + (1 | season),
  data = statcast_combined,
  prior = normal(0, 0.05, autoscale = TRUE),  # Weakly informative for eye_color
  prior_intercept = normal(0, 0.1),
  prior_covariance = decov(reg = 1, conc = 1, shape = 1, scale = 1),
  chains = 4, iter = 4000
)


## model interpretation

### check prior
prior_summary(
  hierarchical_model
)


## posterior samples
posterior <- hierarchical_model |> 
  as_tibble()

### analyze results
eye_color_effects <- posterior |> 
  # calculate eye color effects
  summarize(
    hazel = mean(eye_colorHazel),
    brown = mean(eye_colorBrown),
    hazel_ci = quantile(eye_colorHazel, c(0.025, 0.975)),
    brown_ci = quantile(eye_colorBrown, c(0.025, 0.975))
  )


## visualize posterior distributions
plot_normal_normal(
  mean = mean(posterior$eye_colorBrown),
  sd = sd(posterior$eye_colorBrown),
  y_bar = 0,
  sigma = 0.05,
  n = 525
) + 
  labs(title = "Brown-Eyed Effect on wOBA Diff")


## practical significance check
mean(posterior$eye_colorBrown > 0.005)  



## validation

### posterior predictive checks
pp_check(hierarchical_model, plotfun = "stat", stat = "mean")


## cross-validation
# loo_compare(loo(model), loo(interaction_model))




# Factor in Difference for Blue Eyed Players ------------------------------

## change reference level for eye color
combined_statcast <- statcast_combined |>
  mutate(
    eye_color =
      relevel(
        factor(
          eye_color
        ),
        ref = "Blue"
      )
  )


## rerun model to interpret coefficients relative to blue-eyed players
model_blue_ref <- update(
  hierarchical_model, 
  data = combined_statcast
)


## extract posterior distributions

### get all eye color effects
eye_effects <- model_blue_ref |> 
  spread_draws(
    `eye_color.*`, 
    regex = TRUE
  ) |> 
  rename_with(
    ~ gsub("eye_color", "", .x), 
    starts_with("eye_color")
  )

### calculate contrasts if needed
contrasts <- eye_effects |> 
  mutate(
    Brown_vs_Blue = Brown,
    Hazel_vs_Blue = Hazel
  )



## visualize comparisons
eye_effects |> 
  pivot_longer(
    cols = c(Brown, Hazel), 
    names_to = "Eye_Color"
  ) |> 
  ggplot(
    aes(x = value, y = Eye_Color)
  ) +
  stat_halfeye(
    fill = "#a1caf1"
  ) +
  geom_vline(
    xintercept = 0, 
    linetype = 2
  ) +
  labs(
    x = "wOBA Difference (vs Blue-Eyes)", 
    y = ""
  ) +
  mlb_plot_theme()


## direct probability statements
### posterior samples of blue-eyed average (intercept)
blue_effect <- as.data.frame(model_blue_ref)$`(Intercept)`

### probability of meaningful effect (> 0.005)
mean(blue_effect > 0.005)

### 95% Credible Interval
quantile(blue_effect, c(0.025, 0.975))



## compare all groups
### newdata
new_data <- expand_grid(
  eye_color = c("Blue", "Brown", "Hazel"),
  PA_day = mean(year_statcast$PA_day),
  PA_night = mean(year_statcast$PA_night),
  player_name = unique(year_statcast$player_name)[1],  # Single representative player
  season = unique(year_statcast$season)[1]  # Single representative season
)
  


### get posterior predictions by eye color
predictions <- model_blue_ref |> 
  epred_draws(
    newdata = new_data,
    re_formula = NA  # Critical: Remove random effects
  )



# Plot comparison
predictions |> 
  ggplot(aes(x = .epred, y = eye_color)) +
  stat_halfeye() +
  labs(x = "Predicted wOBA Night-Day Difference", y = "")


## critical validation
identical(
  levels(new_data$eye_color), 
  levels(model_blue_ref$data$eye_color)  # Must match
)



## player-adjusted predictions
new_data <- combined_statcast |> 
  distinct(player_name, season) |> 
  expand_grid(eye_color = c("Blue", "Brown", "Hazel")) |> 
  mutate(
    PA_day = mean(year_statcast$PA_day),
    PA_night = mean(year_statcast$PA_night)
  )

predictions <- model_blue_ref |> 
  epred_draws(newdata = new_data) |> 
  group_by(eye_color) |> 
  median_qi(.epred)  # Collapse player-specific effects


predictions |> 
  ggplot(aes(x = .epred, y = eye_color)) +
  stat_halfeye() +
  labs(x = "Predicted wOBA Night-Day Difference", y = "")
