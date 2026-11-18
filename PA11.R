
# PA11.R
# James Davon Austin
# 11/18/2026
# Analyzes vehicle weight based on the number of cylinders.

library(tidyverse)
library(modelr)

# Create a tibble with cylinders and weight.
cars <- as_tibble(mtcars) %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

# Fit a linear regression model.
m <- lm(wt ~ cyl, data = cars)

# Create a data grid and calculate predicted weights.
grid <- cars %>%
  data_grid(cyl) %>%
  add_predictions(m)

# Plot observed and predicted vehicle weights.
ggplot(cars, aes(x = cyl, y = wt)) +
  geom_point() +
  geom_point(data = grid, aes(y = pred), color = "red", size = 4)

# Analysis:
# Vehicles with more cylinders generally weigh more.
# The predicted values represent the average weight for each
# cylinder category. Eight-cylinder vehicles have the highest
# predicted weight, while four-cylinder vehicles have the lowest.
