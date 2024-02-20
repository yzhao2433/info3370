library(tidyverse)
library(scales)
baseball <- read_csv("https://info3370.github.io/data/baseball.csv")

Salary <- baseball$salary

p <- hist(
  x = Salary,
  main = "Salary of All 944 MLB Players on Opening Day 2023",
  xlab = "Annual Salary"
)

# New function called simple_sample
simple_sample <- function(population) {
  population |>
    # Define sampling probability and weight
    mutate(
      p_sampled = 60 / n(),
      sampling_weight = 1 / p_sampled
    ) |>
    # Sample 60 players
    sample_n(size = 60)
}

sample <- simple_sample(population = baseball)


stratified_sample <- function(population) {
  population |>
    # Draw sample within each team
    group_by(team) |>
    # Define sampling probability and weight
    mutate(
      p_sampled = 2 / n(),
      sampling_weight = 1 / p_sampled
    ) |>
    # Within each team, sample 2 players
    sample_n(size = 2)
}

# Clustered Sampling
clustered_sample <- function(population) {
  
  # First, sample 3 teams
  sampled_teams <- population |>
    # Make one row per team
    distinct(team) |>
    # Sample 3 teams
    sample_n(3) |>
    # Store those 3 team names in a vector
    pull()
  
  # Then load data on those teams and sample 20 per team
  population |>
    # Logical comparison: %in%  
    filter(team %in% sampled_teams) |>
    # Define sampling probability and weight
    group_by(team) |>
    mutate(
      p_sampled = (3 / 30) * (20 / n()),
      sampling_weight = 1 / p_sampled
    )
  # Sample 20 players
  sample_n(20) |>
    ungroup()
}

estimator <- function(sample) {
  sample |>
    summarize(estimate = weighted.mean(
      x = salary, 
      w = sampling_weight
    )) |>
    pull(estimate)
}

sample_example <- simple_sample(population = baseball)
sampe <- estimator(sample = sample_example)

strat_example <- stratified_sample(population = baseball)
strate <- estimator(sample = strat_example)

clus_example <- clustered_sample(population = baseball)
ce <- estimator(sample = clustered_sample)

# REPEAT THE  CODE 
sample_estimates <- replicate(
  n = 1000,
  expr = {
    a_sample <- simple_sample(population = baseball)
    estimator(sample = a_sample)
  }
)

# By law of large number, even if the population is non-normal, sampled enough of
# population --> regarless of method, gets you a normal distribution and closer
# to true mean