# Author: Ying Lin Zhao 

library(tidyverse)
library(mgcv)
library(rsample)

train_df <- read.csv("learning.csv")
split <- initial_split(train_df, prop = .7)
test_df <- read.csv("holdout_public.csv")

y <- train_df$g3_log_income

# Plotting a chart to visualize correlation between the income of parent and child
# There are some extremely low values for child income, so perhaps trying the
# penalized regression would be helpful.

# Also, there does not seem to be a huge correlation between child income with
# parent income, education of the child seemed to matter more according to the chart.
# However, among those with the same education levels, those with a higher parent
# income seems to also earn more.
train_df |> ggplot(aes(
  x = g2_log_income,
  y = g3_log_income,
  color = factor(g3_educ)
)) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE)


# Observing correlation between parent log income and parent education to see if
# interaction term is necessary
p_income <- quantile(train_df$g2_log_income)
# Lowest log parent income is 8.7448
# 50th percentile of log parent income is 11.4432
# Highest log parent income is 12.9332

train_df$PIncome_range <- c()
# Anything lower than 25th percentile
train_df$PIncome_range[train_df$g2_log_income <= p_income[2]] <- "0-25"
train_df$PIncome_range[train_df$g2_log_income > p_income[2] &
  train_df$g2_log_income <= p_income[3]] <- "25-50"
train_df$PIncome_range[train_df$g2_log_income > p_income[3] &
  train_df$g2_log_income <= p_income[4]] <- "50-75"
train_df$PIncome_range[train_df$g2_log_income > p_income[4]] <- "75-100"

# Conditional Table - P(Income | Educ)
prop.table(table(train_df$PIncome_range, train_df$g2_educ), 2)
# Although there are some correlations between college and earning above 75th
# percentile and between less than high school and earning below 25 percentile.
# The other values of the table is not very significant for there to be a
# conclusion made that there is a correlation between these two factors.


# First model: Income ~ Parent income + Parent Education + Sex + Race
# Included both parent income and parent education because as shown by the prop.
# table, there is not a strong or very obvious correlation between the two variables.
lm1 <- lm(
  g3_log_income ~ as.factor(sex) + as.factor(race) +
    as.factor(g2_educ) + g2_log_income,
  data = training(split)
)

summary(lm1)

# MSE for training data (first model) - 0.4374
X1 <- select(training(split), sex, race, g2_log_income, g2_educ)
lm1_pred <- predict(lm1, newdata = X1)
# select keeps info as a data frame while pull keeps it as a vector
y_obs <- pull(training(split), g3_log_income)
mse_train <- mean((y_obs - lm1_pred)^2)

# MSE for testing data (first model) - 0.4631
X1 <- select(testing(split), sex, race, g2_log_income, g2_educ)
lm1_pred <- predict(lm1, newdata = X1)
# select keeps info as a data frame while pull keeps it as a vector
y_obs <- pull(testing(split), g3_log_income)
mse <- mean((y_obs - lm1_pred)^2)

# The plot shows that not additional transformation is needed, given the
# residuals follow an random distribution
# plot(resid(lm1))
# abline(a = 0, b = 0, col = "red", lty = 2)


# Second model: Income ~ Parent Income + Sex + Race + Education
# How would it change we replaced parent's education with the child's education,
# which is a factor that is likely to have more of a direct influence on child's
# income?
lm2 <- lm(
  g3_log_income ~ as.factor(g3_educ) + as.factor(sex) +
    as.factor(race) + g2_log_income,
  data = training(split)
)

summary(lm2)

# MSE for training data (second model) - 0.4149
X2 <- select(training(split), g3_educ, sex, race, g2_log_income)
lm2_pred <- predict(lm2, newdata = X2)
y_obs <- pull(training(split), g3_log_income)
mse2_train <- mean((y_obs - lm2_pred)**2)

# MSE for testing data (second model) - 0.4329
X2 <- select(testing(split), g3_educ, sex, race, g2_log_income)
lm2_pred <- predict(lm2, newdata = X2)
y_obs <- pull(testing(split), g3_log_income)
mse2 <- mean((y_obs - lm2_pred)**2)

# The plot shows that not additional transformation is needed, given the
# residuals follow an random distribution
# plot(resid(lm2))
# abline(a = 0, b = 0, col = "red", lty = 2)


# Third model: All (Generally does better than 1st model in testing data but not
# training data in terms of MSE)
lm3 <- lm(g3_log_income ~ g2_log_income + g1_log_income +
            g3_educ + g2_educ + g1_educ + 
            race + sex,
          data = training(split))

summary(lm3)

# MSE for training data (third model) - 0.4330
X3 <- select(
  training(split), g2_log_income, g1_log_income,
  g3_educ, g2_educ, g1_educ, race, sex
)
lm3_pred <- predict(lm3, newdata = X3)
y_obs <- pull(training(split), g3_log_income)
mse3_train <- mean((y_obs - lm3_pred)**2)

# MSE for training data (third model) - 0.4573
X3 <- select(
  testing(split), g2_log_income, g1_log_income,
  g3_educ, g2_educ, g1_educ, race, sex
)
lm3_pred <- predict(lm3, newdata = X3)
y_obs <- pull(testing(split), g3_log_income)
mse3 <- mean((y_obs - lm3_pred)**2)


# Fourth model: None (Highest MSE among all 4 models)
lm4 <- lm(g3_log_income ~ 1, data = training(split))

summary(lm4)

# MSE for training data (fourth model) - 0.5380
lm4_pred <- predict(lm4, newdata = training(split))
y_obs <- pull(training(split), g3_log_income)
mse4_train <- mean((y_obs - lm4_pred)**2)

# MSE for training data (fourth model) - 0.5593
lm4_pred <- predict(lm4, newdata = testing(split))
y_obs <- pull(testing(split), g3_log_income)
mse4 <- mean((y_obs - lm4_pred)**2)


# Resource used to address errors with gam: https://www.youtube.com/watch?v=8doPTpkAWDQ
# Fifth model -- Penalized Regression of 2nd Model
lm5 <- gam(
  g3_log_income ~ g3_educ + sex +
    race + s(g2_log_income), bs = "re",
  data = training(split)
)

# MSE for training data (fifth model) - 0.4441
X5 <- select(training(split), g3_educ, sex, race, g2_log_income)
lm5_pred <- predict(lm5, newdata = X5)
y_obs <- pull(training(split), g3_log_income)
mse5_train <- mean((y_obs - lm5_pred)**2)

# MSE for testing data (fifth model) - 0.3577
X5 <- select(testing(split), g3_educ, sex, race, g2_log_income)
lm5_pred <- predict(lm5, newdata = X5)
y_obs <- pull(testing(split), g3_log_income)
mse5 <- mean((y_obs - lm5_pred)**2)


# Sixth Model -- Penalized Regression of 3rd model
lm6 <- gam(g3_log_income ~ s(g2_log_income) + s(g1_log_income) +
            g3_educ + g2_educ + g1_educ + 
            race + sex, bs = "re",
          data = training(split))

summary(lm6)

# MSE for training data (sixth model) - 0.4122
X6 <- select(
  training(split), g2_log_income, g1_log_income,
  g3_educ, g2_educ, g1_educ, race, sex
)
lm6_pred <- predict(lm6, newdata = X6)
y_obs <- pull(training(split), g3_log_income)
mse6_train <- mean((y_obs - lm6_pred)**2)

# MSE for training data (sixth model) - 0.3928
X6 <- select(
  testing(split), g2_log_income, g1_log_income,
  g3_educ, g2_educ, g1_educ, race, sex
)
lm6_pred <- predict(lm6, newdata = X6)
y_obs <- pull(testing(split), g3_log_income)
mse6 <- mean((y_obs - lm6_pred)**2)


# Best Model: Model 2 (Income ~ Parent Income + Sex + Race + Education)
# Consistently performs best based on the MSE and AIC values.
X_best <- select(test_df, g3_educ, sex, race, g2_log_income)
test_df$g3_log_income <- predict(lm2, newdata = X_best)

# Creation of csv
results <- data.frame(test_df$g3_id, test_df$g3_log_income)
write.csv(results, "13ForTheWin(yz2433_ah859)", row.names = FALSE)
