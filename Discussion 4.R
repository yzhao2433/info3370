library(tidyverse)
library(mgcv)
library(splines)
parents <- read_csv("https://info3370.github.io/data/parents.csv")


grouped_data <- parents |>
  group_by(child_age, sex) |>
  count(at_work) |>
  mutate (percent = n / 100) |> 
  filter(at_work == 1, sex == "female")

lm <- lm(percent ~ child_age, data = grouped_data)
summary(lm)

penalized_reg <- gam(
  percent ~ s(child_age, bs = "re")
              ,data = grouped_data
  )
summary(penalized_reg)

spline_reg <- lm(
  percent ~ bs(child_age, degree = 1, knots = c(.4,.6)),
  data = grouped_data
)

child_age <- c(1)
to_predict <- data.frame(child_age)

all_predicted$lmpredicted = predict(lm, 
                                    newdata = to_predict)

all_predicted$gampredicted = predict(penalized_reg, 
                                newdata = to_predict)

all_predicted$splinepredicted = predict(spline_reg, 
                                     newdata = to_predict)

# grouped_data2 <- parents |>
#   group_by(child_age, sex) |>
#   count(at_work) |>
#   mutate (percent = n / 100) |> 
#   filter(at_work == 1)
# 
# lm2 <- lm(percent ~ child_age + sex, data = grouped_data)
# to_predict2 <- data.frame(child_age = 1, sex = "female")




