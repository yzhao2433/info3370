library(tidyverse)
library(ggrepel)
data <- read_csv(file = "https://info3370.github.io/data/jencks_table1.csv")
data |>
  ggplot(mapping = aes(x = ratio, y = gdp)) +
  geom_point() +
  scale_x_continuous(name = "Inequality\n90th percentile / 10th percentile of household income") +
  scale_y_continuous(name = "GDP as a Percent of U.S.", labels = scales::percent) +
  geom_text_repel(aes(label = data$country)) +
  geom_smooth(method = lm, se = FALSE) +
  theme_bw()
