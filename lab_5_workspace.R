
# Load packages -----------------------------------------------------------

library(tidyverse)

# Print Data --------------------------------------------------------------

iris
iris <- as_tibble(iris)
iris

# Question 1 --------------------------------------------------------------

iris <- rename(iris, sepal_length = Sepal.Length, sepal_width = Sepal.Width, petal_length = Petal.Length, petal_width = Petal.Width, species = Species)
iris

# Question 2 --------------------------------------------------------------

iris2 <- mutate(iris, sepal_length_cm = sepal_length * 10, sepal_width_cm = sepal_width * 10, petal_length_cm = petal_length * 10, petal_width_cm = petal_width * 10)
iris2 <- select(iris2, species, sepal_length_cm, sepal_width_cm, petal_length_cm, petal_width_cm)
iris2

# Question 3 --------------------------------------------------------------

iris3 <- mutate(iris, sepal_area = sepal_length * sepal_width, petal_area = petal_length * petal_width)
iris3 <- select(iris3, sepal_area, petal_area, species)
iris3

# Question 4 --------------------------------------------------------------

summary4 <- summarise(iris, sampl_size = n(), max_value = max(sepal_length), min_value = min(sepal_length), range = max_value - min_value, median = median(sepal_length), q1 = quantile(sepal_length, probs = 0.25), q3 = quantile(sepal_length, probs = 0.75), iqr = q3 - q1)
summary4

# Question 5 --------------------------------------------------------------

iris_grouped <- group_by(iris, species)
summary5 <- summarise(iris_grouped, sampl_size = n(), mean_petal_width = mean(petal_width), sd_petal_width = sd(petal_width), variance = var(petal_width), sem = mean_petal_width / sqrt(sampl_size), ci_lower = mean_petal_width - 2 * sem, ci_upper = mean_petal_width + 2 * sem)
summary5

# Question 6 --------------------------------------------------------------

ggplot(data = iris) +
  geom_jitter(mapping = aes(x = species, y = petal_length))  +
  labs(x = "Species", y = "Petal Length (mm)")

# Question 7 --------------------------------------------------------------

iris_summary <-
  summarize(
    iris_grouped, 
    mean_petal_length = mean(petal_length),
    sem = sd(petal_length) / sqrt(n()),
    upper_limit = mean_petal_length + 1.96 * sem,
    lower_limit = mean_petal_length - 1.96 * sem
  )
ggplot(data = iris) +
  geom_jitter(mapping = aes(x = species, y = petal_length)) +
  geom_crossbar(
    data = iris_summary, 
    mapping = aes(x = species, y = mean_petal_length, ymax = upper_limit, ymin = lower_limit),
    color = "red") +
    labs(x = "Species", y = "Petal Length (mm)")

# Question 8 --------------------------------------------------------------

ggplot(data = iris) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.8) + labs(x = "Petal Length (mm)", y = "Petal Width (mm)", color = "Species") 

ggplot(data = iris) +
  geom_point(mapping = aes(x = petal_length, y = petal_width, color = species), alpha = 0.8) + labs(x = "Petal Length (mm)", y = "Petal Width (mm)", color = "Species") +
  scale_y_continuous(breaks = seq(0, 0.5, 3), limits = c(0, 3),
                     expand = expansion(mult=0)) +
  scale_x_continuous(breaks = seq(0, 1, 7), limits = c(1, 7))
