library(tidyverse) # Load the tidyverse packages
library(Hmisc) # Needed for correlation
library(MASS) # Needed for maths functions
library(car) # Needed for VIF calculation
library(olsrr) # Needed for stepwise regression (decide on the best model based on R square change and p value)
library(performance) # Needed to check model assumptions

coffee_ratings <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

coffee_filtered <- filter(coffee_ratings, flavor != "0")

ggplot(coffee_filtered, aes(x = aroma, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = aftertaste, y = flavor)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  geom_jitter() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = acidity, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = body, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = balance, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = sweetness, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = uniformity, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = clean_cup, y = flavor)) +
  geom_point() +
  geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

ggplot(coffee_filtered, aes(x = moisture, y = flavor)) +
  geom_point() +
  #geom_jitter(width = 1, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) + #without se = FALSE, a grey area of se upper and below the regression line was drawn
  theme_minimal() +
  theme(text = element_text(size = 13))

model0 <- lm(flavor ~ 1, data = coffee_filtered)
model1 <- lm(flavor ~ aroma + aftertaste + acidity + body + balance + uniformity + clean_cup + sweetness + moisture, data = coffee_filtered)
anova(model0,model1)
check_model(model1, panel = FALSE)
summary(model1)

steplimitsf <- step(model0, scope = list (lower = model0, upper = model1), direction = "forward")
summary(steplimitsf)

steplimtsb <- step(model1, direction = "back")
summary(steplimtsb)

steplimitsboth <- step(model1, scope = list(upper = model1), direction = "both")

pmodel <- ols_step_forward_p(model1)
pmodel

