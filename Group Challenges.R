library(tidyverse)
library(Hmisc) #to use rcorr()
library(performance)

crime <- read_csv("https://raw.githubusercontent.com/ajstewartlang/09_glm_regression_pt1/master/data/crime_dataset.csv")
head(crime)
crime <- separate(crime, col = "City, State", into = c("City", "State"))
head(crime)
crime <- crime %>%
  rename(House_price = index_nsa) %>%
  rename(Violent_Crimes = `Violent Crimes`)
head(crime)

#remember to deal with the missing data!!!
crime_filtered <- filter(crime, Year == 2015) %>%
  filter(!is.na(Violent_Crimes) & !is.na(House_price))
crime_filtered %>%
  ggplot(aes(x=Violent_Crimes, y=House_price)) +
  geom_point() +
  #this layer is to plot the City names and ensure the lables don't overlap
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal() +
  theme(text = element_text(size=13)) +
  labs(x = "Voilent Crimes",
       y = "House Price")

model1 <- lm(House_price~1, data = crime_filtered)
model2 <- lm(House_price ~ Violent_Crimes, data = crime_filtered)
anova(model1,model2)
summary(model2)
check_model(model2, panel = FALSE)



