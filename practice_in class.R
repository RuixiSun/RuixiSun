library(tidyverse)
starwars %>%
  #filter(!is.na(height)) %>%
  filter(species == "Human") %>%
  summarise(mean_height = mean(height, na.rm = TRUE))

starwars %>%
  summarise(mean_height= mean(height, na.rm=TRUE))

starwars %>%
  group_by(gender) %>%
  summarise_at(c("height"), mean, na.rm = TRUE) %>%
  


starwars %>%
  group_by(gender) %>%
  summarise(mean_height= mean(height,na.rm= TRUE)) %>%
  arrange(mean_height)


library(tidyverse)
birth_year <- starwars %>%
  group_by(films) %>%
  summarise(mean_birthyear = mean(birth_year, na.rm = TRUE)) %>%
  arrange(mean_birthyear) 
view(birth_year)

library(tidyverse)
storms_recoded <- storms %>%
  #distinct(status) %>%
  mutate(status = recode(status,
                          "tropical depression" = "1",
                          "tropical storm" = "2",
                          "hurricane" = "3"))
storms %>%
  group_by(status) %>%
  summarise(number = n())

storms %>%
  group_by(status) %>%
  summarise(pressure_mean = mean(pressure, na.rm = TRUE))

storms %>%
  filter(year > 2000) %>%
  summarise_at(c("wind","pressure"), mean, na.rm = TRUE)

storms %>%
  select(year, wind, pressure)

storms %>%
  mutate(year = str_to_title(year), model = str_to_upper(model)) %>%