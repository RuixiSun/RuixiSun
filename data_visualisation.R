library(tidyverse)
library(ggplot2)
mpg %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_point()

mpg_selected <- mpg %>%
  select(manufacturer, cty)
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_point() + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 0.5))
#hjust: horizontal justification; vsjust: vertical

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  geom_jitter(width = .2, alpha = .75, size = 2) + #alpha: darkness
  theme_minimal() + #background white, lines grey (if deleted, reverse)
  theme(axis.text.x = element_text(angle = 45, vjust = .5, hjust =  .5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)")

#adding summary statistics
install.packages("Hmisc")
library(Hmisc)
mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = manufacturer, y = cty)) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = 0.5) + 
  theme_minimal() + #cl - center line
  theme(axis.title.x = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)")

#re-order by the mean fuel economy of the manufacturer

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, 
             colour = manufacturer)) + #in .fun, mean/medium/mode determines the order of y axis
  stat_summary(fun.data = mean_cl_boot, size = 0.5) + #size of mean points and lines
  geom_jitter(alpha = .25) +
  theme_minimal() +
  theme(text = element_text(size = 11)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer",
       y = "City Fuel Economy (mpg)") +
  guides(colour = 'none') + #决定是否有图例(legend)
  coord_flip() #exchange x and y axis

#plot different pics for different types (similar to moderation effect)
#facet_wrap()

mpg %>%
  filter(class != "suv") %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = cty, colour = class)) +
  geom_jitter(width = .2) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Engine Displacement",
       x = "Engine Displacement (liters)",
       y = "City Fuel Economy (mpg)") +
  guides(color = 'none') +
  facet_wrap(~class) #means plot a picture for each class


#scatterplots - two numerical variables

mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(color = class)) +
  geom_smooth(se = TRUE) + #TURE: includes a CI line? & ONLY works for y~x
  theme(text = element_text(size = 13)) +
  theme_minimal() +
  labs(x = "City Fuel Economy (mpg)",
       y = "Engine Displacement (liters)",
       color = "Vehicle Class") 

#plotting histograms
#histograms of engine sizes (displ)
mpg %>%
  ggplot(aes(x = displ)) +
  geom_histogram(#aes(y=..density..),
    binwidth = .5, color = "black", fill = "grey") + #binwidth-width of each square
  #geom_density(alpha=.2, fill="white") + #add density line;a nice color "#FF6666"
  theme_minimal() +
  labs(title = "Histogram of Engine Displacement",
       x = "Engine Displacement (litres)", 
       y = "Count")

#compare the distribution of variable A (engine size) by B (vehicle class)
#ggridge()
install.packages("ggridges")
library(ggridges)
mpg %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) + #order y(class) based on the mean of x(displ)
  geom_density_ridges(height = .5, aes(fill = class)) + #the fill color differ according to class
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  #guides(fill = "none")+
  labs(x = "Engine Displacement (litres)",
       y = NULL,
       fill = "Vehicle Class")

#Discover the NHANES Dataset
install.packages("NHANES")
library(NHANES)
ncol(NHANES)
nrow(NHANES) 
head(NHANES)
###Tidy the data - some participants appear more than once
NHANES %>%
  select(ID) %>%
  n_distinct()
###remove duplicate IDs
NHANES_tidied <- NHANES %>%
  distinct(ID, .keep_all = TRUE)
nrow(NHANES_tidied)

### plot histogram for BMI
NHANES_tidied %>%
  ggplot(aes(x = BMI)) +
  geom_histogram(bins = 100, color = "black", fill = "grey",
                 na.rm = TRUE) +
  theme_minimal() 

### BMI as a function of educational level

NHANES_tidied %>%
  group_by(Education) %>%
  summarise(median = median(BMI, na.rm = TRUE))

### Plot the means of var A (BMI, y axis) by B (Education,x axis) & C(diabetes, x) 其实是Education*Diabetes

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI) & !is.na(Diabetes)) %>%
  ggplot(aes(x = Education:Diabetes, y = BMI, color = Education)) +
  geom_violin() +
  geom_jitter(alpha = .2, width = .1) +
  geom_boxplot(alpha = .5) + #aplha: transparency of the box,higher-less transparent
  guides(color = 'none') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  labs(title = "Examining the effect of education level and diabetes on BMI",
       x = "Education Level × Diabetes",
       y = "BMI")


### Histograms for BMI according to educational level

## y axis = number of cases

#try on my own
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  ggplot(aes(x = BMI, y = fct_reorder(Education, .fun = mean, BMI))) + #order y(class) based on the mean of x(displ)
  geom_density_ridges(height = .5, aes(fill = Education)) + #the fill color differ according to class
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  #guides(fill = "none")+
  labs(x = "BMI",
       y = NULL,
       fill = "Education")

#follow instructions
NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  #group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram() +
  guides(fill = 'none') +
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI",
       y = "Number of cases") + 
  facet_wrap(~Education, scales = "free") #scales="free" allows the change in values of each y axis

## y axis = density

NHANES_tidied %>%
  filter(!is.na(Education) & !is.na(BMI)) %>%
  group_by(Education) %>%
  ggplot(aes(x = BMI, fill = Education)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density(aes(y = ..density..), fill="white", alpha = .5) +
  guides(fill = 'none') +
  labs(title = "Examining the effect of education level on BMI",
       x = "BMI",
       y = "Density") +
  facet_wrap(~Education)


