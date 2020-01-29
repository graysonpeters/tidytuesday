# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# Grayson Peters
# 01/08/2019
# Tidy Tuesday 2020
# Week 2 - Australian Fires
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

remotes::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)
library(tidyverse)
library(lubridate)
library(gganimate)
tuesdata <- tidytuesdayR::tt_load(2020, week = 2)
rainfall <- tuesdata$rainfall
temperature <- tuesdata$temperature
dta1 <- tuesdata$IDCJAC0009_009151_1800_Data

# -----------------------------------------------------------------------------------

# Going to try gganimate for the first time! 
# I'm going to try to animate a pretty simple plot that tracks temperature through major 
# Australian cities from 1910 to 2020. 

temp.year <- temperature %>% 
  mutate(year = year(date)) %>% 
  group_by(city_name, year, temp_type) %>% 
  summarise(temp.c = round(mean(temperature, na.rm = T), 1)) %>% 
  mutate(temp.f = round(temp.c*9/5 + 32, 1))

anim <- ggplot(data = temp.year) + 
  geom_bar(mapping = aes(x = city_name, y = temp.f, fill = temp_type), 
           stat = "identity", position = "dodge", width = 0.8) + 
  labs(title = "Average Temperature for Major Australian Cities", 
       subtitle = "Tidy Tuesday 2020, Week 2\nYear: {closest_state}", 
       y = "Temperature (°F)\n", 
       fill = "", 
       caption = "Years: 1910 - 2019\n Data: Australian Bureau of Meteorology") + 
  coord_cartesian(ylim = c(0,100)) +
  scale_fill_manual(values = c("#EEA837", "#639063"), labels = c("Maximum", "Minimum")) + 
  theme(
    axis.title.x.bottom = element_blank(), 
    axis.ticks = element_blank(),
    axis.text.x = element_text(color = "black", size = 10),
    axis.text.y = element_text(color = "black", size = 8),
    axis.title.y.left = element_text(family = "sans", size = 10), 
    plot.title = element_text(size = 18),
    plot.subtitle = element_text(size = 10, face = "italic"),
    plot.caption = element_text(size = 10, face = "italic"),
    legend.text = element_text(size = 10),
    legend.position = "bottom", 
    plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm"),
    panel.background = element_rect(fill = "white", color = "white"), 
    panel.grid.major.y = element_line(color = "grey50", size = 0.3), 
    panel.grid.minor.y = element_line(color = "grey5", size = 0.075)) + 
  gganimate::transition_states(year, 
                               transition_length = 0.5, 
                               state_length = 1)

animate(anim, nframes = 300,)

anim_save(filename = "~/Desktop/Tidy Tuesday 2020/(2) 2020-01-07/animation.gif")

# Days per year with temperature over X 