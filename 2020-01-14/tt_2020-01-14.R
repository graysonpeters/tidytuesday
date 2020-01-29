# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------
# Grayson Peters
# 01/26/2020
# Tidy Tuesday 2020
# Week 3 - Password Strength
# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

library(tidytuesdayR)
library(tidyverse)
library(cowplot)
library(ggtext)
tuesdata <- tidytuesdayR::tt_load(2020, week = 3)
passwords <- tuesdata$passwords

# -----------------------------------------------------------------------------------

passwords$value.hr <- NA

passwords <- passwords[!is.na(passwords$value),]
passwords <- passwords[passwords$category != "rebellious-rude",] 

passwords$value.hr[passwords$time_unit == "seconds"] <- passwords$value[passwords$time_unit == "seconds"]/3600
passwords$value.hr[passwords$time_unit == "minutes"] <- passwords$value[passwords$time_unit == "minutes"]/60
passwords$value.hr[passwords$time_unit == "hours"] <- passwords$value[passwords$time_unit == "hours"]
passwords$value.hr[passwords$time_unit == "days"] <- passwords$value[passwords$time_unit == "days"]*24
passwords$value.hr[passwords$time_unit == "weeks"] <- passwords$value[passwords$time_unit == "weeks"]*168
passwords$value.hr[passwords$time_unit == "months"] <- passwords$value[passwords$time_unit == "months"]*720
passwords$value.hr[passwords$time_unit == "years"] <- passwords$value[passwords$time_unit == "years"]*8760

# -----------------------------------------------------------------------------------


# First plot - explores the time that it takes to crack common passwords by category

plot1 <- ggplot(passwords) +
  geom_density(mapping = aes(value.hr, fill = category, col = category)) + 
  facet_wrap(~category, labeller = as_labeller(c("animal" = "Animal", 
                                                 "cool-macho" = "Cool/Macho", 
                                                 "fluffy" = "Fluffy", 
                                                 "food" = "Food", 
                                                 "name" = "Name", 
                                                 "nerdy-pop" = "Nerdy/Pop", 
                                                 "password-related" = "Password-Related", 
                                                 "simple-alphanumeric" = "Simple Alphanumeric", 
                                                 "sport" = "Sporty"))) + 
  scale_x_continuous(trans = "log10") + 
  theme_classic() + 
  labs(x = "\nTime to Crack Password (hours)", 
       y = "", 
       title = "Time to Crack Common Passwords by Category") +
  theme(legend.position = "none", 
        text = element_text(color = "white", family = "mono"),
        plot.background = element_rect(fill = "black", color = "black"), 
        panel.background = element_rect(fill = "black"),
        strip.background = element_rect(fill = "black", color = "white"),
        strip.text = element_text(color = "white", face = "bold"),
        rect = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.line.y = element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.text.x = element_text(color = "white", size = 12), 
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_fill_manual(values = c("#FF000044", "#0022FF44", "#00FF0044", 
                               "#FF00FF44", "#FF880044", "#FFFF0044", 
                               "#FF994444", "#FF888844", "#FFFFFF44")) + 
  scale_color_manual(values = c("#FF0000",  "#0000FF",   "#00FF00", 
                                "#FF00FF",  "#FF8800",   "#FFFF00", 
                                "#BB6622",  "#FF6688",   "#CCCCCC"))

# -----------------------------------------------------------------------------------

# Among alphanumeric, how does the inclusion of numbers change things? 

alphanumeric <- passwords %>% 
  filter(.,category == "simple-alphanumeric") %>% 
  mutate(num = ifelse(str_detect(password, "[1234567890]"), 1, 0), 
         alpha = ifelse(str_detect(password, "[qwertyuiopasdfghjklzxcvbnm]"), 1, 0), 
         alphanum = num == 1 & alpha == 1, 
         type = ifelse(alphanum == T, "Alphanumeric", 
                       ifelse(num == 1, "Numeric Only", "Alphabetical Only")))
# Plot 2

plot2 <- ggplot(alphanumeric) + 
  geom_density(mapping = aes(value.hr, color = type, fill = type)) + 
  scale_x_continuous(trans = "log10") + 
  theme_classic() + 
  labs(x = "\nTime to Crack Password (hours)", 
       y = "Density\n", 
       title = "Time to Crack Simple Passwords Based on Type", 
       col = "", fill = "") +
  theme(legend.position = "bottom",
        rect = element_rect(fill = "black"),
        text = element_text(color = "white", family = "mono"), 
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.text = element_text(color = "white", size = 12), 
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_color_manual(values = c("red", "blue", "yellow"), 
                     labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only")) +
  scale_fill_manual(values = c("#FF000044", "#0000FF44", "#FFFF0033"), 
                               labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only"))
  
# -----------------------------------------------------------------------------------

passwords <- passwords %>% 
  mutate(length = str_length(password)) %>%
  mutate(num = ifelse(str_detect(password, "[1234567890]"), 1, 0), 
         alpha = ifelse(str_detect(password, "[qwertyuiopasdfghjklzxcvbnm]"), 1, 0), 
         alphanum = num == 1 & alpha == 1, 
         type = ifelse(alphanum == T, "Alphanumeric", 
                       ifelse(num == 1, "Numeric Only", "Alphabetical Only")))

# Third plot - same as plot 2 with all passwords instead of just the alphanumeric ones

plot3a <- ggplot(passwords) +
  geom_density(mapping = aes(value.hr, color = type, fill = type)) + 
  scale_x_continuous(trans = "log10") + 
  theme_classic() + 
  labs(x = "\nTime to Crack Password (hours)", 
       y = "Density\n", 
       title = "Time to Crack Passwords Based on Type", 
       col = "", fill = "") +
  theme(legend.position = "bottom",
        rect = element_rect(fill = "black"),
        text = element_text(color = "white", family = "mono"), 
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.text = element_text(color = "white", size = 12), 
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_color_manual(values = c("red", "blue", "yellow"), 
                     labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only")) +
  scale_fill_manual(values = c("#FF000044", "#0000FF44", "#FFFF0033"), 
                    labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only"))

# Among passwords, is difficulty perfectly scalable to letters? how does # of letters work

plot3b <-  ggplot(passwords) +
  geom_point(mapping = aes(x = length, y = value.hr, col = type)) +
  scale_y_continuous(trans = "log10") + 
  theme_classic() + 
  labs(x = "\nPassword Length", 
       y = "Time to Crack Password (hours)\n", 
       title = "Time to Crack Passwords Based on Length", 
       col = "", fill = "")+
  theme(legend.position = "bottom",
        rect = element_rect(fill = "black"),
        text = element_text(color = "white", family = "mono"), 
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.text = element_text(color = "white", size = 12), 
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_color_manual(values = c("red", "blue", "yellow"), 
                     labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only"))

# Is the strength measure a proxy for length and alphanumeric complexity? 
# A) alphanumeric complexity

plot4a <- ggplot(passwords) +
  geom_point(mapping = aes(y = strength, x = length, col = type)) +
  geom_smooth(mapping = aes(y = strength, x = length, col = type), 
                method = "lm", se = F, lwd = 0.5) +
  theme_classic() + 
  labs(x = "\nPassword Length", 
       y = "Password Strength\n", 
       title = "Password Strength Based on Length and Type",
       col = "", 
       fill = "") +
  theme(legend.position = "bottom",
        rect = element_rect(fill = "black"),
        text = element_text(color = "white", family = "mono"), 
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.text = element_text(color = "white", size = 12), 
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_color_manual(values = c("red", "blue", "yellow"), 
                     labels = c("Alphabetical Only", "Alphanumeric", "Numeric Only"))

# -----------------------------------------------------------------------------------

# is the difficulty n/10 and alpha/26 and aplhanum/36

plot4b <- passwords %>% 
  mutate(strength2 = ifelse(type == "Alphabetical Only", strength/26, 
                            ifelse(type == "Numeric Only", strength/10, strength/36))) %>% 
  ggplot() +
  geom_point(mapping = aes(y = strength2, x = length, col = type)) +
  geom_smooth(mapping = aes(y = strength2, x = length, col = type), 
              method = "lm", se = F, lwd = 0.5) +
  theme_classic() + 
  labs(x = "\nPassword Length", 
       y = "Normalized Password Strength\n", 
       title = "Password Strength Based on Length and Type",
       subtitle = "Normalized by # of Available Characters in Typeset",
       col = "", 
       fill = "") +
  theme(legend.position = "bottom",
        rect = element_rect(fill = "black"),
        text = element_text(color = "white", family = "mono"), 
        plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        axis.text = element_text(color = "white", size = 12), 
        axis.line = element_line(color = "white"),
        axis.ticks = element_line(color = "white"),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm")) + 
  scale_color_manual(values = c("red", "blue", "yellow"), 
                     labels = c("Alphabetical Only\n(Strength/26)", "Alphanumeric\n(Strength/36)", "Numeric Only\n(Strength/10)"))

# -----------------------------------------------------------------------------------
# -----------------------------------------------------------------------------------

# Arranging the plots into final layout

text <- ggdraw() + 
  draw_label(
    "Passwords can be alphabetical-only, \nalphanumeric, or numeric-only. \nPasswords can also be grouped \nthematically. While different \nthemes of passwords are not easier \nor more difficult to crack, different \ncompositions of passwords can lead to \nincreased security, even beyond the \nsimple fact that alphanumeric \nalphabets have 36 characters \ncompared to the 10 numerals and 26 letters. ",
    size = 14,
    fontface = "bold",
    fontfamily = "mono",
    color = "white",
    x = 0,
    hjust = 0
  ) +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm"))


row1 <- plot_grid(plot1, text, nrow = 1, rel_widths = c(0.6, 0.4))
row2 <- plot_grid(plot3a, plot3b, nrow = 1)
row3 <- plot_grid(plot4a, plot4b, nrow = 1)

title <- ggdraw() + 
  draw_label(
    "Does Changing the Type or Subject of Your Password Make It Stronger?",
    size = 24,
    fontface = "bold",
    fontfamily = "mono",
    color = "white",
    x = 0,
    hjust = 0
  ) +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm"))

caption <- ggdraw() + 
  draw_label(
    "Tidy Tuesday 2020, Week 3                 Data: Information is Beautiful                 @graysonpeters__",
    size = 14,
    fontfamily = "mono",
    color = "white",
    x = 0,
    hjust = -0.05
  ) +
  theme(plot.background = element_rect(fill = "black", color = "black"),
        panel.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16),
        plot.margin = margin(t = 0.5,b = 0.5,l = 0.5,r = 0.5, unit = "cm"))

plot_grid(title, row1, row2, row3, caption, ncol = 1, rel_heights = c(0.07, 0.29, 0.29, 0.29, 0.04))

ggsave(filename = "~/Desktop/Tidy Tuesday 2020/(3) 2020-01-14/Passwords.pdf", 
      width = 14, height = 14)
