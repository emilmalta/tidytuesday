# Load libraries ---------------------------------------------------------------

library(ggbeeswarm)
library(tidyverse)
library(lubridate)
library(statgl)
library(plotly)

# Import data ------------------------------------------------------------------

passwords_raw <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-14/passwords.csv')

# Declare helper function ------------------------------------------------------

num_uniques <- function(string) {
  string %>% 
    map(function(string) unique(str_split(string, "")[[1]])) %>% 
    map_int(length)
}

# Tidy/transform ---------------------------------------------------------------

passwords <- passwords_raw %>% 
  drop_na() %>% 
  unite(crack_time, value, time_unit, sep = " ") %>% 
  mutate(crack_time_  = time_length(crack_time, unit = "second"),
         pass_length = str_length(password),
         pass_unique = num_uniques(password),
         category = category %>% 
           str_replace_all("-", " ") %>% 
           str_to_title)

# Visualise --------------------------------------------------------------------

# Which categories are the most popular?
passwords %>% 
  count(category, sort = T)

passwords %>% 
  mutate(category = fct_infreq(category) %>% fct_rev) %>% 
  ggplot(aes(x = category, y = rank, color = category, group = category,
             text = paste("Password:", password, "\nRank:", rank))) +
  geom_quasirandom(size = 0.75, width = 0.3) +
  coord_flip() +
  scale_y_reverse() +
  theme_statgl() +
  theme(legend.position = "none") +
  scale_color_statgl() +
  labs(title = "183 of the 500 most popular passwords are names",
       x = "", y = "Rank") ->
  password_rank

interactive_plot <- 
  ggplotly(password_rank, tooltip = "text")

interactive_plot

# Does password length == safe password? 

qplot(log(crack_time_), data = passwords) # Approx log normal
qplot(strength, data = passwords, binwidth = 1)
qplot(pass_length, strength, data = passwords)

passwords %>% 
  mutate(pass_length = factor(pass_length),
         hi_there = format(crack_time, scientific = FALSE),
         alpha_indicator = case_when(
           category == "Simple Alphanumeric" ~ "Alphanumeric password",
           T ~ "Literally anything else"
         )) %>% 
  ggplot(aes(x = pass_length, y = offline_crack_sec, 
             color = alpha_indicator,
             text = paste("Password:", password, "\nTime to crack:", crack_time)
  )) +
  geom_quasirandom(alpha = 0.8) +
  scale_y_log10(labels = scales::comma_format(suffix = " sec")) +
  theme_statgl() +
  theme(legend.position = "bottom") +
  labs(x = "Password length (characters)",
       y = "Time to crack by online guessing",
       color = "",
       caption = "Log scale") ->
  time_to_crack

ggplotly(time_to_crack, tooltip = "text")

passwords %>% 
  mutate(category = fct_reorder(category, crack_time_) %>% fct_rev) %>% 
  ggplot(aes(x = category, y = log(crack_time_), color = category)) +
  geom_quasirandom() +
  coord_flip() +
  theme_statgl()
