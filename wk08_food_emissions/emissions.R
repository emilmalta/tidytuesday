# Load libraries ---------------------------------------------------------------

library(rnaturalearth)
library(tidyverse)
library(ggforce)
library(patchwork)
library(ggchicklet)
library(WDI)
library(janitor)
library(statgl)
library(fuzzyjoin)
library(countrycode)

# Import data ------------------------------------------------------------------

food_consumption_raw <- 
  readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')

pop_world <-
  WDI(indicator = "SP.POP.TOTL", start = 2018, end = 2018) %>% 
  as_tibble() %>% 
  clean_names()

gdp_world <-
  WDI(indicator = "NY.GDP.PCAP.CD", start = 2018, end = 2018, extra = TRUE) %>% 
  as_tibble() %>% 
  clean_names()

# Tidy/transform ---------------------------------------------------------------

world_indicators <-
  pop_world %>% 
  full_join(gdp_world, by = c("iso2c", "country")) %>% 
  arrange(desc(sp_pop_totl)) %>% 
  select(-country, -starts_with("year")) %>% 
  filter(income != "Aggregates")

income_levels <- 
  c("Low income", "Lower middle income", "Upper middle income", "High income")

food_consumption <- 
  food_consumption_raw %>% 
  mutate(iso2c = countrycode(country, origin = "country.name", destination = "iso2c")) %>% 
  left_join(world_indicators, by = "iso2c") %>% 
  drop_na() %>% 
  mutate(
    food_category = food_category %>% word() %>% 
      fct_reorder(co2_emmission, sum, na.rm = T), 
    income = factor(income,  levels = income_levels) %>% fct_rev()
  )

# Visualise --------------------------------------------------------------------

p1 <- 
  food_consumption %>% 
  drop_na() %>%
  ggplot(aes(
    x = food_category, y = co2_emmission, 
    group = food_category, size = sp_pop_totl, fill = income, label = country)
    ) +
  geom_sina(pch = 21) +
  scale_y_log10(labels = scales::unit_format(suffix = " kg", accuracy = .1)) +
  scale_size_area(max_size = 8, breaks = c(1000000, 100000000), labels = scales::comma) +
  expand_limits(y = 0) +
  scale_fill_statgl(palette = "grey") +
  labs(x = "", y = "Annual CO2 emission per person (log scale)", 
       title = "Beef",
       subtitle = "CO2 emissions of 123 countries by food group") +
  theme_minimal() +
  theme(
    axis.ticks = element_blank(),
    axis.text.x.bottom = element_blank(),
    panel.grid.minor = element_blank(),axis.title.y = element_text(hjust = 0)
  ) +
  labs(
    title = "Carbon footprint of food",
    fill = "Income", size = "Population of country"
  )


p2 <- (food_consumption %>% 
  distinct(food_category) %>% 
  ggplot(aes(food_category, y = 1)) +
  geom_chicklet(width = .5) +
  geom_text(
    aes(label = food_category, x = food_category, y = .95, angle = 270),
    nudge_x = .025, hjust = 0, size = 4.5, color = "white", family = "RobotoCondensed-Regular"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank()
  ) +
  labs(x = "", y = "", caption = "Source: nu3 and WDI | #TidyTuesday"))

# Patchwork --------------------------------------------------------------------

design <- "
A
A
A
A
B
"

p1 / p2 +
  plot_layout(design = design)

