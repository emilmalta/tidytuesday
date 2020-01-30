# Load libraries ---------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(osmdata)
library(janitor)
library(statgl)
library(skimr)

# Import data ------------------------------------------------------------------

sf_trees_raw <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-28/sf_trees.csv')

# Tidy/transform ---------------------------------------------------------------

sf_trees <- sf_trees_raw %>%
  filter(longitude > -135, latitude %>% between(37.6, 40)) %>% 
  mutate(caretaker = case_when(
    caretaker != "Private" ~ "Public",
    T ~ "Private"
  )) %>% 
  st_as_sf(coords = c("longitude", "latitude")) %>% 
  st_set_crs(4326)

# Quantize map -----------------------------------------------------------------

# Make grid
sf_grid <- sf_trees %>% 
  st_make_grid(n = 40) 

# Calculate number of trees in grid
trees_in_grid <- sf_grid %>% 
  st_intersects(sf_trees) %>% 
  lengths()

# Combine to a single one
bubbles <- sf_grid %>% 
  st_centroid() %>%
  st_coordinates() %>% 
  cbind(trees_in_grid) %>% 
  as_tibble() %>% 
  arrange(trees_in_grid) %>% 
  st_as_sf(coords = c("X", "Y")) %>% 
  st_set_crs(4326)

# Import San Fran street data --------------------------------------------------

sf_roads <- sf_grid %>% 
  st_bbox() %>% 
  opq() %>% 
  add_osm_feature("highway") %>% 
  osmdata_sf() %>%
  `$`("osm_lines") 

# Visualise --------------------------------------------------------------------

ggplot() +
  geom_sf(
    data = sf_roads,
    color = "#A13D2D", size = .2, alpha = .25
    ) +
  geom_sf(
    data = bubbles,
    aes(size = trees_in_grid + .1),
    pch = 21, fill = "#36802d", color = "#f5f5f5", alpha = .9
    ) +
  scale_size_area(max_size = 14) +
  theme_void(
    base_family = "RobotoCondensed-Regular", 
    base_size = 36
    ) +
  theme(
    plot.background = element_rect(fill = "#f5f5f5"),
    legend.position = "none") +
  labs(title = "Trees of San Francisco", 
       caption = "Source: data.sfgov.org | #TidyTuesday")

# Save -------------------------------------------------------------------------

ggsave(
  here::here("wk05_sftrees/trees.png"), 
  width = 11.9, height = 15, dpi = 600
  )
