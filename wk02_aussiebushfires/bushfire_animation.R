# Load libraries ---------------------------------------------------------------

library(rnaturalearth)
library(tidyverse)
library(extrafont)
library(gganimate)
library(lubridate)
library(maps)
library(sf)


# Import data ------------------------------------------------------------------

nasa_fire_raw <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-07/MODIS_C6_Australia_and_New_Zealand_7d.csv')

# Helper function for stretching map -------------------------------------------

mold <- function(df, lonlabel, latlabel){
  df %>% 
    st_as_sf(coords = c(lonlabel, latlabel)) %>% 
    st_set_crs(4326) %>% 
    st_transform(3577) %>% 
    cbind(., st_coordinates(.)) %>% 
    as_tibble() %>% 
    select(-geometry)
}

# Map of Australia -------------------------------------------------------------

# Map outline
australia <- ne_states(geounit = "Australia", returnclass = "sf") %>% 
  st_transform(3577)

# Cities
cities <- world.cities %>% filter(country.etc == "Australia") %>% 
  arrange(desc(pop)) %>% 
  mold(lonlabel = "long", latlabel = "lat")

# Transform map data -----------------------------------------------------------

# gganimate won't animate sf's properly, do some converting:
nasa_fire <- nasa_fire_raw %>% 
  mutate(id = row_number(),
         datetime = ymd_hm(paste(acq_date, acq_time))) %>% 
  select(-starts_with("acq")) %>% 
  mold(lonlabel = "longitude", latlabel = "latitude")

# Visualise fire ---------------------------------------------------------------

# Time range for animation
timerange <- nasa_fire %>% pull(datetime) %>% range()

# The magic:
animated_fire <- ggplot() +
  geom_sf(data = australia, fill = "#FBF7F5", color = "#B4A19D") +
  lims(x = c(790000, 2050000), y = c(-4500000, -3000000)) +
  geom_text(data = cities, size = 2, color = "#B4A19D",
            check_overlap = T,
            mapping = aes(x = X, y = Y, label = name)) +
  geom_point(
    data = nasa_fire %>% filter(confidence >= 50), 
    mapping = aes(x = X, y = Y, group = id, color = brightness),
    color = "orange", 
    shape = ".") +
  transition_components(
    datetime,
    range = timerange,
    enter_length = as_datetime(hm("12:0")),
    exit_length  = as_datetime(hm("12:0"))
  ) +
  enter_fade() +
  exit_fade() +
  shadow_trail(colour = "#400000", alpha = alpha / 8) +
  theme_minimal(base_family = "RobotoCondensed-Regular", base_size = 18) +
  theme(legend.position = "none") +
  labs(title = "Australian bushfires", 
       subtitle = "{format(frame_time, '%B %d %Y %H:%00')}",
       caption  = "Source: NASA Active Fires dataset, TidyTuesday",
       x = "", y = "") +
  scale_color_manual(values = c("8B0000", "#FE9B29"))

# Export -----------------------------------------------------------------------

anim_save(
  "bushfire_animation.gif", 
  animate(animated_fire, width = 750, height = 1000), 
  path = "wk02_aussiebushfires/"
)

