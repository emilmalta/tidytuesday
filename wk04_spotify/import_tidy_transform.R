# Load libraries ---------------------------------------------------------------

library(ggbeeswarm)
library(tidyverse)
library(lubridate)
library(statgl)

# Import data ------------------------------------------------------------------

# Download data
spotify_songs_raw <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

# Helper functions -------------------------------------------------------------

# Finds the most common element of vector
most_common <- function(vec){
  names(which.max(table(vec)))
}

# Tidy/transform ---------------------------------------------------------------

# Songs (Tracks can have duplicates)
spotify_songs <- spotify_songs_raw %>% 
  mutate(
    release_date = ymd(track_album_release_date),
    duration = milliseconds(duration_ms)) %>% 
  select(-duration_ms, track_album_release_date)

# Tracks are in multiple playlists/genres. Just pick the most common:
track_genres <- spotify_songs %>% 
  group_by(track_id) %>% 
  summarise(
    genre = most_common(playlist_genre)
  )

# And left join it on everything except playlist:
tracks <- spotify_songs %>% 
  distinct_at(vars(-starts_with("playlist"))) %>% 
  left_join(track_genres) %>% 
  select_if(is.numeric) %>% 
  select(-key, -mode) 
