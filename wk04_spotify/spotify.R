# ALGORHYTHM :_: WHAT MAKES A SONG POPULAR ON SPOTIFY?

# Load libraries ---------------------------------------------------------------

library(gghighlight)
library(tidyverse)
library(lubridate)
library(ggforce)
library(statgl)
library(skimr)
library(reshape2)

# Set constants ----------------------------------------------------------------

theme_set(theme_minimal())

# Import data ------------------------------------------------------------------

spotify_songs_raw <- 
  read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv')

spotify_songs_raw %>% 
  skim()


# Tidy/transform ---------------------------------------------------------------
spotify_songs <- spotify_songs_raw %>% 
  mutate(date = ymd(track_album_release_date),
         weekday = wday(date, week_start = 1, label = TRUE),
         duration = milliseconds(duration_ms))

# Duplicates? ------------------------------------------------------------------

# Yes!
spotify_songs %>% 
  distinct(track_id)

# Seems to be different playlists / whatever
spotify_songs %>% 
  add_count(track_id) %>% 
  filter(n > 1) %>% 
  arrange(track_id) 

# Visualise distributions ------------------------------------------------------

# How are ratings distributed?
spotify_songs %>% 
  ggplot(aes(x = track_popularity)) +
  geom_histogram(binwidth = 1)

# So many 0's...
spotify_songs %>% 
  filter(track_popularity == 0)


# How many samples in each genre?
spotify_songs %>% 
  ggplot(aes(x = playlist_genre, fill = playlist_genre, alpha = playlist_subgenre)) +
  geom_bar() +
  theme(legend.position = "none") +
  scale_alpha_discrete(range = c(0.15, 1))

# Heavily time skewed -> new songs are overrepresented
spotify_songs %>% 
  ggplot(aes(x = date)) +
  geom_histogram()

# How popular is each subgenre? Definite patterns within each:
spotify_songs %>% 
  ggplot(aes(x = playlist_subgenre, y = track_popularity,
             color = playlist_genre)) +
  geom_sina(size = 0.1) +
  facet_wrap(~ playlist_genre, scales = "free_x")

# How long are the songs across genres?
spotify_songs %>% 
  ggplot(aes(x = playlist_subgenre, y = duration,
             color = playlist_genre)) +
  geom_sina(size = 0.1) +
  scale_y_time(labels = scales::time_format("%M:%S")) +
  facet_wrap(~ playlist_genre, scales = "free_x")

# Is there a difference between genres? ----------------------------------------

# Looks like it
spotify_songs %>%
  lm(track_popularity ~ playlist_genre, data = .) %>% 
  anova()

# Is there an obvious predictor? -----------------------------------------------

spotify_songs %>% 
  mutate_at(c("acousticness", "instrumentalness"), log1p) %>% 
  pivot_longer(cols = danceability:duration_ms, names_to = "predictor") %>% 
  ggplot(aes(x = value)) +
  geom_histogram() +
  facet_wrap(~ predictor, scales = "free")

# Smoothing predictors to response
spotify_songs %>% 
  pivot_longer(cols = danceability:duration_ms, names_to = "predictor") %>% 
  ggplot(aes(x = value, y = track_popularity)) +
  geom_point(size = 0.01) +
  geom_smooth(method = "lm") +
  facet_wrap(~ predictor, scales = "free")

# How does the correlation look? -----------------------------------------------

spotify_songs %>% 
  select(danceability:duration_ms) %>% 
  cor() %>% 
  ggcorrplot::ggcorrplot(method = "circle", type = "upper", hc.order = TRUE) +
  labs(title = "Correlation matrix",
       x = "", y = "") +
  theme_minimal()

# Fit a linear model -----------------------------------------------------------

# Something like this, idunno?
spotify_songs %>% 
  lm(track_popularity ~ danceability + energy + acousticness + 
       instrumentalness + liveness + tempo, data =.) %>% 
  summary()

# Miscellaneous ----------------------------------------------------------------

# More releases on fridays for some reason
spotify_songs %>% 
  ggplot(aes(x = weekday, fill = case_when(weekday == "Fri" ~ "Friday"))) +
  geom_bar() +
  coord_polar(start = - pi / 8) +
  theme(legend.position = "none") +
  facet_wrap(~ playlist_genre) +
  labs(title = "Release on fridays!",
       subtitle = "Fridays are by far the most popular weekday for album releases")

# Maybe do the radar thing?
spotify_songs %>% 
  select(playlist_genre, track_popularity, danceability:duration_ms, -mode, -key) %>% 
  group_by(playlist_genre) %>% 
  top_n(100, track_popularity) %>% 
  select(-track_popularity) %>% 
  mutate_all(scales::rescale) %>% 
  summarise_all(median, na.rm = T) %>% 
  ungroup() %>% 
  pivot_longer(-playlist_genre, names_to = "feature") %>% 
  ggplot(aes(x = feature, y = value, color = playlist_genre, group = playlist_genre)) +
  geom_point() +
  geom_line() +
  coord_polar(theta = "x") +
  facet_wrap(~playlist_genre)

spotify_songs %>% 
  distinct(track_id, .keep_all = TRUE) %>% 
  arrange(desc(track_popularity)) %>% 
  head(20) %>% 
  select(track_name, danceability:duration_ms, -mode, -key) %>% 
  group_by(track_name) %>% 
  mutate_all(scales::rescale)

# TODO: Model all the things!