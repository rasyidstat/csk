library(tidyverse)
library(sf)
library(nusantr)
library(mrsq)
library(units)

csk <- read_sf("data/csk.geojson")
csk_river <- read_sf("data/csk_river.geojson")
point <- tibble(latitude = -6.3252265,
                longitude = 106.6414905) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
point2 <- tibble(latitude = -6.3249982,
                 longitude = 106.6407625) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
point3 <- tibble(latitude = -6.325958,
                 longitude = 106.643638) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
circle <- point %>%
  st_transform(32619) %>%
  st_buffer(set_units(2.5, km)) %>%
  st_transform(4326)
krl_line <- krl_route %>%
  filter(line_id == "Serpong line",
         is_main)

p <- csk %>%
  st_intersection(circle) %>%
  mutate(highway = ifelse(highway %in% c("residential",
                                         "pedestrian",
                                         "bus_stop",
                                         "service",
                                         "track",
                                         "living_street",
                                         "footway",
                                         "bus_guideway",
                                         "unclassified",
                                         "path",
                                         "raceway"),
                          NA_character_, highway)) %>%
  ggplot() +
  geom_sf(data = csk_river %>%
            st_intersection(circle),
          color = "steelblue",
          fill = "steelblue") +
  geom_sf(aes(color = highway), show.legend = FALSE) +
  geom_sf(data = krl_line %>%
            st_intersection(circle),
          color = "grey40",
          linetype = "dashed") +
  geom_sf(data = point2,
          color = "red",
          size = 3,
          alpha = 0.7) +
  geom_sf_text(data = point3,
               aes(label = "Stasiun Cisauk"),
               family = "Nunito",
               size = 4) +
  coord_sf(datum = NA) +
  theme_nunito() +
  scale_color_discrete(na.value = "grey90") +
  labs(x = NULL, y = NULL)
ggsave("figs/csk.png", width = 8, height = 8, dpi = 200)

p <- csk %>%
  st_intersection(circle) %>%
  ggplot() +
  geom_sf(data = csk_river %>%
            st_intersection(circle),
          color = "steelblue",
          fill = "steelblue") +
  geom_sf(aes(color = highway), show.legend = FALSE) +
  geom_sf(data = krl_line %>%
            st_intersection(circle),
          color = "grey40",
          linetype = "dashed") +
  geom_sf(data = point2,
          color = "red",
          size = 3,
          alpha = 0.7) +
  geom_sf_text(data = point3,
               aes(label = "Stasiun Cisauk"),
               family = "Nunito",
               size = 4) +
  coord_sf(datum = NA) +
  theme_nunito() +
  scale_color_viridis_d(na.value = "grey90") +
  labs(x = NULL, y = NULL)
ggsave("figs/csk-viridis.png", width = 8, height = 8, dpi = 200)
