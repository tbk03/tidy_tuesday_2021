library(tidyverse)
library(tidycensus)
library(lubridate)


# *********************************************************
# IMPORT DROUGHT DATA
# *********************************************************

# define column types of dataset
col_types <- "nnccnnnnnnDDn"

droughts <- read_csv("20210720/dm_export_20200721_20210721.csv",
                     col_types = col_types) %>%
  janitor::clean_names()

# visual check for missing data
visdat::vis_miss(droughts, warn_large_data = FALSE)

# clean and tidy data
droughts_tidy <- droughts %>%

  # update column types that couldn't be mamaged in data import
  mutate(map_date = ymd(map_date)) %>%

  # tidy format
  pivot_longer(cols = none:d4,
               names_to = "drought_level",
               values_to = "num_of_people") %>%

  rename(geoid = fips)


# *********************************************************
# IMPORT SHAPEFILES FOR USA GEOGRAPHIES (STATES AND COUNTIES)
# *********************************************************

data(state_laea)
data(county_laea)

# clean spatial dataframe which stores the shape file for state and county boundarie
counties <- county_laea %>%
  rename(geoid = GEOID) %>%
  mutate(geoid = as.numeric(geoid))

# *********************************************************
#
# *********************************************************

# focus on most recent data
latest_map <- droughts_tidy %>%
  filter(map_date == ymd(20210713))

# join drought data and geospatial polygons
counties_drought <- counties %>%
  left_join(latest_map)

# deal with nas created by joining (only four counties)
counties_drought_clean <- counties_drought %>%
  spread(key = drought_level, value = num_of_people) %>%

  mutate(across(d0:none, ~ replace_na(.x, median(.x, na.rm = TRUE)))) %>%

  gather(key = "drought_level", value = "num_of_people",
         d0:none)

# split the counties into those with/without people living in extreme drought conditions
counties_except_dr <- counties_drought_clean %>%
  filter(drought_level == "d4") %>%
  filter(num_of_people > 0) %>%
  mutate(nop_group = cut(num_of_people, breaks = c(0,1e3, 1e4, 1e5, 1e6, Inf)))

counties_wo_except_dr <- counties_drought_clean %>%
  filter(drought_level == "d4") %>%
  filter(num_of_people == 0)

# split the counties into those with/without people living in extreme drought conditions
states_people_totals <- counties_drought_clean %>%
  filter(drought_level == "d4") %>%
  group_by(state) %>%
  summarise(num_of_people = sum(num_of_people))

state_except_dr <- states_people_totals %>%
  filter(num_of_people > 0)

state_wo_except_dr <- states_people_totals %>%
  filter(num_of_people == 0)

# set colour scale for binned data
fill_colours <- c("#feefd9", "#e7c09b", "#d29067", "#bd5e3d", "#a51d21")

ggplot() +

  # add counties WITHOUT exceptional drought (low emphasis)
  geom_sf(data = counties_wo_except_dr, colour = "grey90", fill = "grey90") +

  # add counties WITH exceptional drought (high emphasis)
  geom_sf(data = counties_except_dr,
          mapping = aes(fill = nop_group, colour = nop_group)) +

  # add states WITH exceptional drought (low emphasis)
  geom_sf(data = state_except_dr, alpha = 0, colour = "white") +
  geom_sf_text(data = state_except_dr, mapping = aes(label = state),
               colour = "white", fontface = "bold") +

  # apply fill colour gradients
  scale_fill_manual(values = fill_colours) +
  scale_colour_manual(values = fill_colours) +
  #scale_fill_gradient(low = "#FEEFD9", high = "#A51D21", trans = "log") +
  #scale_colour_gradient(low = "#FEEFD9", high = "#A51D21", trans = "log") +
  # scale_fill_viridis_c(trans = "log", option = "magma", direction = -1) +
  # scale_colour_viridis_c(trans = "log", option = "magma", direction = -1) +

  ggthemes::theme_map() +
  theme(legend.position = "top")

ggsave("20210720/drought_map.svg", units = "mm", width = 163.3, height = 93.3)


state_except_dr %>%
  mutate(nop_group = cut(num_of_people, breaks = c(0,1e3, 1e4, 1e5, 1e6, Inf))) %>%

  ggplot(aes(x = 0, y = num_of_people, colour = nop_group)) +

  geom_point(aes(size = num_of_people), alpha = 0.5) +
  geom_text(aes(label = state)) +

  scale_x_continuous(breaks = c(0,0), expand = c(0,0)) +

  scale_y_log10() +

  scale_colour_manual(values = fill_colours) +
  theme_light()

ggsave("20210720/drought_strip.svg", units = "mm", width = 163.3, height = 93.3)

counties_except_dr %>%
  slice_max(order_by = num_of_people, n = 10)

counties_except_dr %>%
  summarise(total = sum(num_of_people))

# View(latest_map)
# View(counties_drought)
#
# 46113 	Shannon 29203
# 51515 	Bedford City 51019
# 02261 	Valdez-Cordova
# 02270 	Wade Hampton
#
# View(fips_codes)

# ggplot() +
#   geom_sf(data = county_laea) +
#   geom_sf(data = state_laea, alpha = 0, colour = "red") +
#
#   ggthemes::theme_map()


