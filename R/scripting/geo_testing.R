library(stringi)
library(sf)
library(raster)
library(dplyr)
library(ggplot2)
library(broom)
library(maps)
library(ggspatial)
library(tidyverse)
library(raster)
library(gridExtra)

# creating csv for tableau ----
geo_loc <- read_csv("data/input/France_Regions_data.csv")

geo_loc %>%
  select(`Region Name`) %>%
  distinct() %>%
  View()
reg_dict <- read_csv("data/input/regions_FR.csv", col_names = FALSE) %>%
  mutate(
    code = paste0("R", X2),
    region_name = toupper(X1)
  ) %>%
  select(-X2, -X1) %>%
  mutate(region_name = stri_trans_general(
    str = region_name,
    id = "Latin-ASCII"
  )) %>%
  mutate(
    region_name =
      ifelse(startsWith(region_name, "CENTRE"), "CENTRE", region_name)
  )


# ggplot and shapefile testing ----


wd <- file.path("C:/Users/patos/Documents/ST4092_120442916")
dsn <- file.path(wd, "data/input/shapefile")

shape <- shapefile(file.path(dsn, "FRA_adm1.shp"))
plot(shape)

shape_copy <- shape

# replaced by function ----
shape_copy@data <- shape@data %>%
  left_join(claim_number_map_data)

map_obj <- shape_copy@data %>%
  select(ID_1, ClaimNb) %>%
  mutate(id = as.character(ID_1)) %>%
  inner_join(sf::st_as_sf(shape_copy), join_by(ID_0))

claim_map <- sf::st_as_sf(shape_copy)

ggplot() +
  geom_polygon(
    data = map_obj,
    aes(x = long, y = lat, group = group, fill = ClaimNb),
    color = "white"
  ) +
  scale_fill_continuous(
    name = "Number of Claims",
    low = "lightblue", high = 6
  ) +
  theme_void()

ggplot() +
  geom_polygon(
    data = map_obj,
    aes(x = long, y = lat, group = group, fill = ClaimNb),
    color = "black"
  ) +
  theme_void()

coeffs <- glm1$coefficients

length(coeffs[substr(names(coeffs), 1, 6) == "Region"])

wd <- file.path("C:/Users/patos/Documents/ST4092_120442916")
dsn <- file.path(wd, "data/input/shapefile")
shape <- shapefile(file.path(dsn, "FRA_adm1.shp"))
claim_map <- create_map(cars, ClaimNb, shape)
density_map <- create_map(cars, Density, shape)
grid.arrange(claim_map, density_map, ncol = 2)
plot(density_map)
density_map
