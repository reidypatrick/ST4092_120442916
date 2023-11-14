create_map <- function(.data, .col, .shape){
  shape_copy <- .shape
  
  # summarise desired column from cars data set
  cars_data <- .data %>% 
    group_by(Region) %>% 
    summarise(col = sum({{.col}})) %>% 
    mutate(Region = substr(Region, 2,3))
  
  # cars_data <- cars %>% 
  #   group_by(Region) %>% 
  #   summarise(col = sum(ClaimNb)) %>% 
  #   mutate(Region = substr(Region, 2,3))
  
  # join onto .shape data
  map_obj <- shape_copy@data %>%
    left_join(cars_data, join_by(CCA_1 == Region)) %>%
    dplyr::select(ID_1, col) %>%
    inner_join(sf::st_as_sf(shape_copy), join_by(ID_1))
  
  # plot according to desired column
  ggplot() + 
    geom_sf(map_obj, mapping = aes(fill = col, geometry = geometry), colour = "white") +
    scale_fill_gradient(name = substitute(.col)) +
    theme_void()
  }
