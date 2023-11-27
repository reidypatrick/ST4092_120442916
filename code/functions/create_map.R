create_map <- function(.data = cars_orig, .col, .shape = shape) {
  # Defaults to using cars_orig and france shapefile
  # also defaults to summarising by the sum of specified column
  
  shape_copy <- .shape
  
  # TODO: create separate function for summarising:
  #      args: .data, group_by, fun(s), ...?
  # summarise desired column from cars data set
  cars_data <- .data %>%
    group_by(Region) %>%
    summarise(col = sum({{ .col }})) %>%
    mutate(Region = substr(Region, 2, 3))

  # join onto .shape data
  map_obj <- shape_copy@data %>%
    left_join(cars_data, join_by(CCA_1 == Region)) %>%
    dplyr::select(ID_1, col) %>%
    inner_join(sf::st_as_sf(shape_copy), join_by(ID_1))

  # plot according to desired column
  ggplot() +
    geom_sf(map_obj, 
            mapping = aes(fill = col, geometry = geometry), 
            colour = "white") +
    scale_fill_gradient(name = substitute(.col), labels = comma) +
    theme_void()
}
