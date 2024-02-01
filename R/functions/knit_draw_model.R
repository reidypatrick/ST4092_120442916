knit_draw_model <- function(model, name) {
  draw_network_from_model(model, name)
  knitr::include_graphics(paste("data/figures/", name, ".pdf", sep = ""))
}