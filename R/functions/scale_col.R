scale_col <- function(.col) {
  (.col - min(.col)) / (max(.col) - min(.col))
}
