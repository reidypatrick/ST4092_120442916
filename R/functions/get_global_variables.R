get_global_variables <- function(setting, gv = global_variables) {
  gv[[substitute(setting)]]
}
