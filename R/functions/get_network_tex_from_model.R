get_network_tex_from_model <- function(model,
                                       margin = 0,
                                       h_scale = 1,
                                       v_scale = 1) {
  c(get_network_tex(
    n_input = model$model$layers[[1]]$input_shape[[2]],
    n_hidden1 = model$model$layers[[2]]$input_shape[[2]],
    n_hidden2 = model$model$layers[[3]]$input_shape[[2]],
    margin = margin,
    h_scale = h_scale,
    v_scale = v_scale
  ))
}
