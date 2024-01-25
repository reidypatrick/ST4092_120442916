# Load the library
library(DiagrammeR)

# Create a simple function to plot the model architecture
plot_keras_model <- function(model) {
  graph <- create_graph()
  
  # Add nodes for each layer
  for (layer in seq_along(model1$layers)) {
    for (node in seq_along(model1$layers[[layer]]$output[[2]])) {
      layer_name <- model1$layers[[layer]]$name
      node_name <- paste(layer_name, node)
      graph <- graph %>%
        add_node(label = node_name)
    }
    
    
    # Connect layers
    for (i in 2:length(model1$layers)) {
      graph %>%
        add_edge(from = model$layers[[i-1]]$name, to = model$layers[[i]]$name)
    }
  }
  # Plot the graph
  render_graph(graph)
}

# Call the function with your Keras model
plot_keras_model(model1)

render_graph(graph)

for (i in seq_along(model1$layers)) {
  model1$layers[[i]]$input_shape[[2]] %>% print()
  model1$layers[[i]]$output_shape[[2]] %>% print()
}

graph

graph <- create_graph()


for (node in seq_along(model1$layers[[1]]$input[[2]])) {
  layer_name <- model1$layers[[1]]$name
  node_name <- paste(layer_name, node, sep = "_")
  graph <- graph %>%
      add_node(label = node_name)
}

for (node in seq_along(model1$layers[[1]]$output[[2]])) {
  layer_name <- model1$layers[[2]]$name
  node_name <- paste(layer_name, node, sep = "_")
  graph <- graph %>%
    add_node(label = node_name)
  for (node1 in seq_along(graph$nodes_df)){
    graph <- graph %>% 
      add_edge(from = node, to = node1)
  }
}

render_graph(graph)

graph$nodes_df

graph <- create_graph()

graph %>% get_node_df()

graph <- graph %>% 
  add_node(label="1") %>% 
  add_node(label="2") %>% 
  add_node(label="3") 

render_graph(graph)
