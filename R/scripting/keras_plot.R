py_view_model(model1)

library(deepviz)

plot_model(model1)

model1$layers[[1]]$name
model1$layers[[1]]$output_shape
model1$weights[[1]][[1]]
length(model1$weights[[1]][[1]])

# Weights ----------------------------------------------------------------------
model1$weights # Length 6,

## Matrices
seq_along(nrow(model1$weights[[1]])) # input layer: 43 matrices (vectors?), each size 16
model1$weights[[2]] # ? length 16
model1$weights[[3]] # Hidden layer 1: 16 matrices each size 16
model1$weights[[4]] # ? length 16
model1$weights[[5]] # Hidden layer 2
model1$weights[[6]] # ? length 16

# Odd indexed weight matrices are kernel regularisers, these act on the weights
#   this penalises the weights that are large and causing the network to
#   overfit.
# Even indexed weight matrices are bias regularisers, these act on the biases
#   this adds a bias so that our bias approaches zero

# are these the weights or the regularisers that act on the weights

model1$variables[[1]]
model1$weights[[1]] # these are the same thing

count_params(model1)
summary(model1)

x_test[1, ] %>% as.matrix(dimnames = NULL)

x_test[1, ] # (43,1)
as.matrix(model1$weights[[1]]) # (32,43)
as.matrix(model1$weights[[2]]) # (32,1)
as.matrix(model1$weights[[3]]) # (32,16)
as.matrix(model1$weights[[4]]) # (16,1)
as.matrix(model1$weights[[5]]) # (16,1)
as.matrix(model1$weights[[6]]) # (1)

length(x_test[1, ])
model1$weights[[1]]$shape # input layer: 43 matrices (vectors?), each size 16
model1$weights[[2]]$shape # ? length 16
model1$weights[[3]]$shape # Hidden layer 1: 16 matrices each size 16
model1$weights[[4]]$shape # ? length 16
model1$weights[[5]]$shape # Hidden layer 2
model1$weights[[6]]$shape # ? length 16

as.matrix(model1$weights[[1]]) * x_test[1, ]



par(mfrow = c(1, 1))

edges <- numeric()


for (i in c(1:43)) {
  for (k in c(44:(44 + 16))) {
    edges <- c(edges, i, k)
  }
}

for (k in c(44:(44 + 16))) {
  for (j in c(61:(61 + 16))) {
    edges <- c(edges, k, j)
  }
}

final_layer <- edges[length(edges)] + 1

for (l in c(61:(61 + 16))) {
  edges <- c(edges, l, final_layer)
}


par(mar = c(0, 0, 0, 0))
g <- graph(edges)
plot(g, layout = layout_as_tree, vertex.size = 4)


g_input <- c(1, 2, "...", as.numeric(model1$layers[[1]]$input_shape[[2]]))
g_hidden_1 <- c(1, 2, "...", as.numeric(model1$layers[[2]]$input_shape[[2]]))
g_hidden_2 <- c(1, 2, "...", as.numeric(model1$layers[[3]]$input_shape[[2]]))
g_output <- c(1)

library(igraph)
library(graphlayouts)
library(networkD3)
library(ggraph)
edges <- c(g_input, g_hidden_1, g_hidden_2)

network <- data.frame(
  to = c("*", "*", "+", "+", "ReLu", " * ", " * ", " + ", " + ", " ReLu ", "  *  ", "W3", "b3"),
  from = c("W1", "X", "*", "b1", "+", "ReLU", "W2", " * ", "b2", " + ", " ReLU ", "  *  ", "  +  ")
)


p <- simpleNetwork(network,
  height = "100px", width = "100px", # column number of target
  linkDistance = 10, # distance between node. Increase this value to have more space between nodes
  charge = -900, # numeric value indicating either the strength of the node repulsion (negative value) or attraction (positive value)
  fontSize = 14, # size of the node names
  fontFamily = "serif", # font og node names
  linkColour = "#666", # colour of edges, MUST be a common colour for the whole graph
  nodeColour = "#69b3a2", # colour of nodes, MUST be a common colour for the whole graph
  opacity = 0.9, # opacity of nodes. 0=transparent. 1=no transparency
  zoom = T # Can you zoom on the figure?
)

p

edges <- c(
  "W1", "*",
  "X", "*",
  "*", "+",
  "b1", "+",
  "+", "ReLU",
  "ReLU", " * ",
  "W2", " * ",
  " * ", " + ",
  "b2", " + ",
  " + ", " ReLU ",
  "ReLU", "    *  ",
  "  *  ", ""
)

g <- graph(edges)
plot(g, layout = layout_as_tree, vertex_size = 16)

gg <- graph_from_data_frame(network)

ggraph(gg, layout = "dendrogram", circular = FALSE) +
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()


Predictions <- model1 %>%
  predict(x_test)

val_data_model1 <- test_data %>%
  cbind(Predictions) %>%
  relocate(ClaimNb, .before = Predictions)

par(mfrow = c(1, 2))
hist(Predictions, freq = FALSE)
hist(data$data$ClaimNb, freq = FALSE)

# libraries
library(ggraph)
library(igraph)
library(tidyverse)

# create an edge list data frame giving the hierarchical structure of your individuals
d1 <- data.frame(from = "origin", to = paste("group", seq(1, 5), sep = ""))
d2 <- data.frame(from = rep(d1$to, each = 5), to = paste("subgroup", seq(1, 25), sep = "_"))
edges <- rbind(d1, d2)

# Create a graph object
mygraph <- graph_from_data_frame(edges)

# Basic tree
ggraph(mygraph, layout = "dendrogram", circular = FALSE) +
  geom_edge_diagonal() +
  geom_node_point() +
  theme_void()
