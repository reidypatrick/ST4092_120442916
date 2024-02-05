get_computation_graph_tex <- function(margin = 0,
                                      h_scale = 1,
                                      v_scale = 1,
                                      act_funs = c("ReLU", "ReLU", "Exp")) {
  c("
  \\begin{figure}
  \\centering
  \\begin{tikzpicture}[>=stealth, node distance=1.5cm, every node/.style={scale=0.8}]
  % Input layer
  \\node[circle, draw, fill=blue!20] (X) at (0,-1) {$X$};
  \\node[circle, draw, fill=blue!20] (W1) at (0,-2) {$W_1$};
  \\node[circle, draw, fill=blue!20] (b1) at (0,-3) {$b_1$};
  
  \\node[circle, draw, fill=blue!20] (star1) at (1,-1) {$*$};
  \\node[circle, draw, fill=blue!20] (plus1) at (2,-1) {$+$};
  
  \\node[circle, draw, fill=blue!20] (act_fun1) at (3,-1) {$",act_funs[1],"$};
  
  % Labels
  \\node[above=0.5cm of X] {Input Layer};
  
  % Connect Nodes
  \\draw[->] (X) -- (star1);
  \\draw[->] (W1) -- (star1);
  
  \\draw[->] (star1) -- (plus1);
  \\draw[->] (b1) -- (plus1);
  
  \\draw[->] (plus1) -- (act_fun1);
  
  % First Hidden Layer
  \\node[circle, draw, fill=blue!20] (W2) at (3,-2) {$W_2$};
  \\node[circle, draw, fill=blue!20] (b2) at (3,-3) {$b_2$};
  
  \\node[circle, draw, fill=blue!20] (star2) at (4,-1) {$*$};
  \\node[circle, draw, fill=blue!20] (plus2) at (5,-1) {$+$};
  
  \\node[circle, draw, fill=blue!20] (act_fun2) at (6,-1) {$",act_funs[2],"$};
  
  % Labels
  \\node[above=0.5cm of act_fun1] {Hidden Layer 1};
  
  % Connect Nodes
  \\draw[->] (act_fun1) -- (star2);
  \\draw[->] (W2) -- (star2);
  
  \\draw[->] (star2) -- (plus2);
  \\draw[->] (b2) -- (plus2);
  
  \\draw[->] (plus2) -- (act_fun2);
  
  % First Hidden Layer
  \\node[circle, draw, fill=blue!20] (W3) at (6,-2) {$W_3$};
  \\node[circle, draw, fill=blue!20] (b3) at (6,-3) {$b_3$};
  
  \\node[circle, draw, fill=blue!20] (star3) at (7,-1) {$*$};
  \\node[circle, draw, fill=blue!20] (plus3) at (8,-1) {$+$};
  
  \\node[circle, draw, fill=blue!20] (act_fun3) at (9,-1) {$",act_funs[3],"$};
  
  % Labels
  \\node[above=0.5cm of act_fun2] {Hidden Layer 1};
  
  % Connect Nodes
  \\draw[->] (act_fun2) -- (star3);
  \\draw[->] (W3) -- (star3);
  
  \\draw[->] (star3) -- (plus3);
  \\draw[->] (b3) -- (plus3);
  
  \\draw[->] (plus3) -- (act_fun3);
  
  % Final Layer
  \\node[circle, draw, fill=blue!20] (output) at (10,-1) {$\\hat{y}$};
  \\node[circle, draw, fill=blue!20] (loss) at (10,-2) {$Loss$};
  \\node[circle, draw, fill=blue!20] (truth) at (10,-3) {$y$};
  
  % Labels
  \\node[above=0.5cm of act_fun3] {Output};
  
  % Connect Nodes
  \\draw[->] (act_fun3) -- (output);
  \\draw[->] (output) -- (loss);
  \\draw[->] (truth) -- (loss);
  
  \\end{tikzpicture}
  \\caption{Computation Graph of Neural Network}
  \\label{fig:neural_network_ellipses_labels}
  \\end{figure}"
  )
}