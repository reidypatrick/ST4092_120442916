get_network_tex <- function(
    name,
    n_input = 43,
    n_hidden1 = 16,
    n_hidden2 = 16,
    margin = 0,
    h_scale = 1,
    v_scale = 1) {
  c(
    "
  \\begin{figure}
    \\centering
    \\begin{tikzpicture}[>=stealth, node distance=1.5cm,",
    "every node/.style={scale=0.8}]
      % Input layer
      \\node[circle, draw, fill=yellow!20] (input1) at (",
    margin, ",", v_scale * (-1), ") {$x_1$};
      \\node[circle, draw, fill=yellow!20] (input2) at (",
    margin, ",", v_scale * (-2), ") {$x_2$};
      \\node[circle, draw, fill=yellow!20] (input3) at (",
    margin, ",", v_scale * (-3), ") {$\\ldots$};
      \\node[circle, draw, fill=yellow!20] (input4) at (",
    margin, ",", v_scale * (-4), ") {$x_{", n_input, "}$};

      % First hidden layer
      \\path (", margin + h_scale * 2, ",", v_scale * (-1),
    ") node[circle, draw, fill=blue!20] (hidden11) {$h_{1,1}$};
      \\path (", margin + h_scale * 2, ",", v_scale * (-2),
    ") node[circle, draw, fill=blue!20] (hidden12) {$h_{1,2}$};
      \\path (", margin + h_scale * 2, ",", v_scale * (-3),
    ") node[circle, draw, fill=blue!20] (hidden13) {$\\ldots$};
      \\path (", margin + h_scale * 2, ",", v_scale * (-4),
    ") node[circle, draw, fill=blue!20] (hidden14) {$h_{1,",
    n_hidden1, "}$};

      % Second hidden layer
      \\path (", margin + h_scale * 4, ",", v_scale * (-1),
    ") node[circle, draw, fill=blue!20] (hidden21) {$h_{2,1}$};
      \\path (", margin + h_scale * 4, ",", v_scale * (-2),
    ") node[circle, draw, fill=blue!20] (hidden22) {$h_{2,1}$};
      \\path (", margin + h_scale * 4, ",", v_scale * (-3),
    ") node[circle, draw, fill=blue!20] (hidden23) {$\\ldots$};
      \\path (", margin + h_scale * 4, ",", v_scale * (-4),
    ") node[circle, draw, fill=blue!20] (hidden24) {$h_{2,",
    n_hidden2, "}$};

      % Output layer
      \\path (", margin + h_scale * 6, ",", v_scale * (-2.5),
    ") node[circle, draw, fill=yellow!20] (output) {$y$};

      % Connect nodes
      \\foreach \\i in {1,...,4}
          \\foreach \\h in {1,...,4}
              \\draw[->] (input\\i) -- (hidden1\\h);

      \\foreach \\h in {1,...,4}
          \\foreach \\o in {1,...,4}
              \\draw[->] (hidden1\\h) -- (hidden2\\o);

      \\foreach \\o in {1,...,4}
          \\draw[->] (hidden2\\o) -- (output);

      % Labels
      \\node[above=0.5cm of input1] {Input Layer};
      \\node[above=0.5cm of hidden11] {Hidden Layer 1};
      \\node[above=0.5cm of hidden21] {Hidden Layer 2};
      \\node[above=0.5cm of output] {Output Layer};

    \\end{tikzpicture}
    \\caption{Diagram of Architecture of Neural Network.}
    \\label{fig:", name, "}
  \\end{figure}
  "
  )
}
