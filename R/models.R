# ======================================== #
# Example models used in the talk          #
# Sean Browning                            #
# ======================================== #
library(keras)
library(dplyr)

sphere_binary_classifier <- function(n_neurons = 20, n_layers = 2, activation = "relu") {
  model <- keras_model_sequential(name = "")

  model <- model %>%
      layer_dense(
        units = n_neurons,
        activation = activation,
        input_shape = shape(3),
        name = "Dense-1"
      )

  # Create however many dense layers specified
  for (i in seq_len(n_layers - 1)) {
    model <- model %>%
      layer_dense(
        units = n_neurons,
        activation = activation,
        name = sprintf("Dense-%d", i + 1)
      )
  }

  # Create binary output layer
  model <- model %>% layer_dense(units = 1, activation = "softmax")

  # Compile
  model <- model %>%
    compile(
      optimizer = "adam",
      loss = "binary_crossentropy",
      metrics = "accuracy"
    )

  return(model)
}

circle_binary_classifier <- function(n_neurons = 20, n_layers = 2, activation = "relu") {
  model <- keras_model_sequential(name = "")

  model <- model %>%
      layer_dense(
        units = n_neurons,
        activation = activation,
        input_shape = shape(2),
        name = "Dense-1"
      )

  # Create however many dense layers specified
  for (i in seq_len(n_layers - 1)) {
    model <- model %>%
      layer_dense(
        units = n_neurons,
        activation = activation,
        name = sprintf("Dense-%d", i + 1)
      )
  }

  # Create binary output layer
  model <- model %>% layer_dense(units = 1, activation = "softmax")

  # Compile
  model <- model %>%
    compile(
      optimizer = "adam",
      loss = "binary_crossentropy",
      metrics = "accuracy"
    )

  return(model)
}