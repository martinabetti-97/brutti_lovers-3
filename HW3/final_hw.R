



# Libraries ---------------------------------------------------------------

library(data.table)
library(keras)
library(tensorflow)
library(tfdatasets)




# Import data -------------------------------------------------------------

data <- fread("data/train4final_hw.csv")


# Transform data ----------------------------------------------------------

set.seed(123)

# Take only the mel-spectrum: 

val.idx <- sample(1:1561, 315)

val.labels <- data$tempo[val.idx]
val.data <- data[val.idx, 1:(40*171)]

summary(val.labels)

train.labels <- data$tempo[-val.idx]
train.data <- data[!val.idx, 1:(40*171)]

summary(train.labels)

summary(data$tempo)

unique(data$tempo)

which.max(tabulate(data$tempo))

rm(data)

# Define network ----------------------------------------------------------

# Input layer: the input layer should accept a vector of shape (1x6840), i.e.
# a row (song) of the data.table; probably the network expects a matrix 
# (40x171) for each observation, even though I tried to adapt it to work with
# vectors instead of matrices (cfr. first layer in the paper's second diagram)

inputs <- layer_input(shape=c(40*171, 1))
 

# Check the inputs
summary(inputs)


# Build the network: first 3 layers
# The number of filters indicates that 16 different filters will be applied at this 
# step and then the output of this layer will be 16 vectors of shape (1,6840) for each 
# observation
outputs <- inputs %>%
  # layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_conv_1d(filters = 16, kernel_size = 5, padding = "same",
                strides = 1L, activation = "elu") %>%
  
  layer_batch_normalization() %>%
  layer_conv_1d(filters = 16, kernel_size = 5, padding = "same",
                strides = 1L, activation = "elu") %>%
  
  layer_batch_normalization() %>%
  layer_conv_1d(filters = 16, kernel_size = 5, padding = "same",
                strides = 1L, activation = "elu")
 
# Check the network so far
summary(outputs)


# Define the multi-filter module
create_mf_module <- function(model, pool_size=2) {
  
  conv_ker_dim <- c(32,64,96,128,196,256)
  mf_mod_first_part <- model %>%
    layer_average_pooling_1d(pool_size = pool_size, 
                             strides = 1, padding = "same") %>%
    layer_batch_normalization()
      
  parallel_layers <- list()
  
  for(size in conv_ker_dim) {
    parallel_layers <- append(parallel_layers, 
                              layer_conv_1d(mf_mod_first_part, filters = 24, 
                                            kernel_size = size, padding = "same",
                                            strides = 1, activation = "elu"))
  }
  
  mf_mod <- layer_concatenate(parallel_layers) %>%
    layer_conv_1d(filters = 36, kernel_size = 1, strides = 1,
                  padding = "same", activation = "elu")
  return(mf_mod)
} 


# Add multi-filter modules
filter_sizes <- c(5, rep(2,3))

for(filter_size in filter_sizes)
  outputs <- create_mf_module(outputs, pool_size = filter_size)


# Check outputs
summary(outputs)


# Add final layers to the outputs
outputs <- outputs %>%
  layer_flatten() %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = 64, activation = "elu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = 64, activation = "elu") %>%
  layer_batch_normalization() %>%
  layer_dense(units = 201, activation = "softmax")


# Check again the outputs
summary(outputs)


network <- keras_model(inputs = inputs, outputs = outputs, name="audio_net")


# Check the network
summary(network)

kerasR::plot_model(network, show_shapes = T)


# Compile the network -----------------------------------------------------

network %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy", "mean_squared_error")
)



callbacks <- c(callback_model_checkpoint("./data"), 
               callback_early_stopping(patience = 3), 
               callback_reduce_lr_on_plateau(patience = 2))


# Try with the data -------------------------------------------------------

network %>% fit(x = as.matrix(train.data), y = keras::to_categorical(train.labels),
                validation_data = list(as.matrix(val.data), 
                                       keras::to_categorical(val.labels)),
                epochs = 12, batch_size = 100,
                callbacks = callbacks)

network <- load_model_tf("results/best/weights312-128.20-0.39.hdf5")


summary(network)


metrics <- network %>% evaluate(as.matrix(val.data), to_categorical(val.labels), verbose = 2)

predictions <- network %>% kerasR::keras_predict(as.matrix(val.data), verbose = 1)

rmse <- 0
for(i in 1:315) {
  rmse <- rmse + (which.max(predictions[i]) - val.labels[i])^2
}

rmse <- sqrt(rmse / 315)








# Part C ------------------------------------------------------------------

l <- list(1,2,3,4,5)
names <- sapply(top_var, function(x) {paste0("fn.",x)})


model.list <- lapply(rep(NA, 5), function(x) {x})
names(model.list) <- sapply(top_var, function(x) {paste0("fn.",x)})

l$a <- list(1,2,3,4)





