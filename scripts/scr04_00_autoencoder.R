library(tidyverse)
library(tidymodels)
library(keras)

data_folder <- file.path("data")
results_folder <- file.path("results")

# NK
nk_bendall <- read_tsv(file.path(results_folder, "nk_bendall.tsv"))
nk_mini <- read_tsv(file.path(results_folder, "nk_mini.tsv"))
# nk_bendall_pcs <- get_pca(nk_bendall)
# nk_mini_pcs <- get_pca(nk_mini)
# nk_all <- rbind(nk_bendall_pcs, nk_mini_pcs) %>%
#         data.frame()
# 
# CD4
cd4_bendall <- read_tsv(file.path(results_folder, "cd4_bendall.tsv"))
cd4_mini <- read_tsv(file.path(results_folder, "cd4_mini.tsv"))
# cd4_all <- rbind(cd4_bendall_pcs, cd4_mini_pcs) %>%
#         data.frame()
# CD8
cd8_bendall <- read_tsv(file.path(results_folder, "cd8_bendall.tsv"))
cd8_mini <- read_tsv(file.path(results_folder, "cd8_mini.tsv"))
# cd8_bendall_pcs <- get_pca(cd8_bendall)
# cd8_mini_pcs <- get_pca(cd8_mini)
# cd8_all <- rbind(cd8_bendall_pcs, cd8_mini_pcs) %>%
#         data.frame()
# 
# autoencoder in keras
# set training data

stop()
x_train <- as.matrix(nk_bendall)
# set model
model <- keras_model_sequential()
model %>%
          layer_dense(units = 10, activation = "tanh", input_shape = ncol(x_train)) %>%
          layer_dense(units = 9, activation = "tanh", input_shape = ncol(x_train)) %>%
          layer_dense(units = 8, activation = "tanh", input_shape = ncol(x_train)) %>%
            layer_dense(units = 7, activation = "tanh", name = "bottleneck") %>%
           layer_dense(units = 8, activation = "tanh", input_shape = ncol(x_train)) %>%
          layer_dense(units = 9, activation = "tanh", input_shape = ncol(x_train)) %>%
             layer_dense(units = 10, activation = "tanh") %>%
                layer_dense(units = ncol(x_train))
        # view model layers
summary(model)


model %>% compile(
  loss = "mean_squared_error", 
  optimizer = "adam"
)
# fit model
model %>% fit(
  x = x_train, 
  y = x_train, 
  epochs = 10,
  verbose = 1
)

# evaluate the performance of the model
mse.ae2 <- evaluate(model, x_train, x_train)
mse.ae2

# extract the bottleneck layer
intermediate_layer_model <- keras_model(inputs = model$input, outputs = get_layer(model, "bottleneck")$output)
intermediate_output <- predict(intermediate_layer_model, x_train)



stop()

nnet_da <- rbind(data.frame("target" = "nk", nk_all),
                  data.frame("target" = "cd4", cd4_all),
                  data.frame("target" = "cd8", cd8_all))
nnet_dat$target <- as.factor(nnet_dat$target) 
split_data <- initial_split(nnet_dat, strata = "target")
train_data <- training(split_data)
test_data <- testing(split_data)


nnet_res <- nnet::multinom(target ~ ., train_data)

predicted_classes <- nnet_res %>% predict(test_data)
mean(predicted_classes == test_data$target)

