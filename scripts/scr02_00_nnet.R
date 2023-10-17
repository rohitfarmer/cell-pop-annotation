library(tidyverse)
library(tidymodels)
library(pROC)
library(nnet)

data_folder <- file.path("data")
results_folder <- file.path("results")

get_pca <- function(dat){
        pca_res <- prcomp(dat)
        pca_res <- pca_res$x[,1:7]
        return(pca_res)
}

# NK
nk_bendall <- read_tsv(file.path(results_folder, "nk_bendall.tsv"))
nk_mini <- read_tsv(file.path(results_folder, "nk_mini.tsv"))
nk_bendall_pcs <- get_pca(nk_bendall)
nk_mini_pcs <- get_pca(nk_mini)
nk_all <- rbind(nk_bendall_pcs, nk_mini_pcs) %>%
        data.frame()

# CD4
cd4_bendall <- read_tsv(file.path(results_folder, "cd4_bendall.tsv"))
cd4_mini <- read_tsv(file.path(results_folder, "cd4_mini.tsv"))
cd4_bendall_pcs <- get_pca(cd4_bendall)
cd4_mini_pcs <- get_pca(cd4_mini)
cd4_all <- rbind(cd4_bendall_pcs, cd4_mini_pcs) %>%
        data.frame()
# CD8
cd8_bendall <- read_tsv(file.path(results_folder, "cd8_bendall.tsv"))
cd8_mini <- read_tsv(file.path(results_folder, "cd8_mini.tsv"))
cd8_bendall_pcs <- get_pca(cd8_bendall)
cd8_mini_pcs <- get_pca(cd8_mini)
cd8_all <- rbind(cd8_bendall_pcs, cd8_mini_pcs) %>%
        data.frame()

nnet_dat <- rbind(data.frame("target" = "nk", nk_all),
                  data.frame("target" = "cd4", cd4_all),
                  data.frame("target" = "cd8", cd8_all))
nnet_dat$target <- as.factor(nnet_dat$target) 
split_data <- initial_split(nnet_dat, strata = "target")
train_data <- training(split_data)
test_data <- testing(split_data)


nnet_res <- nnet::multinom(target ~ ., train_data)

predicted_classes <- nnet_res %>% predict(test_data)
mean(predicted_classes == test_data$target)

