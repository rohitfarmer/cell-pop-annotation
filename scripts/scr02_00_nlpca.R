library(tidyverse)
library(tidymodels)
library(pROC)
#library(pcaMethods)
#library(kernlab)


data_folder <- file.path("data")
results_folder <- file.path("results")



# nk_bendall_vec <- c(as.matrix(nk_bendall))
# nk_bendall_shuf <- sample(nk_bendall_vec)
get_pca <- function(dat){
        set.seed(789847)
        dat_shuf <- matrix(sample(c(as.matrix(dat))), nrow = nrow(dat), ncol = ncol(dat))
        colnames(dat_shuf) <- colnames(dat)
        pca_dat <- rbind(data.frame("target" = 1,dat),
                         data.frame("target" = 0, dat_shuf))
        pca_res <- prcomp(pca_dat[colnames(dat)])

        dat_pcs <- cbind("target" = pca_dat$target, pca_res$x[, 1:7])
        return(dat_pcs)
}

run_glm <- function(model_data){
        set.seed(1353)
        split_data <- initial_split(model_data, strata = "target")
        train_data <- training(split_data)
        test_data <- testing(split_data)

        glm_res <- glm(target ~ ., data = train_data, family = "binomial")
        pred_res <- predict(glm_res, test_data, type="response")
        auc_res <- auc(test_data$target, pred_res)
        return(list("glm_model" = glm_res,"train_data" = train_data, "test_data" = test_data, "auc" = auc_res))
}

# NK
nk_bendall <- read_tsv(file.path(results_folder, "nk_bendall.tsv"))
nk_mini <- read_tsv(file.path(results_folder, "nk_mini.tsv"))
nk_bendall_pcs <- get_pca(nk_bendall)
nk_mini_pcs <- get_pca(nk_mini)
nk_all <- rbind(nk_bendall_pcs, nk_mini_pcs) %>%
        data.frame()
nk_glm <- run_glm(nk_all)

# CD4
cd4_bendall <- read_tsv(file.path(results_folder, "cd4_bendall.tsv"))
cd4_mini <- read_tsv(file.path(results_folder, "cd4_mini.tsv"))
cd4_bendall_pcs <- get_pca(cd4_bendall)
cd4_mini_pcs <- get_pca(cd4_mini)
cd4_all <- rbind(cd4_bendall_pcs, cd4_mini_pcs) %>%
        data.frame()
cd4_glm <- run_glm(cd4_all)

cross_pred <- predict(nk_glm$glm_model, cd4_glm$test_data, type="response")
cross_auc <- auc(cd4_glm$test_data$target, cross_pred)
