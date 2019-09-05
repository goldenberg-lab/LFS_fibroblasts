library(tidyverse)
library(randomForest)
library(ranger)
library(parallel)
library(cowplot)
source('./scripts/import_data.R')
source('../Helpers/rmv_low_variance.R')
source('./scripts/Functions.R')

# Fit a randomForest model to predict Mutant or WT cell population given the
# morphological features.
dat <- import_data() %>% rmv_correlated_cols() %>%
  rmv_low_var(keep_cols = c('Row', 'Column')) %>%
  mutate(Y = as.factor(Row > 3))

# sample a small training set to figure out if trainControl will return all the
# model information I need.
set.seed(7)
small_dat <- dat %>% sample_frac(size = 0.02) %>%
  split_dat(., bincol = bin, response = Y, n = 10) %>%
  select(-Row, -Column, -S, -M)

hp_grid <- expand.grid(mtry = caret::var_seq(ncol(small_dat) - 1, classification = T, len = 3),
                       min.node.size = 1,
                       splitrules = c('gini', 'extratrees'))

val_acc <- vapply(1:nrow(hp_grid),
                  function(iter, train, val, hps){
                    print(paste0('Iteration: ', iter))
                    mdl <- ranger(data = train, mtry = hps[iter,1], min.node.size = hps[iter,2],
                                  splitrule = hps[iter,3], dependent.variable.name = 'Y',
                                  probability = T, importance = 'impurity');
                    ranger_out <- predict(mdl, val)
                    sum(val$Y == (ranger_out$predictions[,colnames(ranger_out$predictions)[[2]]] > 0.5))/length(val$Y)
                    }, numeric(1),
                  small_dat %>% filter(bin < 9), small_dat %>% filter(bin == 9),hp_grid)

best_params <- hp_grid[which(val_acc == max(val_acc)),]


test <- small_dat %>% filter(bin == 10)
final_mdl <- ranger(data = small_dat %>% filter(bin != 10), mtry = best_params$mtry,
                    min.node.size = best_params$min.node.size, splitrule = best_params$splitrules,
                    dependent.variable.name = 'Y', probability = T)
final_mdl_out <- predict(final_mdl, test)
final_acc <- sum(test$Y == (final_mdl_out$predictions[,colnames(final_mdl_out$predictions)[[2]]] > 0.5))/length(test$Y)

save.image('./Random_forest_tester.Rdata')

