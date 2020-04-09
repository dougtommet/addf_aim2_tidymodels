rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

sages_train_dich <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_train_dich.rds"))
# sages_test_dich  <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_test_dich.rds"))
# sages_folds_dich <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_folds_dich.rds"))

# https://www.brodrigues.co/blog/2020-03-08-tidymodels/

sages_recipe_dich <- recipes::recipe(true_decline ~ ., data = sages_train_dich) %>%
  update_role(studyid, new_role = "id") %>%
  recipes::step_knnimpute(starts_with("vdfriedfrail"), vdalcohol, 
                          vdiqc_proxy, vdgds15, vdhearingimp,
                          impute_with = imp_vars(all_predictors())) %>%
  recipes::step_dummy(all_nominal(), -all_outcomes(), -has_role("id")) %>%
  recipes::step_nzv(all_predictors()) %>%
  recipes::step_center(all_predictors(), -all_nominal()) %>%
  recipes::step_scale(all_predictors(), -all_nominal())



### The common workflow
sages_wf_dich <- workflow() %>%
  add_recipe(sages_recipe_dich)

#########
### Logistic regression
logistic_sages <- logistic_reg() %>%
  set_engine("glm") %>%
  set_mode("classification")

logistic_wflow <- sages_wf_dich %>%
  add_model(logistic_sages)

#########
### glmnet
glmnet_tune_sages_dich <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")

# Hyperparameter grid
glmnet_grid_dich <- expand_grid(mixture = c(0, .25, .5, .75, 1),
                            penalty = c(seq(0, .5, by = 0.0001)))

# glmnet_grid_dich <- glmnet_tune_sages_dich %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

# Workflow  
glmnet_wflow_dich <- sages_wf_dich %>%
  add_model(glmnet_tune_sages_dich)

##########
### random forest
rf_tune_sages_dich <- rand_forest(mtry = tune(), 
                             trees = 2500,
                             min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Hyperparameter grid
rf_grid_dich <- expand_grid(mtry = c(3, 6, 9, 15, 20, 30),
                       min_n = c(3, 6, 9, 12))

# rf_grid_dich <- rf_tune_sages_dich %>%
#   parameters() %>%
#   finalize(select(sages_train_dich, -true_decline)) %>%  
#   grid_max_entropy(size = 15)


# Workflow 
rf_wflow_dich <- sages_wf_dich %>%
  add_model(rf_tune_sages_dich)

##########
### gradient boost
boost_tune_sages_dich <- boost_tree(mtry = tune(), 
                               trees = tune(), 
                               learn_rate = tune(), 
                               tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("classification")

# Hyperparameter grid
boost_grid_dich <- expand_grid(mtry = c(.3, .5, .7),
                          trees = c(500, 1000, 2500, 5000),
                          learn_rate = c(0, 0.5, 0.1, 0.15, 0.2, 0.25, 0.3),
                          tree_depth = c(3, 4, 5, 7, 9, 11, 13, 15))

# boost_grid_dich <- boost_tune_sages_dich %>%
#   parameters() %>%
#   finalize(select(sages_train_dich, -true_decline)) %>%  
#   grid_max_entropy(size = 25)

# Workflow 
boost_wflow_dich <- sages_wf_dich %>%
  add_model(boost_tune_sages_dich)

##########
### mars model (Multivariate Adaptive Regression Splines)
mars_tune_sages_dich <- mars(num_terms = tune(), 
                        prod_degree = 2) %>%
  set_engine("earth") %>%
  set_mode("classification")

mars_grid_dich <- expand_grid(num_terms = seq(1, 30, by = 1))

# mars_grid_dich <- mars_tune_sages_dich %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

mars_wflow_dich <-sages_wf_dich %>%
  add_model(mars_tune_sages_dich)

##########
### neural nets
nnet_tune_sages_dich <- mlp(hidden_units = tune(), 
                     penalty = tune(), 
                     activation = "softmax") %>%
  set_engine("nnet") %>%
  set_mode("classification")

nnet_grid_dich <- expand_grid(hidden_units = c(3, 5, 7, 9, 11, 15, 17, 20),
                       penalty = c(0, 0.1, 0.001, 0.0001, 0.00001))

# nnet_grid_dich <- nnet_tune_sages_dich %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

nnet_wflow_dich <- sages_wf_dich %>%
  add_model(nnet_tune_sages_dich)

##########
### Suport vector machines (polynomial kernal)
svm_poly_tune_sages_dich <- svm_poly(cost = tune(),
                                degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

# svm_poly_grid_dich <- expand_grid(cost = c(.01, .05, .1, .25, .5, .75, 1, 2),
#                              degree = c(1, 2, 3, 5))

svm_poly_grid_dich <- svm_poly_tune_sages_dich %>%
  parameters() %>%
  grid_max_entropy(size = 15)

svm_poly_wflow_dich <- sages_wf_dich %>%
  add_model(svm_poly_tune_sages_dich)

##########
### Suport vector machines (radial basis function kernal)
svm_rbf_tune_sages_dich <- svm_rbf(cost = tune(),
                              rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

svm_rbf_grid_dich <- svm_rbf_tune_sages_dich %>%
  parameters() %>%
  grid_max_entropy(size = 50)

svm_rbf_wflow_dich <- sages_wf_dich %>%
  add_model(svm_rbf_tune_sages_dich)

##########
### Nearest neighbor
nneighbor_tune_sages_dich <- nearest_neighbor(neighbors = tune(),
                                              weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("classification")


nneighbor_grid_dich <- nneighbor_tune_sages_dich %>%
  parameters() %>%
  grid_max_entropy(size = 50)

nneighbor_wflow_dich <- sages_wf_dich %>%
  add_model(nneighbor_tune_sages_dich)

##########
### Combining the workflows and hyperparameter grids into lists
# wflow_list_dich <- list(glmnet_wflow_dich, rf_wflow_dich, boost_wflow_dich, mars_wflow_dich, nnet_wflow_dich, svm_poly_wflow_dich, svm_rbf_wflow_dich, nneighbor_wflow_dich)
# grid_list_dich <- list(glmnet_grid_dich, rf_grid_dich, boost_grid_dich, mars_grid_dich, nnet_grid_dich, svm_poly_grid_dich, svm_rbf_grid_dich, nneighbor_grid_dich)
# model_names_dich <- c("glmnet", "rf", "boost", "mars", "nnet", "svm_poly", "svm_rbf", "nneighbor")

wflow_list_dich <- list(logistic_wflow, glmnet_wflow_dich, mars_wflow_dich, rf_wflow_dich, boost_wflow_dich, nnet_wflow_dich, nneighbor_wflow_dich, svm_rbf_wflow_dich)
grid_list_dich <- list(NULL,            glmnet_grid_dich,  mars_grid_dich,  rf_grid_dich,  boost_grid_dich,  nnet_grid_dich,  nneighbor_grid_dich,  svm_rbf_grid_dich)
model_names_dich <- c("logistic",       "glmnet",          "mars",          "rf",          "boost",          "nnet",          "nneighbor",          "svm_rbf")

model_df_dich <- tibble(model = model_names_dich) %>%
  mutate(model_fct = factor(model, 
                            levels = c("logistic", "glmnet", "mars", 
                                       "rf", "boost", "nnet", "nneighbor", "svm_rbf"),
                            labels = c("Logistic Regression", "Regularization Regression", 
                                       "Multivariate Adaptive Regression Splines", "Random Forest",
                                       "Gradient Boosted Model", "Neural Network", 
                                       "K-Nearest Neighbor",
                                       "Support Vector Machine (radial basis)")))


### Save the R objects
saveRDS(wflow_list_dich,     file=path(r.objects.folder.tidymodel, "045_wflow_list_dich.rds"))
saveRDS(grid_list_dich,      file=path(r.objects.folder.tidymodel, "045_grid_list_dich.rds"))
saveRDS(model_names_dich,    file=path(r.objects.folder.tidymodel, "045_model_names_dich.rds"))
saveRDS(model_df_dich,       file=path(r.objects.folder.tidymodel, "045_model_df_dich.rds"))

