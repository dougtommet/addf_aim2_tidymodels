rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))


# sages_split <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_split.rds"))
sages_train <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_train.rds"))
# sages_test  <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_test.rds"))
# sages_folds <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_folds.rds"))

# https://www.brodrigues.co/blog/2020-03-08-tidymodels/



sages_recipe <- recipes::recipe(vdgcp_slope48m ~ ., data = sages_train) %>%
  update_role(studyid, new_role = "id") %>%
  recipes::step_knnimpute(starts_with("vdfriedfrail"), vdalcohol, 
                          vdiqc_proxy, vdgds15, vdhearingimp,
                          impute_with = imp_vars(all_predictors())) %>%
  recipes::step_dummy(all_nominal(), -all_outcomes(), -has_role("id")) %>%
  recipes::step_nzv(all_predictors()) %>%
  recipes::step_center(all_predictors(), -all_nominal()) %>%
  recipes::step_scale(all_predictors(), -all_nominal())


### The common workflow
sages_wf <- workflow() %>%
  add_recipe(sages_recipe)

#########
### Linear Regression
linear_sages <- linear_reg() %>%
  set_engine("lm") %>%
  set_mode("regression")

linear_wflow <- sages_wf %>%
  add_model(linear_sages)

# foo <- parsnip::fit(linear_wflow, data = sages_train)
# 
# foo_resutls <- foo %>%
#   pull_workflow_fit() %>%
#   tidy() %>%
#   mutate(abs_stat = abs(statistic))
#########
### glmnet
glmnet_tune_sages <- linear_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet") %>%
  set_mode("regression")

# Hyperparameter grid
glmnet_grid <- expand_grid(mixture = c(0, .25, .5, .75, 1),
                            penalty = c(seq(0, .5, by = 0.0001)))

# glmnet_grid <- glmnet_tune_sages %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

# Workflow  
glmnet_wflow <- sages_wf %>%
  add_model(glmnet_tune_sages)

##########
### random forest
rf_tune_sages <- rand_forest(mtry = tune(), 
                             trees = 2500,
                             min_n = tune()) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# Hyperparameter grid
rf_grid <- expand_grid(mtry = c(3, 6, 9, 15, 20, 30),
                       min_n = c(3, 6, 9, 12))

# rf_grid <- rf_tune_sages %>%
#   parameters() %>%
#   finalize(select(sages_train, -vdgcp_slope48m)) %>%  
#   grid_max_entropy(size = 15)


# Workflow 
rf_wflow <- sages_wf %>%
  add_model(rf_tune_sages)

##########
### gradient boost
boost_tune_sages <- boost_tree(mtry = tune(), 
                               trees = tune(), 
                               learn_rate = tune(), 
                               tree_depth = tune()) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

# Hyperparameter grid
boost_grid <- expand_grid(mtry = c(.3, .5, .7),
                          trees = c(500, 1000, 2500, 5000),
                          learn_rate = c(0, 0.5, 0.1, 0.15, 0.2, 0.25, 0.3),
                          tree_depth = c(3, 4, 5, 7, 9, 11, 13, 15))

# boost_grid <- boost_tune_sages %>%
#   parameters() %>%
#   finalize(select(sages_train, -vdgcp_slope48m)) %>%  
#   grid_max_entropy(size = 25)

# Workflow 
boost_wflow <- sages_wf %>%
  add_model(boost_tune_sages)

##########
### mars model (Multivariate Adaptive Regression Splines)
mars_tune_sages <- mars(num_terms = tune(), 
                        prod_degree = 2) %>%
  set_engine("earth") %>%
  set_mode("regression")

mars_grid <- expand_grid(num_terms = seq(1, 30, by = 1))

# mars_grid <- mars_tune_sages %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

mars_wflow <-sages_wf %>%
  add_model(mars_tune_sages)

##########
### neural nets
nnet_tune_sages <- mlp(hidden_units = tune(), 
                     penalty = tune(), 
                     activation = "softmax") %>%
  set_engine("nnet") %>%
  set_mode("regression")

nnet_grid <- expand_grid(hidden_units = c(3, 5, 7, 9, 11, 15, 17, 20),
                       penalty = c(0, 0.1, 0.001, 0.0001, 0.00001))

# nnet_grid <- nnet_tune_sages %>%
#   parameters() %>%
#   grid_max_entropy(size = 15)

nnet_wflow <- sages_wf %>%
  add_model(nnet_tune_sages)

##########
### Suport vector machines (polynomial kernal)
svm_poly_tune_sages <- svm_poly(cost = tune(),
                                degree = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

# svm_poly_grid <- expand_grid(cost = c(.01, .05, .1, .25, .5, .75, 1, 2),
#                              degree = c(1, 2, 3, 5))

svm_poly_grid <- svm_poly_tune_sages %>%
  parameters() %>%
  grid_max_entropy(size = 15)

svm_poly_wflow <- sages_wf %>%
  add_model(svm_poly_tune_sages)

##########
### Suport vector machines (radial basis function kernal)
svm_rbf_tune_sages <- svm_rbf(cost = tune(),
                              rbf_sigma = tune()) %>%
  set_engine("kernlab") %>%
  set_mode("regression")

svm_rbf_grid <- svm_rbf_tune_sages %>%
  parameters() %>%
  grid_max_entropy(size = 50)

svm_rbf_wflow <- sages_wf %>%
  add_model(svm_rbf_tune_sages)

##########
### Nearest neighbor
nneighbor_tune_sages <- nearest_neighbor(neighbors = tune(),
                                         weight_func = tune()) %>%
  set_engine("kknn") %>%
  set_mode("regression")

nneighbor_grid <- nneighbor_tune_sages %>%
  parameters() %>%
  grid_max_entropy(size = 50)

nneighbor_wflow <- sages_wf %>%
  add_model(nneighbor_tune_sages)

##########
### Combining the workflows and hyperparameter grids into lists
# wflow_list <- list(linear_wflow, glmnet_wflow, rf_wflow, boost_wflow, mars_wflow, nnet_wflow, svm_poly_wflow, svm_rbf_wflow, nneighbor_wflow)
# grid_list <- list(NULL, glmnet_grid, rf_grid, boost_grid, mars_grid, nnet_grid, svm_poly_grid, svm_rbf_grid, nneighbor_grid)
# model_names <- c("linear", "glmnet", "rf", "boost", "mars", "nnet", "svm_poly", "svm_rbf", "nneighbor")

wflow_list <- list(linear_wflow, glmnet_wflow, mars_wflow, rf_wflow, boost_wflow, nnet_wflow, nneighbor_wflow, svm_rbf_wflow)
grid_list  <- list(NULL,         glmnet_grid,  mars_grid,  rf_grid,  boost_grid,  nnet_grid,  nneighbor_grid,  svm_rbf_grid)
model_names <- c("linear",       "glmnet",     "mars",     "rf",     "boost",     "nnet",     "nneighbor",     "svm_rbf")

model_df <- tibble(model = model_names) %>%
  mutate(model_fct = factor(model, 
                            levels = c("linear", "glmnet", "mars", 
                                       "rf", "boost", "nnet", "nneighbor", "svm_rbf"),
                            labels = c("Linear Regression", "Regularization Regression", 
                                       "Multivariate Adaptive Regression Splines", "Random Forest",
                                       "Gradient Boosted Model", "Neural Network", 
                                       "K-Nearest Neighbor",
                                       "Support Vector Machine (radial basis)")))



### Save the R objects
saveRDS(wflow_list,     file=path(r.objects.folder.tidymodel, "040_wflow_list.rds"))
saveRDS(grid_list,      file=path(r.objects.folder.tidymodel, "040_grid_list.rds"))
saveRDS(model_names,    file=path(r.objects.folder.tidymodel, "040_model_names.rds"))
saveRDS(model_df,       file=path(r.objects.folder.tidymodel, "040_model_df.rds"))
