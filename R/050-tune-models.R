rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))


sages_folds <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_folds.rds"))
wflow_list  <- readRDS(file=path(r.objects.folder.tidymodel, "040_wflow_list.rds"))
grid_list   <- readRDS(file=path(r.objects.folder.tidymodel, "040_grid_list.rds"))

doParallel::registerDoParallel()

##########
##### Regression models
### Tune the models
tuned_models_list <- map2(.x = wflow_list,
                          .y = grid_list,
                          ~tune_grid(.x, 
                                     resamples = sages_folds, 
                                     grid = .y))

### Get the best hyperparameters
tuned_models_best <- tuned_models_list %>%
  map(tune::select_best, metric = "rmse", maximize = FALSE)

### Put the best parameters in the workflow
wflow_list_best <- map2(.x = wflow_list, 
                        .y = tuned_models_best, 
                        ~finalize_workflow(.x, .y))


### Save the R objects
saveRDS(tuned_models_list, file=path(r.objects.folder.tidymodel, "050_tuned_models_list.rds"))
saveRDS(tuned_models_best, file=path(r.objects.folder.tidymodel, "050_tuned_models_best.rds"))
saveRDS(wflow_list_best,   file=path(r.objects.folder.tidymodel, "050_wflow_list_best.rds"))






# tuned_models_list[[1]][[4]]
# tuned_models_list[[1]]$.metrics[[1]]
# 
# glmnet_grid_results <- tuned_models_list[[1]]
# 
# glmnet_grid_results %>%
#   collect_metrics() %>%
#   ggplot(aes(penalty, mean, color = .metric)) +
#   geom_line(size = 1.5) +
#   facet_wrap(~.metric, scales = "free", nrow = 2) +
#   scale_x_log10()
# 
# glmnet_grid_results %>%
#   autoplot()

