rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

sages_folds_dich <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_folds_dich.rds"))
wflow_list_dich  <- readRDS(file=path(r.objects.folder.tidymodel, "045_wflow_list_dich.rds"))
grid_list_dich   <- readRDS(file=path(r.objects.folder.tidymodel, "045_grid_list_dich.rds"))

doParallel::registerDoParallel()

##########
##### Classification models
### Tune the models
tuned_models_list_dich <- map2(.x = wflow_list_dich,
                               .y = grid_list_dich,
                               ~tune_grid(.x, 
                                          resamples = sages_folds_dich, 
                                          grid = .y))

### Get the best hyperparameters
tuned_models_best_dich <- tuned_models_list_dich %>%
  map(tune::select_best, metric = "roc_auc", maximize = TRUE)

### Put the best parameters in the workflow
wflow_list_best_dich <- map2(.x = wflow_list_dich, 
                             .y = tuned_models_best_dich, 
                             ~finalize_workflow(.x, .y))

### Save the R objects
saveRDS(tuned_models_list_dich, file=path(r.objects.folder.tidymodel, "055_tuned_models_list_dich.rds"))
saveRDS(tuned_models_best_dich, file=path(r.objects.folder.tidymodel, "055_tuned_models_best_dich.rds"))
saveRDS(wflow_list_best_dich,   file=path(r.objects.folder.tidymodel, "055_wflow_list_best_dich.rds"))
