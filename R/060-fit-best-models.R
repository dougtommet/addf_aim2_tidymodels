rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

wflow_list_best <- readRDS(file=path(r.objects.folder.tidymodel, "050_wflow_list_best.rds"))
sages_train     <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_train.rds"))
sages_test      <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_test.rds"))
model_names     <- readRDS(file=path(r.objects.folder.tidymodel, "040_model_names.rds"))
model_df        <- readRDS(file=path(r.objects.folder.tidymodel, "040_model_df.rds"))


### Refit using the whole training data
trained_models_list <- map(.x = wflow_list_best, 
                           ~parsnip::fit(.x, 
                                         data = sages_train))

### Predict on the test data
results_list <- map(.x = trained_models_list, 
                    ~predict(.x, new_data = sages_test))
results_list <- map(.x = results_list, 
                    ~bind_cols(.x, sages_test %>% select(vdgcp_slope36m)))

# saveRDS(model_names,     file=path(r.objects.folder.tidymodel, "060_model_names.rds"))
saveRDS(results_list,    file=path(r.objects.folder.tidymodel, "060_results_list.rds"))

# For dichotomized results
predictions <- results_list %>%
  enframe() %>%
  bind_cols(model = model_df) %>%
  unnest(cols = c(value)) 

decline.cutpoints <- predictions %>%
  group_by(model) %>%
  mutate(pred_pct_rank = 1-rank(.pred)/length(.pred)) %>%
  filter(pred_pct_rank>.8) %>%
  arrange(model, pred_pct_rank) %>%
  slice(1) %>%
  ungroup() %>%
  rename(.pred_cutpoint = .pred) %>%
  select(model, model_fct, .pred_cutpoint) 


predictions <- predictions %>%
  left_join(decline.cutpoints, by = c("model", "model_fct")) %>%
  mutate(true_decline = factor(vdgcp_slope36m > -.5, 
                               labels = c("Decline", "No Decline")),
         pred_decline = factor(.pred > .pred_cutpoint, 
                        labels = c("Decline", "No Decline")))


results_conf <- predictions %>%
  group_by(model, model_fct) %>%
  nest() %>%
  mutate(conf = map(.x = data, ~conf_mat(.x, true_decline, pred_decline)),
         conf_tidy = map(.x = conf, ~tidy(.x)),
         conf_summary = map(.x = conf, ~summary(.x)))

saveRDS(predictions,   file=path(r.objects.folder.tidymodel, "060_predictions.rds"))
saveRDS(results_conf,  file=path(r.objects.folder.tidymodel, "060_results_conf.rds"))
saveRDS(decline.cutpoints,   file=path(r.objects.folder.tidymodel, "060_decline_cutpoints.rds"))







