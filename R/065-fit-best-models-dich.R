
rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

wflow_list_best_dich <- readRDS(file=path(r.objects.folder.tidymodel, "055_wflow_list_best_dich.rds"))
sages_train_dich     <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_train_dich.rds"))
sages_test_dich      <- readRDS(file=path(r.objects.folder.tidymodel, "030_sages_test_dich.rds"))
model_names_dich     <- readRDS(file=path(r.objects.folder.tidymodel, "045_model_names_dich.rds"))
model_df_dich        <- readRDS(file=path(r.objects.folder.tidymodel, 
                                          "045_model_df_dich.rds"))

### Refit using the whole training data
trained_models_list_dich <- map(.x = wflow_list_best_dich, 
                                ~parsnip::fit(.x, 
                                              data = sages_train_dich))
### Predict on the test data
results_list_dich <- map(.x = trained_models_list_dich, 
                    ~predict(.x, new_data = sages_test_dich, type = "prob"))
results_list_dich <- map(.x = results_list_dich, 
                    ~bind_cols(.x, sages_test_dich %>% select(true_decline, studyid)))

predictions_dich <- results_list_dich %>%
  enframe() %>%
  bind_cols(model = model_df_dich) %>%
  unnest(cols = c(value)) %>%
  group_by(model) %>%
  mutate(pred_pct_rank = percent_rank(.pred_Decline),
         pred_decline_67 = case_when(pred_pct_rank > (1- 286/(286+135)) ~ "Decline",
                                     TRUE ~ "No Decline"),
         pred_decline_67 = factor(pred_decline_67, levels = c("Decline", "No Decline"))
         ) %>%
  ungroup()


decline.cutpoints_dich <- predictions_dich %>%
  group_by(model) %>%
  filter(pred_decline_67 == "No Decline") %>%
  arrange(model, pred_pct_rank) %>%
  slice(1) %>%
  ungroup() %>%
  rename(.pred_cutpoint = .pred_Decline) %>%
  select(model, .pred_cutpoint) 

results_conf_dich <- predictions_dich %>%
  group_by(model, model_fct) %>%
  nest() %>%
  mutate(conf = map(.x = data, ~conf_mat(.x, true_decline, pred_decline_67)),
         conf_tidy = map(.x = conf, ~tidy(.x)),
         conf_summary = map(.x = conf, ~summary(.x)),
         auc = map(.x = data, ~roc_auc(.x, true_decline, .pred_Decline)))

saveRDS(predictions_dich,   file=path(r.objects.folder.tidymodel, "065_predictions_dich.rds"))
saveRDS(results_conf_dich,  file=path(r.objects.folder.tidymodel, "065_results_conf_dich.rds"))
saveRDS(decline.cutpoints_dich,   file=path(r.objects.folder.tidymodel, "065_decline_cutpoints_dich.rds"))

