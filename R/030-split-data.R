rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

sages.combined <- readRDS(file=path(r.objects.folder.tidymodel, "020_sages_combined.rds"))
variable.df <- readRDS(file=path(r.objects.folder.dm, "021_variable_df.rds"))

covariates.to.use.1 <- variable.df %>%
  filter(covariates.to.use.1) %>%
  pull(variable)

sages.combined <- sages.combined %>% 
  select(studyid, vdgcp_slope36m, all_of(covariates.to.use.1), vdlos) %>%
  mutate(true_decline = factor(vdgcp_slope36m > -.5, 
                               labels = c("Decline", "No Decline")))

# sages.combined <- sages.combined %>%
#   select(-studyid, -vdsagesdeliriumever, -vdgcp_slope48m)

########################
# Split the data
set.seed(1234)
sages_split <- sages.combined %>%
  rsample::initial_split(strata = vdgcp_slope36m)
sages_train <- rsample::training(sages_split)
sages_test  <- rsample::testing(sages_split)
# sages_folds <- vfold_cv(sages_train, strata = vdgcp_slope36m)
sages_folds <- bootstraps(sages_train, times = 25, strata = vdgcp_slope36m)

# dichotomized version of the outcome
sages_train_dich <- sages_train %>%
  select(-vdgcp_slope36m) %>%
  select(true_decline, everything())
sages_test_dich  <- sages_test %>%
  select(-vdgcp_slope36m) %>%
  select(true_decline, everything())
sages_folds_dich <- bootstraps(sages_train_dich, times = 25, strata = true_decline)



sages.combined <- sages_train %>%
  mutate(testing_fct = 0) %>%
  bind_rows(sages_test) %>%
  mutate(testing_fct = case_when(is.na(testing_fct) ~ 1,
                                 TRUE ~ testing_fct),
         testing_fct = factor(testing_fct, 
                              labels = c("Training", "Testing"), ordered = TRUE),
         testing_char = as.character(testing_fct))

saveRDS(sages.combined,   file=path(r.objects.folder.tidymodel, "030_sages_combined.rds"))
saveRDS(sages_split,   file=path(r.objects.folder.tidymodel, "030_sages_split.rds"))
saveRDS(sages_train,   file=path(r.objects.folder.tidymodel, "030_sages_train.rds"))
saveRDS(sages_test,    file=path(r.objects.folder.tidymodel, "030_sages_test.rds"))
saveRDS(sages_folds,   file=path(r.objects.folder.tidymodel, "030_sages_folds.rds"))
saveRDS(sages_train_dich,   file=path(r.objects.folder.tidymodel, "030_sages_train_dich.rds"))
saveRDS(sages_test_dich,    file=path(r.objects.folder.tidymodel, "030_sages_test_dich.rds"))
saveRDS(sages_folds_dich,   file=path(r.objects.folder.tidymodel, "030_sages_folds_dich.rds"))
