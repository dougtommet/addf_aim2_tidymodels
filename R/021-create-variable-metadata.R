rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

# This files creates a data frame that has meta data about the variables 
# - labels, 
# - whether to use it in the analysis, 
# - whether it has missing values

# It's input is information taken from codebooks and assessment forms.
# It's output is the following R objects:
# saveRDS(variable.df,            file=path(r.objects.folder, "021_variable.df.rds"))


variable.df <- tibble(variable = c("studyid", "vdgcp_slope36m", "vdgcp_slope48m", "vdsagesdeliriumever", "vdage", "vdfemale", "vdnonwhite", "vdeduc_r", 
                                   "vdlivesalone", "vdsmokingstatus", "vdalcohol", 
                                   "vdadlany", "vdiadlany", "vdiadlanyc", "vdgds15", "vdhearingimp", "vddrisk93_1", 
                                   "vdfriedfrail1", "vdfriedfrail4", "vdfriedfrail5", 
                                   "vdsurg", 
                                   "vd3ms", "vdbmi3", "vdesl",  
                                   "vdcci", "vdvascom", 
                                   "ins01", "ins02", "ins03", "ins04", "ins05", 
                                   "ci01", "dep01", 
                                   "vdlab01a_imp", "vdlab02a_imp", "vdlab03a_imp", "vdlab04a_imp", "vdlab05a_imp", "vdlab06a_imp", 
                                   "vdlab09a_imp", "vdlab10a_imp", "vdlab11a_imp", "vdlab12a_imp", "vdlab13a_imp", "vdlab15a_imp", 
                                   "lab.anion.gap", "lab.corrected.calcium", "lab.bun.cre.ratio", 
                                   "lab.wbc.lo", "lab.wbc.hi", "lab.hct.lo", "lab.hct.hi", "lab.bun.hi", "lab.cre.hi", 
                                   "lab.sod.lo", "lab.sod.hi", "lab.pot.lo", "lab.pot.hi", "lab.anion.hi", 
                                   "lab.alt.hi", "lab.ast.hi", "lab.bil.hi", "lab.alb.lo", "lab.alb.hi", 
                                   "lab.cal.lo", "lab.cal.hi", "lab.glu.lo", "lab.glu.hi", 
                                   "lab.bun.cre.hi", "lab.o2.lo", 
                                   "lab.wbc.lo.10", "lab.wbc.hi.10", "lab.hct.lo.10", "lab.hct.hi.10", 
                                   "lab.cre.hi.10", "lab.sod.lo.10", "lab.sod.hi.10", "lab.pot.lo.10", "lab.pot.hi.10", 
                                   "lab.alt.hi.10", "lab.ast.hi.10", "lab.bil.hi.10", 
                                   "lab.bun.cre.hi.10", "lab.o2.lo.10", 
                                   "vdcci3", "op01_3", "vdanesth_spi", 
                                   "comorbid.diab_mod", "comorbid.tumor", "comorbid.pvd", "comorbid.mi", 
                                   "comorbid.ctd", "comorbid.crd", "comorbid.cs", 
                                   "comorbid.diab_sev", "comorbid.chf", "comorbid.stroke", 
                                   "vdiqc_proxy"),
                      variable_labels = c("Studyid", "XXvdgcp_slope36m", "XXvdgcp_slope48m",  "SAGES delirium", "Age","Female", "Nonwhite", "Educ",
                                          "Lives alone", "Smoking status", "Alcohol",
                                          "ADL (any)", "IADL (any)", "IADL cognitive (any)", "Depression", "Hearing impairment", "XXvddrisk93_1",
                                          "Fraility - Weight Loss", "Fraility - Low Grip Strength", "Fraility - Low Timed Walk", 
                                          "Surgery",  
                                          "3MS", "BMI", "English is second language", 
                                          "Charlson Comorbidity Index", "Vascular Comorbidity", 
                                          "No Insurance", "Has Medicare", "Has Private Insurance", "Has Medicaid", "Has Other Insurance",
                                          "Evidence of Cognitive Impairment", "Evidence of Depression", 
                                          "WBC", "Hematocrit", "BUN", "Creatinine", "Sodium", "Potassium",              
                                          "Glucose", "ALT", "AST", "Bilirubin", "Albumin", "O2 saturation",
                                          "Anion Gap", "Corrected calcium", "BUN/creatinine ratio", 
                                          "WBC low","WBC High", "Hematocrit low", "Hematocrit high", "BUN high", "Creatinine high",
                                          "Sodium low", "Sodium high", "Potassium low", "Potassium high", "Anion Gap high",
                                          "ALT high", "AST high", "Bilirubin high", "Albumin low", "Albumin high",
                                          "Corrected calcium low", "Corrected calcium high", "Glucose low", "Glucose high", 
                                          "BUN/creatinine ratio high", "O2 saturation low",
                                          "WBC low (10%)","WBC High (10%)", "Hematocrit low (10%)", "Hematocrit high (10%)",
                                          "Creatinine high (10%)", "Sodium low (10%)", "Sodium high (10%)", "Potassium low (10%)", "Potassium high (10%)",
                                          "ALT high (10%)", "AST high (10%)", "Bilirubin high (10%)", 
                                          "BUN/creatinine ratio high (10%)", "O2 saturation low (10%)",
                                          "Charlson Comorbidity Index", "ASA class", "Anesthesia Type",
                                          "Diabetes - mild to moderate", "Tumor", "Peripheral vascular disease", "Myocardial infarction",
                                          "Connective tissue disease", "Chronic respiratory disease", "Carotid stenosis",
                                          "Diabetes - severe with end organ damage", "Congestive heart failure", "Stroke",
                                          "IQCODE (proxy)") )

# variable.df <- tibble(variable = c("studyid", "vdage", "vdfemale", "vdnonwhite", "vdeduc_r", 
#                                    "vdlivesalone", "vdpastsmoker", "vdcurrentsmoker", "vdalcohol1", 
#                                    "vdadlany", "vdiadlany", "vdiadlanyc", "vdgds15", "vdhearingimp", "vddrisk93_1", 
#                                    "vdfriedfrail1", "vdfriedfrail4", "vdfriedfrail5", 
#                                    "vdsurg", "vdsurg_vas", "vdsurg_gas", 
#                                    "vd3ms", "vdbmi3", "vdesl", "vdgcp_slope36m", "vdgcp_slope48m", 
#                                    "vdcci", "vdvascom", 
#                                    "ins01", "ins02", "ins03", "ins04", "ins05", 
#                                    "ci01", "dep01", 
#                                    "vdlab01a.imp", "vdlab02a.imp", "vdlab03a.imp", "vdlab04a.imp", "vdlab05a.imp", "vdlab06a.imp", 
#                                    "vdlab09a.imp", "vdlab10a.imp", "vdlab11a.imp", "vdlab12a.imp", "vdlab13a.imp", "vdlab15a.imp", 
#                                    "lab.anion.gap", "lab.corrected.calcium", "lab.bun.cre.ratio", 
#                                    "lab.wbc.lo", "lab.wbc.hi", "lab.hct.lo", "lab.hct.hi", "lab.bun.hi", "lab.cre.hi", 
#                                    "lab.sod.lo", "lab.sod.hi", "lab.pot.lo", "lab.pot.hi", "lab.anion.hi", 
#                                    "lab.alt.hi", "lab.ast.hi", "lab.bil.hi", "lab.alb.lo", "lab.alb.hi", 
#                                    "lab.cal.lo", "lab.cal.hi", "lab.glu.lo", "lab.glu.hi", 
#                                    "lab.bun.cre.hi", "lab.o2.lo", 
#                                    "lab.wbc.lo.10", "lab.wbc.hi.10", "lab.hct.lo.10", "lab.hct.hi.10", 
#                                    "lab.cre.hi.10", "lab.sod.lo.10", "lab.sod.hi.10", "lab.pot.lo.10", "lab.pot.hi.10", 
#                                    "lab.alt.hi.10", "lab.ast.hi.10", "lab.bil.hi.10", 
#                                    "lab.bun.cre.hi.10", "lab.o2.lo.10", 
#                                    "vdcci3", "op01_3", "vdanesth_spi", 
#                                    "comorbid.diab_mod", "comorbid.tumor", "comorbid.pvd", "comorbid.mi", 
#                                    "comorbid.ctd", "comorbid.crd", "comorbid.cs", 
#                                    "comorbid.diab_sev", "comorbid.chf", "comorbid.stroke", 
#                                    "vdiqc_proxy", "vdsagesdeliriumever"),
#                       variable_labels = c("Studyid", "Age","Female", "Nonwhite", "Educ",
#                                           "Lives alone", "Past smoker", "Current smoker", "Alcohol",
#                                           "ADL (any)", "IADL (any)", "IADL cognitive (any)", "Depression", "Hearing impairment", "XXvddrisk93_1",
#                                           "Fraility - Weight Loss", "Fraility - Low Grip Strength", "Fraility - Low Timed Walk", 
#                                           "XXvdsurg", "Surgery Type - Vascular", "Surgery Type - Gastrointestinal", 
#                                           "3MS", "BMI", "English is second language", "XXvdgcp_slope36m", "XXvdgcp_slope48m",
#                                           "Charlson Comorbidity Index", "Vascular Comorbidity", 
#                                           "No Insurance", "Has Medicare", "Has Private Insurance", "Has Medicaid", "Has Other Insurance",
#                                           "Evidence of Cognitive Impairment", "Evidence of Depression", 
#                                           "WBC", "Hematocrit", "BUN", "Creatinine", "Sodium", "Potassium",              
#                                           "Glucose", "ALT", "AST", "Bilirubin", "Albumin", "O2 saturation",
#                                           "Anion Gap", "Corrected calcium", "BUN/creatinine ratio", 
#                                           "WBC low","WBC High", "Hematocrit low", "Hematocrit high", "BUN high", "Creatinine high",
#                                           "Sodium low", "Sodium high", "Potassium low", "Potassium high", "Anion Gap high",
#                                           "ALT high", "AST high", "Bilirubin high", "Albumin low", "Albumin high",
#                                           "Corrected calcium low", "Corrected calcium high", "Glucose low", "Glucose high", 
#                                           "BUN/creatinine ratio high", "O2 saturation low",
#                                           "WBC low (10%)","WBC High (10%)", "Hematocrit low (10%)", "Hematocrit high (10%)",
#                                           "Creatinine high (10%)", "Sodium low (10%)", "Sodium high (10%)", "Potassium low (10%)", "Potassium high (10%)",
#                                           "ALT high (10%)", "AST high (10%)", "Bilirubin high (10%)", 
#                                           "BUN/creatinine ratio high (10%)", "O2 saturation low (10%)",
#                                           "Charlson Comorbidity Index", "ASA class", "Anesthesia Type",
#                                           "Diabetes - mild to moderate", "Tumor", "Peripheral vascular disease", "Myocardial infarction",
#                                           "Connective tissue disease", "Chronic respiratory disease", "Carotid stenosis",
#                                           "Diabetes - severe with end organ damage", "Congestive heart failure", "Stroke",
#                                           "IQCODE (proxy)", "SAGES delirium") )



variable.df <- variable.df %>%
  mutate(covariates.to.use.1  = variable %in% c("vdage", "vdfemale", "vdnonwhite", "vdesl", "vd3ms", "vdeduc_r",
                                                "vdbmi3", "vdlivesalone", "vdsmokingstatus",  "vdalcohol",
                                                "vdadlany", "vdiadlany", "vdiadlanyc", "vdiqc_proxy", "vdgds15",
                                                "vdhearingimp", "vdfriedfrail1", 
                                                "vdfriedfrail4", "vdfriedfrail5", "vdsurg", 
                                                "vdcci", "vdcci3", "vdvascom", 
                                                "comorbid.diab_mod", "comorbid.tumor", "comorbid.pvd", "comorbid.mi", "comorbid.ctd", 
                                                "comorbid.crd", "comorbid.cs",  "comorbid.diab_sev", "comorbid.chf", "comorbid.stroke",
                                                "ins02", "ins03", "ins04", "ins05", "dep01", "op01_3", "vdanesth_spi",
                                                "vdlab01a_imp", "vdlab02a_imp", "vdlab03a_imp", "vdlab04a_imp", "vdlab05a_imp",
                                                "vdlab06a_imp", "lab.anion.gap", "vdlab10a_imp", "vdlab11a_imp", "vdlab12a_imp", "vdlab13a_imp",
                                                "lab.corrected.calcium", "vdlab09a_imp", "lab.bun.cre.ratio", "vdlab15a_imp",
                                                "lab.wbc.lo.10", "lab.wbc.hi.10", "lab.hct.lo.10",
                                                "lab.hct.hi.10", "lab.cre.hi.10", "lab.sod.lo.10", "lab.sod.hi.10", "lab.pot.lo.10",
                                                "lab.pot.hi.10", "lab.alt.hi.10", "lab.ast.hi.10", "lab.bil.hi.10", "lab.bun.cre.hi.10",
                                                "lab.o2.lo.10"),
         covariates.to.use.2 = variable %in% c("vdage", "vdfemale", "vdnonwhite", "vdeduc_r", 
                                               "vdsmokingstatus",  "vdalcohol", "vdhearingimp",
                                               "vdcci3", "op01_3",
                                               "vdlab01a_imp", "vdlab02a_imp", "vdlab04a_imp", "lab.bun.cre.ratio", "vdlab15a_imp", "vdlab05a_imp",
                                               "vdsurg", "vdsurg_gas"),
         covariates.to.use.3 = variable %in% c("vd3ms", "vdage", "vdfemale", "vdnonwhite", "vdeduc_r", 
                                               "vdsmokingstatus",  "vdalcohol", "vdhearingimp",
                                               "vdcci3", "op01_3",
                                               "vdlab01a_imp", "vdlab02a_imp", "vdlab04a_imp", "lab.bun.cre.ratio", "vdlab15a_imp", "vdlab05a_imp",
                                               "vdsurg", "vdsurg_gas"),
         has.missing.data = variable %in% c("vdalcohol", "vdiqc_proxy", "vdgds15", "vdhearingimp", 
                                            "vdfriedfrail1", "vdfriedfrail4", "vdfriedfrail5"))



# covariates.with.missing.data <- variable.df %>%
#   filter(has.missing.data) %>%
#   pull(variable)
# 
# covariates.to.use.1 <- variable.df %>%
#   filter(covariates.to.use.1) %>%
#   pull(variable)
# 
# covariates.to.use.2 <- variable.df %>%
#   filter(covariates.to.use.2) %>%
#   pull(variable)
# 
# covariates.labels <- variable.df %>%
#   filter(covariates.to.use.1) %>%
#   pull(variable_labels)

### Save the R objects
saveRDS(variable.df,            file=path(r.objects.folder.dm, "021_variable_df.rds"))

