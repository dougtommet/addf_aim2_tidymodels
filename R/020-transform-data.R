rm(list = setdiff(ls(), lsf.str()))
source(here::here("R", "002-folder_paths_and_options.R"))

sages.apoe <-            readRDS(file=path(r.objects.folder.tidymodel, "010_sages_apoe.rds"))
sages.del.assessment <-  readRDS(file=path(r.objects.folder.tidymodel, "010_sages_del_assessment.rds"))
sages.mr <-              readRDS(file=path(r.objects.folder.tidymodel, "010_sages_mr.rds"))
sages.proxy.interview <- readRDS(file=path(r.objects.folder.tidymodel, "010_sages_proxy_iterview.rds"))
sages.slopes.36M <-      readRDS(file=path(r.objects.folder.tidymodel, "010_sages_slopes_36M.rds"))
sages.slopes.48M <-      readRDS(file=path(r.objects.folder.tidymodel, "010_sages_slopes_48M.rds"))
sages.subject <-         readRDS(file=path(r.objects.folder.tidymodel, "010_sages_subject.rds"))

### Recoding medical record data
## Lab values
# Imputing missing lab values at the mean value.  Missing lab values are considered normal.
mean_imp <- function(x) {
  ifelse(is.na(x), mean(x, na.rm=TRUE), x)
}
sages.mr <- sages.mr %>%
  mutate_at(vars(starts_with("vdlab")), .funs = list(imp = ~mean_imp(.)))

# Create indicators for high and low lab values
sages.mr <- sages.mr %>%
  mutate(lab.anion.gap = (vdlab05a_imp + vdlab06a_imp) - (vdlab07a_imp + vdlab08a_imp),
         lab.corrected.calcium = vdlab14a_imp + 0.8*(4-vdlab13a_imp),
         lab.bun.cre.ratio = vdlab03a_imp/vdlab04a_imp,
         lab.wbc.lo     = factor(vdlab01a_imp<=2,             
                                 levels = c("FALSE", "TRUE"), labels = c("WBC > 2", "WBC <= 2")),
         lab.wbc.hi     = factor(vdlab01a_imp>=12,            
                                 levels = c("FALSE", "TRUE"), labels = c("WBC < 12", "WBC >= 12")),
         lab.hct.lo     = factor(vdlab02a_imp<=30,            
                                 levels = c("FALSE", "TRUE"), labels = c("HCT > 30", "HCT <= 30")),
         lab.hct.hi     = factor(vdlab02a_imp>=50,            
                                 levels = c("FALSE", "TRUE"), labels = c("HCT < 50", "HCT >= 50")),
         lab.bun.hi     = factor(vdlab03a_imp>=20,            
                                 levels = c("FALSE", "TRUE"), labels = c("BUN < 20", "BUN >= 20")),
         lab.cre.hi     = factor(vdlab04a_imp>=1.8,           
                                 levels = c("FALSE", "TRUE"), labels = c("CRE < 1.8", "CRE >= 1.8")),
         lab.sod.lo     = factor(vdlab05a_imp<=132,           
                                 levels = c("FALSE", "TRUE"), labels = c("SOD > 132", "SOD <= 132")),
         lab.sod.hi     = factor(vdlab05a_imp>=148,           
                                 levels = c("FALSE", "TRUE"), labels = c("SOD < 148", "SOD >= 148")),
         lab.pot.lo     = factor(vdlab06a_imp<=3.1,           
                                 levels = c("FALSE", "TRUE"), labels = c("POT > 3.1", "POT <= 3.1")),
         lab.pot.hi     = factor(vdlab06a_imp>=5.5,           
                                 levels = c("FALSE", "TRUE"), labels = c("POT < 5.5", "POT >= 5.5")),
         lab.anion.hi   = factor(lab.anion.gap>=16,           
                                 levels = c("FALSE", "TRUE"), labels = c("Anion Gap < 16", "Anion Gap >= 16")),
         lab.alt.hi     = factor(vdlab10a_imp>=50,            
                                 levels = c("FALSE", "TRUE"), labels = c("ALT < 50", "ALT >= 50")),
         lab.ast.hi     = factor(vdlab11a_imp>=50,            
                                 levels = c("FALSE", "TRUE"), labels = c("AST < 50", "AST >= 50")),
         lab.bil.hi     = factor(vdlab12a_imp>=1.5,           
                                 levels = c("FALSE", "TRUE"), labels = c("BIL < 1.5", "BIL >= 1.5")),
         lab.alb.lo     = factor(vdlab13a_imp<=3.5,           
                                 levels = c("FALSE", "TRUE"), labels = c("ALB > 3.5", "ALB <= 3.5")),
         lab.alb.hi     = factor(vdlab13a_imp>=5.2,           
                                 levels = c("FALSE", "TRUE"), labels = c("ALB < 5.2", "ALB >= 5.2")),
         lab.cal.lo     = factor(lab.corrected.calcium<=8.5,  
                                 levels = c("FALSE", "TRUE"), labels = c("CAL > 8.5", "CAL <= 8.5")),
         lab.cal.hi     = factor(lab.corrected.calcium>=10.5, 
                                 levels = c("FALSE", "TRUE"), labels = c("CAL < 10.5", "CAL >= 10.5")),
         lab.glu.lo     = factor(vdlab09a_imp<=50,            
                                 levels = c("FALSE", "TRUE"), labels = c("GLU > 50", "GLU <= 50")),
         lab.glu.hi     = factor(vdlab09a_imp>=190,           
                                 levels = c("FALSE", "TRUE"), labels = c("GLU < 190", "GLU >= 190")),
         lab.bun.cre.hi = factor(lab.bun.cre.ratio>=18,       
                                 levels = c("FALSE", "TRUE"), labels = c("BUN/CRE < 18", "BUN/CRE >= 18")),
         lab.o2.lo      = factor(vdlab15a_imp<=92,            
                                 levels = c("FALSE", "TRUE"), labels = c("O2 > 92", "O2 <= 92")),
         lab.wbc.lo.10     = factor(percent_rank(vdlab01a_imp)<0.10,      
                                    levels = c("FALSE", "TRUE"), labels = c("WBC normal", "WBC lowest 10%")),
         lab.wbc.hi.10     = factor(percent_rank(vdlab01a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("WBC normal", "WBC highest 10%")),
         lab.hct.lo.10     = factor(percent_rank(vdlab02a_imp)<0.10,      
                                    levels = c("FALSE", "TRUE"), labels = c("HCT normal", "HCT lowest 10%")),
         lab.hct.hi.10     = factor(percent_rank(vdlab02a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("HCT normal", "HCT highest 10%")),
         lab.cre.hi.10     = factor(percent_rank(vdlab04a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("CRE normal", "CRE highest 10%")),
         lab.sod.lo.10     = factor(percent_rank(vdlab05a_imp)<0.10,      
                                    levels = c("FALSE", "TRUE"), labels = c("SOD normal", "SOD lowest 10%")),
         lab.sod.hi.10     = factor(percent_rank(vdlab05a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("SOD normal", "SOD highest 10%")),
         lab.pot.lo.10     = factor(percent_rank(vdlab06a_imp)<0.10,      
                                    levels = c("FALSE", "TRUE"), labels = c("POT normal", "POT lowest 10%")),
         lab.pot.hi.10     = factor(percent_rank(vdlab06a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("POT normal", "POT highest 10%")),
         lab.alt.hi.10     = factor(percent_rank(vdlab10a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("ALT normal", "ALT highest 10%")),
         lab.ast.hi.10     = factor(percent_rank(vdlab11a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("AST normal", "AST highest 10%")),
         lab.bil.hi.10     = factor(percent_rank(vdlab12a_imp)>0.90,      
                                    levels = c("FALSE", "TRUE"), labels = c("BIL normal", "BIL highest 10%")),
         lab.bun.cre.hi.10 = factor(percent_rank(lab.bun.cre.ratio)>0.90, 
                                    levels = c("FALSE", "TRUE"), labels = c("BUN/CRE normal", "BUN/CRE highest 10%")),
         lab.o2.lo.10      = factor(percent_rank(vdlab15a_imp)<0.10,      
                                    levels = c("FALSE", "TRUE"), labels = c("O2 normal", "O2 lowest 10%"))) %>%
  select(-lab01a, -lab02a, -lab03a, -lab04a, -lab05a, -lab06a, -lab07a, -lab08a, -lab09a, -lab10a, 
         -lab11a, -lab12a, -lab13a, -lab14a, -lab15a, 
         -vdlab01a, -vdlab02a, -vdlab03a, -vdlab04a, -vdlab05a, -vdlab06a, -vdlab07a, -vdlab08a, -vdlab09a, -vdlab10a,
         -vdlab11a, -vdlab12a, -vdlab13a, -vdlab14a, -vdlab15a,
         -vdlab07a_imp, -vdlab08a_imp)

sages.mr.lab.cutoff.values <- sages.mr %>% 
  summarize(wbc.lo.10     = quantile(vdlab01a_imp, 0.10),
            wbc.hi.10     = quantile(vdlab01a_imp, 0.90),
            hct.lo.10     = quantile(vdlab02a_imp, 0.10),      
            hct.hi.10     = quantile(vdlab02a_imp, 0.90),      
            cre.hi.10     = quantile(vdlab04a_imp, 0.90),      
            sod.lo.10     = quantile(vdlab05a_imp, 0.10),      
            sod.hi.10     = quantile(vdlab05a_imp, 0.90),      
            pot.lo.10     = quantile(vdlab06a_imp, 0.10),      
            pot.hi.10     = quantile(vdlab06a_imp, 0.90),      
            alt.hi.10     = quantile(vdlab10a_imp, 0.90),      
            ast.hi.10     = quantile(vdlab11a_imp, 0.90),      
            bil.hi.10     = quantile(vdlab12a_imp, 0.90),      
            bun.cre.hi.10 = quantile(lab.bun.cre.ratio, 0.90), 
            o2.lo.10      = quantile(vdlab15a_imp, 0.10)) %>%
  gather(key = lab, value = cutoff)


## Creating indicators for the 10 most common comorbidities in the Charlson
# Using ICD codes to create indicators of Myocardial infarction and Connective tissue disease
icd_pad <- function(x) {
  as.character(round(x, 2))
}
sages.mr.icd <- sages.mr %>%
  select(studyid, starts_with("icd")) %>%
  mutate_if(is.numeric, .funs = list(~icd_pad(.))) %>%
  pivot_longer(cols = starts_with("icd")) %>%
  mutate(icd.mi = value %in% c("410", "412"),
         icd.ctd = value %in% c("710.0", "710.1", "710.4", "714", "714.0", "714.1", "714.2", "714.81", "725")) %>%
  group_by(studyid) %>%
  summarise(icd.mi = max(icd.mi),
            icd.ctd = max(icd.ctd)) %>%
  ungroup()
# Merge the ICD indicators back into the MR data
sages.mr <- sages.mr %>%
  left_join(sages.mr.icd, by = "studyid")

# Create the factors for the 10 comorbidities
sages.mr <- sages.mr %>%
  mutate(#1. Diabetes--mild to moderate
         comorbid.diab_mod = case_when(dxv2==1 ~ 1, 
                                       TRUE ~ 0),
         #2. Tumor, exclude luekemia, lymphoma, and metastatic cancer
         comorbid.tumor = case_when(dxm1==1 ~ 1, 
                                    TRUE ~ 0),
         #3. Peripheral vascular disease
         comorbid.pvd = case_when(dxv5==1 ~ 1,
                                  TRUE ~ 0),
         #4. Myocardial infarction
         comorbid.mi = case_when(dxc1a==1 ~ 1,
                                 icd.mi==1 ~ 1,
                                 TRUE ~ 0),
         #5. Connective tissue disease
         comorbid.ctd = case_when(dxi3==1 ~ 1,
                                  icd.ctd==1 ~ 1,
                                  TRUE ~ 0),
         #6. Chronic respiratory disease
         comorbid.crd = case_when(dxr1==1 ~ 1,
                                  TRUE ~ 0),
         #7. Carotid stenosis
         comorbid.cs = case_when(dxn4==1 ~ 1,
                                 TRUE ~ 0),
         #8. Diabetes with end organ damage
         comorbid.diab_sev = case_when(dxv3==1 ~ 1,
                                       TRUE ~ 0),
         #9. Congestive heart failure
         comorbid.chf = case_when(dxc2==1 ~ 1,
                                  TRUE ~ 0),
         #10. Stroke
         comorbid.stroke = case_when(dxn1==1 ~ 1,
                                     TRUE ~ 0)
         ) %>%
  mutate(comorbid.diab_mod = factor(comorbid.diab_mod, 
                                    levels = c(0, 1), labels = c("No diabetes", "Diabetes - Mild to moderate")),
         comorbid.tumor    = factor(comorbid.tumor, 
                                    levels = c(0, 1), labels = c("No tumor", "Tumor")),
         comorbid.pvd      = factor(comorbid.pvd, 
                                    levels = c(0, 1), labels = c("No PVD", "Peripheral vascular disease")),
         comorbid.mi       = factor(comorbid.mi, 
                                    levels = c(0, 1), labels = c("No MI", "Myocardial infarction")),
         comorbid.ctd      = factor(comorbid.ctd, 
                                    levels = c(0, 1), labels = c("No CTD", "Connective tissue disease")),
         comorbid.crd      = factor(comorbid.crd, 
                                    levels = c(0, 1), labels = c("No CRD", "Chronic respiratory disease")),
         comorbid.cs       = factor(comorbid.cs, 
                                    levels = c(0, 1), labels = c("No CS", "Carotid stenosis")),
         comorbid.diab_sev = factor(comorbid.diab_sev, 
                                    levels = c(0, 1), labels = c("No diabetes", "Diabetes - Severe with end organ damage")),
         comorbid.chf      = factor(comorbid.chf, 
                                    levels = c(0, 1), labels = c("No CHF", "Congestive heart failure")),
         comorbid.stroke   = factor(comorbid.stroke, 
                                    levels = c(0, 1), labels = c("No stroke", "Stroke")))

# Comorbities, Insurance status, Anesthesia
sages.mr <- sages.mr %>%
  mutate(vdcci3     = fct_collapse(as.character(vdcci), "cci_0" = "0", "cci_1" = "1", "cci_2_7" = c("2", "3", "4", "5", "6", "7")),
         vdvascom   = fct_recode(as.character(vdvascom), "No comorbidity" = "0", "Vascular comorbidity" = "1"),
         ins01      = fct_recode(as.character(ins01), "Insurance" = "2") %>% fct_expand("No Insurance"),
         ins02      = fct_recode(as.character(ins02), "Medicare" = "1", "No Medicare" = "2") %>% fct_relevel("Medicare", "No Medicare") ,
         ins03      = fct_recode(as.character(ins03), "Private Insurance" = "1", "No Private Insurance" = "2") %>% fct_relevel("Private Insurance", "No Private Insurance"),
         ins04      = fct_recode(as.character(ins04), "Medicaid" = "1", "No Medicaid" = "2") %>% fct_relevel("No Medicaid", "Medicaid"),
         ins05      = fct_recode(as.character(ins05), "No Other Insurance" = "2", "Other Insurance" = "1") %>% fct_relevel("No Other Insurance", "Other Insurance"),
         ci01       = fct_recode(as.character(ci01), "No Cognitive Impairment" = "2",  "Cognitive Impairment (Chart)" = "1") %>%
                         fct_relevel(c("No Cognitive Impairment", "Cognitive Impairment (Chart)")),
         dep01      = fct_collapse(as.character(dep01), "No Depression" = c("2", "8"), "Depression (Chart)" = "1") %>%
                          fct_relevel(c("No Depression", "Depression (Chart)")),
         op01_3     = fct_collapse(as.character(op01), "ASA class:_1_2" = c("1", "2"), "ASA class:_3_4" = c("3", "4")),
         vdanesth   = case_when((op04a==1 & op04b==2) ~ "1",
                                (op04a==2 & op04b==1) ~ "2",
                                (op04a==1 & op04b==1) ~ "3",
                                (op04a==2 & op04b==2 & op04c==1) ~ "2"),
         vdanesth   = fct_recode(vdanesth, "General Anesthesia" = "1", "Spinal Anesthesia" = "2", "General and Spinal Anesthesia" = "3"),
         vdanesth_spi = fct_collapse(vdanesth, "General Anesthesia only" = "General Anesthesia", 
                                     "Any Spinal Anesthesia" = c("Spinal Anesthesia", "General and Spinal Anesthesia"))) %>%
  select(-op04a, -op04b, -op04c, -icd.mi, -icd.ctd)


sages.combined <- sages.subject %>%
  select(studyid, dispo_ccbl, timefr, vdage, vdfemale, vdnonwhite, vdeduc_r, vdlivesalone, 
         vdsmokingstatus, vdalcohol, vdmlta_metmins,
         vdgcp_rta,  wtar01, 
         vdadlany, vdiadlany, vdiadlanyc,  
         vdgds15, vdsf12pcs, vdsf12mcs, vdsf12pf, vdsf12rp, vdsf12bp, vdsf12gh, vdsf12vt, vdsf12sf, vdsf12re, vdsf12mh,
         vdhearingimp, vddrisk93_1, 
         vdfriedfrail1, vdfriedfrail2, vdfriedfrail3, vdfriedfrail4, vdfriedfrail5,
         vdsurg, 
         vdcrp_preop, vdcrp_pod2,
         vd3ms, vdbmi3, vdesl, pai03) %>%
  left_join(sages.apoe, by = "studyid") %>%
  left_join(sages.del.assessment, by = "studyid") %>%
  left_join(sages.proxy.interview) %>%
  left_join(sages.slopes.36M, by = "studyid") %>%
  left_join(sages.slopes.48M) %>%
  left_join(sages.mr) %>%
  mutate_at(vars(vdfemale, vdnonwhite, vdlivesalone, vdsmokingstatus, vdalcohol,
                 vdadlany, vdiadlany, vdiadlanyc, vdhearingimp, vddrisk93_1,
                 vdfriedfrail1, vdfriedfrail4, vdfriedfrail5, vdsurg, vdesl, 
                 vdsagesdeliriumever, apoe4), factor) %>%
  filter(dispo_ccbl!=2 | is.na(dispo_ccbl)) %>%
  select(-dispo_ccbl, -timefr, -op01, -vdanesth, -pai03, -vdlab14a_imp) %>%
  select(-vdgcp_change36m) 


# Dropping variables that aren't easily accessible to clinicians
sages.combined <- sages.combined %>%
  select(-vdsf12pcs, -vdsf12mcs, -vdsf12pf, -vdsf12rp, -vdsf12bp, -vdsf12gh, -vdsf12vt, -vdsf12sf, -vdsf12re, -vdsf12mh,
         -vdgcp_rta, -wtar01, -vdmlta_metmins, -vdcrp_preop, -vdcrp_pod2, -apoe4, -vdfriedfrail2, -vdfriedfrail3)

# Dropping the variables used to create the comorbidity indicators
sages.combined <- sages.combined %>%
  select(-icd01a, -icd02a, -icd03a, -icd04a, -icd05a, -icd06a, -icd07a, -icd08a, -icd09a, -icd10a,
         -icd11a, -icd12a, -icd13a, -icd14a, -icd15a, -icd16a, -icd17a, -icd18a, -icd19a, -icd20a,
         -dxv2,   -dxm1,   -dxv5,   -dxc1a,   -dxi3,  -dxr1,   -dxn4,   -dxv3,   -dxc2,   -dxn1)

sages.combined <- sages.combined %>%
  mutate(vdfemale        = fct_recode(vdfemale, "Male" = "0", "Female"="1"),
         vdnonwhite      = fct_recode(vdnonwhite, "White" = "0", "Nonwhite" = "1"),
         vdlivesalone    = fct_recode(vdlivesalone, "Not_alone" = "0", "Alone" = "1"),
         vdsmokingstatus = fct_recode(vdsmokingstatus, "Never_smoker" = "0", "Past_smoker" = "1", 
                                      "Current_smoker" = "2"),
         vdalcohol       = fct_collapse(vdalcohol, "Less_than_5_a_week" = c("3", "4", "5", "6", "7", "8"),
                                        "Five_times_a_week_or_more" = c("1", "2")),
         vdalcohol       = fct_relevel(vdalcohol, "Less_than_5_a_week", "Five_times_a_week_or_more"),
         vdadlany        = fct_recode(vdadlany, "No_impairment" = "0", "Impairment" = "1"),
         vdiadlany       = fct_recode(vdiadlany, "No_impairment" = "0", "Impairment" = "1"),
         vdiadlanyc      = fct_recode(vdiadlanyc, "No_impairment" = "0", "Impairment" = "1"),
         vdhearingimp    = fct_recode(vdhearingimp, "No_impairment" = "0", "Impairment" = "1"),
         vddrisk93_1     = fct_recode(vddrisk93_1, "No_impairment" = "0", "Impairment" = "1"),
         vdfriedfrail1   = fct_recode(vdfriedfrail1, "No_impairment" = "0", "Weight_loss" = "1"),
         vdfriedfrail4   = fct_recode(vdfriedfrail4, "No_impairment" = "0", "Low_grip_strength" = "1"),
         vdfriedfrail5   = fct_recode(vdfriedfrail5, "No_impairment" = "0", "Slow_timed_walk" = "1"),
         vdsurg          = fct_recode(vdsurg, "Orthopedic" = "1", "Vascular" = "2", "Gastrointestinal" = "3"),
         vdesl           = fct_recode(vdesl, "Non_ESL" = "0", "ESL" = "1"))

saveRDS(sages.combined,   file=path(r.objects.folder.tidymodel, "020_sages_combined.rds"))
