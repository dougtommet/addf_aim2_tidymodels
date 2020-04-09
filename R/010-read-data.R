### This file reads in the raw data.  
# It's input is the raw data files.
# It's output is the following R objects:
# saveRDS(sages.apoe,            file=path(r.objects.folder.tidymodel, "010_sages.apoe.rds"))
# saveRDS(sages.del.assessment,  file=path(r.objects.folder.tidymodel, "010_sages.del.assessment.rds"))
# saveRDS(sages.mr,              file=path(r.objects.folder.tidymodel, "010_sages.mr.rds"))
# saveRDS(sages.proxy.interview, file=path(r.objects.folder.tidymodel, "010_sages.proxy.iterview.rds"))
# saveRDS(sages.slopes.36M,      file=path(r.objects.folder.tidymodel, "010_sages.slopes.36M.rds"))
# saveRDS(sages.slopes.48M,      file=path(r.objects.folder.tidymodel, "010_sages.slopes.48M.rds"))
# saveRDS(sages.subject,         file=path(r.objects.folder.tidymodel, "010_sages.subject.rds"))

### Read in the data

# The (36M) slope variables aren't yet in a frozen file.  They are still in the processing file.
# vdgcp_slope36m = the slope at 36M from the quadratic model
# vdgcp_change36m = the change from from baseline to 36M from the quadratic model

sages.slopes.36M <- read_dta(path(sagesdatafolder, "derived", "clean", "processingfiles", "SAGES-Subject-Interview-Data-Analysis-File.dta"))
attributes(sages.slopes.36M$studyid) <- NULL

sages.slopes.36M <- sages.slopes.36M %>%
  filter(grepl("S", studyid)) %>%
  filter(timefr==0) %>%
  select(studyid, vdgcp_slope36m, vdgcp_change36m)

# The 48M slope
sages.slopes.48M <- read_dta(path(sagesdatafolder.frozen, "slope48.dta")) 
attributes(sages.slopes.48M$studyid) <- NULL

sages.slopes.48M <- sages.slopes.48M %>%
  rename(vdgcp_slope48m = vdgcp_slope48) %>%
  filter(grepl("S", studyid)) %>%
  filter(timefr==0) %>%
  select(-timefr)


# # These are the slopes used by Devore.  They are calculated through 36M
# sages.slopes.36M <- read.csv("~/documents/dwork/sages/projects/p1a1/posted/data/derived/slopes.csv", stringsAsFactors=FALSE)
# sages.slopes.36M <- sages.slopes.36M %>%
#   rename(vdgcp_slope = f_s1)

# Get main subject file
# Keep only the baseline data

sages.subject <- read_dta(path(sagesdatafolder.frozen, "SAGES-Subject-Interview-Data-Analysis-File.dta")) 
attributes(sages.subject$studyid) <- NULL

sages.subject <- sages.subject %>%
  filter(grepl("S", studyid)) %>%
  filter(timefr==0)

# Get delirium status
# Since the variable is time-invariant, keeping only the first record
sages.del.assessment <- read_dta(path(sagesdatafolder.frozen, "SAGES-Delirium-Assessments-Analysis-File.dta"))
attributes(sages.del.assessment$studyid) <- NULL
attributes(sages.del.assessment$vdsagesdeliriumever) <- NULL

sages.del.assessment <- sages.del.assessment %>% 
  select("studyid", "vdsagesdeliriumever") %>%
  group_by(studyid) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(vdsagesdeliriumever = factor(vdsagesdeliriumever),
         vdsagesdeliriumever = fct_collapse(vdsagesdeliriumever, "No Delirium" = c("0", "1"), "Delirium" = "2" ))

# Get baseline proxy IQCODE
sages.proxy.interview <- read_dta(path(sagesdatafolder.frozen, "SAGES-Proxy-Interview-Data-Analysis-File.dta"))
attributes(sages.proxy.interview$studyid) <- NULL

sages.proxy.interview <- sages.proxy.interview %>% 
  filter(timefr==0) %>%
  filter(grepl("S", studyid)) %>%
  select("studyid", "vdiqc") %>%
  rename(vdiqc_proxy = vdiqc)

# Get APOE4 status    
sages.apoe <- read_dta(path(sagesdatafolder, "Source", "Project2", "apoefinal_051914.dta"))
names(sages.apoe) <- tolower(names(sages.apoe))
attributes(sages.apoe$studyid) <- NULL
sages.apoe <- sages.apoe %>%
  select("studyid", "apoe4")

# Get Charlson score, vascular score, lab values
# Setting 999 lab values to missing
# Recoding lab values for certain participants

sages.mr <- read_dta(path(sagesdatafolder.frozen, "SAGES-Medical-Record-Data-Analysis-File.dta"))
attributes(sages.mr$studyid) <- NULL
sages.mr <- sages.mr %>%
  select("studyid", "vdcci", "vdvascom",  
         "ins01", "ins02", "ins03", "ins04", "ins05", "op01", "ci01", "dep01", "op04a", "op04b", "op04c",
         "icd01a", "icd02a", "icd03a", "icd04a", "icd05a", "icd06a", "icd07a", "icd08a", "icd09a", "icd10a",
         "icd11a", "icd12a", "icd13a", "icd14a", "icd15a", "icd16a", "icd17a", "icd18a", "icd19a", "icd20a",
         "dxv2", "dxm1", "dxv5", "dxc1a", "dxi3", "dxr1", "dxn4", "dxv3", "dxc2", "dxn1",   
         "lab01a", "lab02a", "lab03a", "lab04a", "lab05a", "lab06a", "lab07a", "lab08a", "lab09a", "lab10a",
         "lab11a", "lab12a", "lab13a", "lab14a", "lab15a") %>%
  mutate(vdlab01a = car::recode(lab01a, "999.9:999.91 = NA"),
         vdlab02a = car::recode(lab02a, "99.9:99.91 = NA"),
         vdlab03a = car::recode(lab03a, "999 = NA"),
         vdlab04a = car::recode(lab04a, "99.9:99.91 = NA"),
         vdlab05a = car::recode(lab05a, "999 = NA"),
         vdlab06a = car::recode(lab06a, "99.9:99.91 = NA"),
         vdlab07a = car::recode(lab07a, "999 = NA"),
         vdlab08a = car::recode(lab08a, "99.9:99.91 = NA"),
         vdlab09a = car::recode(lab09a, "999 = NA"),
         vdlab10a = car::recode(lab10a, "99 = NA; 999 = NA"),
         vdlab11a = car::recode(lab11a, "999 = NA"),
         vdlab12a = car::recode(lab12a, "99.9:99.91 = NA"),
         vdlab13a = car::recode(lab13a, "9.89:9.91 = NA"),
         vdlab14a = car::recode(lab14a, "99.9:99.91 = NA"),
         vdlab15a = car::recode(lab15a, "999 = NA")) %>%
  filter(str_detect(studyid, "S")) 

# sages.mr$vdlab01a[sages.mr$studyid=="SD000144"] <- 5.7
# sages.mr$vdlab08a[sages.mr$studyid=="SD000144"] <- 25.0
# sages.mr$vdlab09a[sages.mr$studyid=="SD000144"] <- 114
# sages.mr$vdlab10a[sages.mr$studyid=="SD000144"] <- 12
# sages.mr$vdlab11a[sages.mr$studyid=="SD000144"] <- 16
# sages.mr$vdlab12a[sages.mr$studyid=="SD000144"] <- 0.5
# sages.mr$vdlab13a[sages.mr$studyid=="SD000144"] <- 4.5
# sages.mr$vdlab14a[sages.mr$studyid=="SD000144"] <- 9.8
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD001015"] <- 6.9
# sages.mr$vdlab04a[sages.mr$studyid=="SD001015"] <- 1.1
# sages.mr$vdlab05a[sages.mr$studyid=="SD001015"] <- 138
# sages.mr$vdlab06a[sages.mr$studyid=="SD001015"] <- 4.1
# sages.mr$vdlab07a[sages.mr$studyid=="SD001015"] <- 100
# sages.mr$vdlab08a[sages.mr$studyid=="SD001015"] <- 28
# sages.mr$vdlab09a[sages.mr$studyid=="SD001015"] <- 90
# sages.mr$vdlab10a[sages.mr$studyid=="SD001015"] <- 18
# sages.mr$vdlab11a[sages.mr$studyid=="SD001015"] <- 15
# sages.mr$vdlab12a[sages.mr$studyid=="SD001015"] <- 0.5
# sages.mr$vdlab13a[sages.mr$studyid=="SD001015"] <- 4.5
# sages.mr$vdlab14a[sages.mr$studyid=="SD001015"] <- 9.1
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002465"] <- 10.8
# sages.mr$vdlab02a[sages.mr$studyid=="SD002465"] <- 40.9
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002617"] <- 5.9
# sages.mr$vdlab02a[sages.mr$studyid=="SD002617"] <- 40.8
# sages.mr$vdlab03a[sages.mr$studyid=="SD002617"] <- 23
# sages.mr$vdlab04a[sages.mr$studyid=="SD002617"] <- 1.3   
# sages.mr$vdlab05a[sages.mr$studyid=="SD002617"] <- 140
# sages.mr$vdlab06a[sages.mr$studyid=="SD002617"] <- 3.8
# sages.mr$vdlab07a[sages.mr$studyid=="SD002617"] <- 104
# sages.mr$vdlab08a[sages.mr$studyid=="SD002617"] <- 28.0
# sages.mr$vdlab09a[sages.mr$studyid=="SD002617"] <- 111
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002625"] <- 13.6
# sages.mr$vdlab02a[sages.mr$studyid=="SD002625"] <- 30.7
# sages.mr$vdlab03a[sages.mr$studyid=="SD002625"] <- 41
# sages.mr$vdlab04a[sages.mr$studyid=="SD002625"] <- 1.6
# sages.mr$vdlab05a[sages.mr$studyid=="SD002625"] <- 131
# sages.mr$vdlab06a[sages.mr$studyid=="SD002625"] <- 4.6
# sages.mr$vdlab07a[sages.mr$studyid=="SD002625"] <- 94
# sages.mr$vdlab09a[sages.mr$studyid=="SD002625"] <- 169
# sages.mr$vdlab10a[sages.mr$studyid=="SD002625"] <- 30
# sages.mr$vdlab11a[sages.mr$studyid=="SD002625"] <- 23
# sages.mr$vdlab12a[sages.mr$studyid=="SD002625"] <- 0.5
# sages.mr$vdlab13a[sages.mr$studyid=="SD002625"] <- 3.4
# sages.mr$vdlab14a[sages.mr$studyid=="SD002625"] <- 9.7
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002812"] <- 6.4
# sages.mr$vdlab02a[sages.mr$studyid=="SD002812"] <- 29.0
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SW003520"] <- 5.7
# sages.mr$vdlab02a[sages.mr$studyid=="SW003520"] <- 40.6
# sages.mr$vdlab03a[sages.mr$studyid=="SW003520"] <- 24
# sages.mr$vdlab04a[sages.mr$studyid=="SW003520"] <- 0.8
# sages.mr$vdlab05a[sages.mr$studyid=="SW003520"] <- 137
# sages.mr$vdlab06a[sages.mr$studyid=="SW003520"] <- 4.4
# sages.mr$vdlab07a[sages.mr$studyid=="SW003520"] <- 101
# sages.mr$vdlab09a[sages.mr$studyid=="SW003520"] <- 30.0
# sages.mr$vdlab10a[sages.mr$studyid=="SW003520"] <- 120
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002178"] <- 4.3
# sages.mr$vdlab02a[sages.mr$studyid=="SD002178"] <- 36.3
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002183"] <- 6.5
# sages.mr$vdlab02a[sages.mr$studyid=="SD002183"] <- 36.8
# sages.mr$vdlab10a[sages.mr$studyid=="SD002183"] <- 18
# sages.mr$vdlab11a[sages.mr$studyid=="SD002183"] <- 20
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002388"] <- 10.6
# sages.mr$vdlab02a[sages.mr$studyid=="SD002388"] <- 46.5
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002440"] <- 9.4
# sages.mr$vdlab02a[sages.mr$studyid=="SD002440"] <- 38.6
# sages.mr$vdlab03a[sages.mr$studyid=="SD002440"] <- 20
# sages.mr$vdlab04a[sages.mr$studyid=="SD002440"] <- 0.8
# sages.mr$vdlab05a[sages.mr$studyid=="SD002440"] <- 141
# sages.mr$vdlab06a[sages.mr$studyid=="SD002440"] <- 3.6
# sages.mr$vdlab07a[sages.mr$studyid=="SD002440"] <- 101
# sages.mr$vdlab08a[sages.mr$studyid=="SD002440"] <- 30.0
# sages.mr$vdlab09a[sages.mr$studyid=="SD002440"] <- 105
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002490"] <- 4.9
# sages.mr$vdlab02a[sages.mr$studyid=="SD002490"] <- 40.2
# sages.mr$vdlab03a[sages.mr$studyid=="SD002490"] <- 15
# sages.mr$vdlab04a[sages.mr$studyid=="SD002490"] <- 0.6
# sages.mr$vdlab05a[sages.mr$studyid=="SD002490"] <- 140
# sages.mr$vdlab06a[sages.mr$studyid=="SD002490"] <- 4.1
# sages.mr$vdlab07a[sages.mr$studyid=="SD002490"] <- 106
# sages.mr$vdlab09a[sages.mr$studyid=="SD002490"] <- 73
# sages.mr$vdlab10a[sages.mr$studyid=="SD002490"] <- 18
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002628"] <- 11.2
# sages.mr$vdlab02a[sages.mr$studyid=="SD002628"] <- 38.4
# sages.mr$vdlab03a[sages.mr$studyid=="SD002628"] <- 12
# sages.mr$vdlab04a[sages.mr$studyid=="SD002628"] <- 0.6
# sages.mr$vdlab05a[sages.mr$studyid=="SD002628"] <- 143
# sages.mr$vdlab06a[sages.mr$studyid=="SD002628"] <- 4.0
# sages.mr$vdlab07a[sages.mr$studyid=="SD002628"] <- 101
# sages.mr$vdlab08a[sages.mr$studyid=="SD002628"] <- 31
# sages.mr$vdlab09a[sages.mr$studyid=="SD002628"] <- 103
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002916"] <- 6.5
# sages.mr$vdlab02a[sages.mr$studyid=="SD002916"] <- 35.9
# sages.mr$vdlab03a[sages.mr$studyid=="SD002916"] <- 12
# sages.mr$vdlab05a[sages.mr$studyid=="SD002916"] <- 141
# sages.mr$vdlab06a[sages.mr$studyid=="SD002916"] <- 3.2
# sages.mr$vdlab07a[sages.mr$studyid=="SD002916"] <- 103
# sages.mr$vdlab08a[sages.mr$studyid=="SD002916"] <- 30.0
# sages.mr$vdlab09a[sages.mr$studyid=="SD002916"] <- 101
# 
# sages.mr$vdlab01a[sages.mr$studyid=="SD002682"] <- 6.3
# sages.mr$vdlab02a[sages.mr$studyid=="SD002682"] <- 36.8

### Save the R objects
saveRDS(sages.apoe,            file=path(r.objects.folder.tidymodel, "010_sages_apoe.rds"))
saveRDS(sages.del.assessment,  file=path(r.objects.folder.tidymodel, "010_sages_del_assessment.rds"))
saveRDS(sages.mr,              file=path(r.objects.folder.tidymodel, "010_sages_mr.rds"))
saveRDS(sages.proxy.interview, file=path(r.objects.folder.tidymodel, "010_sages_proxy_iterview.rds"))
saveRDS(sages.slopes.36M,      file=path(r.objects.folder.tidymodel, "010_sages_slopes_36M.rds"))
saveRDS(sages.slopes.48M,      file=path(r.objects.folder.tidymodel, "010_sages_slopes_48M.rds"))
saveRDS(sages.subject,         file=path(r.objects.folder.tidymodel, "010_sages_subject.rds"))


