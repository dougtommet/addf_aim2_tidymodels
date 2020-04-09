
knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

analysisfolder <- here()
projectfolder <- fs::path(fs::path_home(), "documents", "dwork", "ADDF", "project1")
datafolder <- fs::path(projectfolder, "posted", "data")
sagesdatafolder <- fs::path(fs::path_home(), "documents", "dwork", "sages", "posted", "data")
sagesdatafolder.frozen <- fs::path(sagesdatafolder, "derived", "CLEAN", "frozenfiles", "freeze_2019-02-28")

r.objects.folder.dm <- fs::path(datafolder, "R_objects", "data_management")
r.objects.folder.aim1 <- fs::path(datafolder, "R_objects", "aim1")
r.objects.folder.aim2 <- fs::path(datafolder, "R_objects", "aim2")
r.objects.folder.tidymodel <- fs::path(datafolder, "R_objects", "tidymodel")

# 
# if (!fs::dir_exists(r.objects.folder.tidymodel)) {
#   fs::dir_create(r.objects.folder.tidymodel)
# }
# # Delete R_objects directory and start fresh
# if (params$r_objects.tidymodel.start.fresh) {
#   fs::dir_delete(r.objects.folder.tidymodel)
#   fs::dir_create(r.objects.folder.tidymodel)
# }
# 
