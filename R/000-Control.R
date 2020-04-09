# renv::init()

renv::snapshot()

rmarkdown::render(here::here("r", "000-master.Rmd"), 
                  params = list(tune_models = FALSE))


fs::file_move(here::here("r", "000-master.html"), 
              here::here("Reports", stringr::str_c("ADDF_aim2_", Sys.Date(),".html")))
