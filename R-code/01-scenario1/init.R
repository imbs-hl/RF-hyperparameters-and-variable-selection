source("../init-global.R", chdir = TRUE)
working_dir <- file.path(main_dir, "01-scenario1")
registry_dir_scen1 <- file.path(registry_dir, "scenario1")
if(!dir.exists(registry_dir_scen1)){
  dir.create(registry_dir_scen1)
}
## =====================
## functions
## =====================
##
# source(file.path(functions_dir, "simulate_cor_pred.R"))
source(file.path(functions_dir, "simulate_cor_bin.R"))
source(file.path(functions_dir, "alternative_cor_boruta.R"))
source(file.path(functions_dir, "alternative_cor_vita.R"))
source(file.path(functions_dir, "filterout.R"))
result_dir_scen1 <- file.path(result_dir, "01-scenario")
dir.create(result_dir_scen1, showWarnings = FALSE, recursive = TRUE)
