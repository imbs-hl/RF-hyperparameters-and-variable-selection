source("../init-global.R", chdir = TRUE)
working_dir <- file.path(main_dir, "02-scenario2")
registry_dir_scen2 <- file.path(registry_dir, "scenario2")
if(!dir.exists(registry_dir_scen2)){
  dir.create(registry_dir_scen2)
}
## =====================
## functions
## =====================
##
source(file.path(functions_dir, "simulate_bin_target.R"))
source(file.path(functions_dir, "test_binary_vita.R"))
source(file.path(functions_dir, "test_binary.R"))
source(file.path(functions_dir, "filterout.R"))
source(file.path(functions_dir, "data_only_scen2.R"))
result_dir_scen2 <- file.path(result_dir, "02-scenario2")
dir.create(result_dir_scen2, showWarnings = FALSE, recursive = TRUE)
