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
source(file.path(functions_dir, "simulate_bin_target2.R"))
source(file.path(functions_dir, "test_binary.R"))
source(file.path(functions_dir, "filterout.R"))
result_dir_scen2 <- file.path(result_dir, "02-scenario/paper")
dir.create(result_dir_scen2, showWarnings = FALSE, recursive = TRUE)
