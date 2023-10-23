source("../init-global.R", chdir = TRUE)
## =====================
## functions
## =====================
##
source(file.path(functions_dir, "jaccard_empirical_function.R"))
source(file.path(functions_dir, "fdr_empirical_function.R"))
source(file.path(functions_dir, "sens_empirical_function.R"))
source(file.path(functions_dir, "spec_empirical_function.R"))
## Sset ggplot theme
theme_set(theme_bw())