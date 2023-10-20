source("../init-global.R", chdir = TRUE)
## =====================
## functions
## =====================
##
source(file.path(functions_dir, "jaccard_empirical_function.R"))
source(file.path(functions_dir, "fdr_empirical_function.R"))
source(file.path(functions_dir, "sens_empirical_function.R"))
source(file.path(functions_dir, "spec_empirical_function.R"))
source("../01-scenario1/init.R", chdir = TRUE)
source("../02-scenario2/init.R", chdir = TRUE)

## Sset ggplot theme
theme_set(theme_bw())