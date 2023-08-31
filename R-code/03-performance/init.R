source("../init-global.R", chdir = TRUE)
## =====================
## functions
## =====================
##
source(file.path(functions_dir, "jaccard_function.R"))
source(file.path(functions_dir, "fdr_function.R"))
source(file.path(functions_dir, "sens_function.R"))
source(file.path(functions_dir, "spec_function.R"))
source("../01-scenario1/init.R", chdir = TRUE)
source("../02-scenario2/init.R", chdir = TRUE)

## Sset ggplot theme
theme_set(theme_bw())