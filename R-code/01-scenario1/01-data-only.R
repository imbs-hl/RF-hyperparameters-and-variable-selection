## This file will generate data only
if(basename(getwd()) == "R-code"){
  source("init-global.R", chdir = TRUE)
}
setwd(file.path(main_dir, "01-scenario1"))
source("init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((is.null(partition)) | is.null(account)) & (!interactive_session)){
  stop("Configure your batchtools account.")
}
## Build jobs for alternative case
## Parameter sets for Boruta
n <- 100
k <- c(10, 50)
q <- 1:6
p <- 5000
null_case <- FALSE
pValue <- 0.01
doTrace <- 1
alpha <- 0.05
no.threads <- 1L
maxRuns <- 100

## Random forests hyperparameter settings
replace <- c(TRUE, FALSE)
sample.fraction <- c(0.200, 0.400, 0.632, 0.800, 1.000)
mtry <- c(0.5, 0.3, 0.2, 0.1, 0.014)
nodesize.prop <- c(0.01, 0.05, 0.1, 0.15, 0.2)#seq(from = 1/n, to = 21/n, 3/n)
num.trees <- 1e4

importance = "impurity_corrected"
holdout = FALSE

## Just some replicates if the system is in the testing mode, and 100 otherwise.
seed <- if(testing_mode){
  ## Variation of mtry
  hyperparam_settings <- data.frame(nodesize.prop = 0.01,
                                    no.threads = no.threads,
                                    replace = TRUE,
                                    sample.fraction = 0.632,
                                    mtry = mtry,
                                    num.trees = num.trees,
                                    holdout = holdout)
  1:5
} else {
  hyperparam_settings <- expand.grid(nodesize.prop,
                                     no.threads,
                                     replace,
                                     sample.fraction,
                                     mtry,
                                     num.trees,
                                     holdout)
  names(hyperparam_settings) <- c("nodesize.prop",
                                  "no.threads",
                                  "replace",
                                  "sample.fraction",
                                  "mtry",
                                  "num.trees",
                                  "holdout")
  hyperparam_settings <- data.table::as.data.table(hyperparam_settings)
  ## Exclude settings with sample.fraction == 1 & replace == FALSE
  hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 &
                                                 replace == FALSE), ]
  1:100
} 

## k_seed prepares seeds for correlation settings. This will be extended to 
## hyperparameter settings. So that, the same seeds will be set to each hyper-
## parameter setting.
k_seed <- data.frame(k = rep(k, each = length(seed)),
                     alpha = rep(alpha,
                                 each = length(rep(k, each = length(seed)))),
                     seed = seed)
## Same as expand.grid, but without setting duplicates.
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL),
                                       list(...))


all_param_settings <- expand.grid.df(as.data.frame(hyperparam_settings), k_seed)
all_param_settings <- as.data.table(all_param_settings)

## We generate the 100 datasets used for all combinations
all_param_settings <- unique(all_param_settings, by = "seed")

## Send jobs
run_boruta <- wrap_batchtools(reg_name = "data-scena1",
                              work_dir = working_dir,
                              reg_dir = registry_dir_scen1,
                              r_function = data_only_scen1,
                              vec_args = all_param_settings,
                              more_args = list(
                                n = n,
                                q = q,
                                p = p,
                                null_case = null_case,
                                doTrace = 1
                              ),
                              name = "data-cor",
                              overwrite = TRUE,
                              memory = "1g",
                              n_cpus = no.threads,
                              walltime = "20",
                              partition = partition, ## Set partition in init-global
                              account = account, ## Set account in init-global
                              test_job = FALSE,
                              wait_for_jobs = TRUE,
                              packages = c(
                                "devtools",
                                "Pomona",
                                "data.table"
                              ),
                              config_file = config_file,
                              interactive_session = interactive_session)

## Load and saved simulated data
load_data_reg <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "data-scena1"), writeable = TRUE,
  conf.file = config_file)
data_scenario1 <- batchtools::reduceResultsList(ids = seed,
                                                reg = load_data_reg)
saveRDS(object = data_scenario1,
        file = file.path(result_dir_scen1, "study1.rds"))
## Re-set the current directory.
setwd(main_dir)