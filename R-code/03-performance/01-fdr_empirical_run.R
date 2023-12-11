setwd(file.path(main_dir, "03-performance"))
source("init.R", chdir = TRUE)
source("../01-scenario1/init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((is.null(partition)) | is.null(account)) & (!interactive_session)){
  stop("Configure your batchtools account.")
}

## Build jobs for alternative case
## Parameter sets
n <- 100
k <- c(10, 50)
q <- 1:6
p <- 5000
null_case <- FALSE
pValue <- 0.01
doTrace <- 1
alpha <- 0.05
no.threads <- 5L
maxRuns <- 100

## Random forests hyperparameter settings
replace <- c(TRUE, FALSE)
sample.fraction <- c(0.200, 0.400, 0.632, 0.800, 1.000)
mtry <- c(0.5, 0.3, 0.2, 0.1, 0.014)
min.node.size_prop <- c(0.01, 0.05, 0.1, 0.15, 0.2)#seq(from = 1/n, to = 21/n, 3/n)
num.trees <- 1e4

importance = "impurity_corrected"
holdout = FALSE

## Just 1 replicates if the system is in the testing mode, and 100 otherwise.
seed <- if(testing_mode){
  ## Variation of mtry
  hyperparam_settings <- data.frame(min.node.size_prop = 0.01,
                                    no.threads = no.threads,
                                    replace = TRUE,
                                    sample.fraction = 0.632,
                                    mtry = mtry,
                                    num.trees = num.trees,
                                    holdout = holdout)
  1:5
} else {
  hyperparam_settings <- expand.grid(min.node.size_prop,
                                     no.threads,
                                     replace,
                                     sample.fraction,
                                     mtry,
                                     num.trees,
                                     holdout)
  names(hyperparam_settings) <- c("min.node.size_prop",
                                  "no.threads",
                                  "replace",
                                  "sample.fraction",
                                  "mtry",
                                  "num.trees",
                                  "holdout")
  hyperparam_settings <- data.table::as.data.table(hyperparam_settings)
  ## Exclude settings with sample.fraction == 1 & replace == FALSE
  hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 & replace == FALSE), ]
  1:100
} 
## k_seed prepares seeds for correlation settings. This will be extended to 
## hyperparameter settings. So that, the same seeds will be set to each hyper-
## parameter setting.
k_seed <- data.frame(k = rep(k, each = length(seed)),
                     alpha = rep(alpha, each = length(rep(k, each = length(seed)))),
                     seed = seed)
## Same as expand.grid, but without setting duplicates.
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


all_param_settings <- expand.grid.df(as.data.frame(hyperparam_settings), k_seed)
all_param_settings <- as.data.table(all_param_settings)

all_param_setting_unique <- unique(all_param_settings,
                                   by = c("min.node.size_prop",
                                          "no.threads",
                                          "replace",
                                          "sample.fraction",
                                          "mtry",
                                          "num.trees",
                                          "holdout",
                                          "k"))
names(all_param_setting_unique) <- c(".min.node.size",
                                     "no.threads",
                                     ".replace",
                                     ".sample.fraction",
                                     ".mtry.prop",
                                     "num.trees",
                                     "holdout",
                                     ".k", "alpha", "seed")

## ****** Remove unecessary parameters
all_param_setting_unique$no.threads <- NULL
all_param_setting_unique$num.trees <- NULL
all_param_setting_unique$seed <- NULL
all_param_setting_unique$alpha <- NULL
all_param_setting_unique$holdout <- NULL


## Send jobs
run_fdr_vita <- wrap_batchtools(reg_name = "fdr_empirical_vita",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = fdr_empirical_function,
                                vec_args = all_param_setting_unique,
                                more_args = list(
                                  config_file = config_file,
                                  all_param_settings = all_param_settings,
                                  reg_dir = file.path(registry_dir_scen1, "vita-cor")
                                ),
                                name = "fdr_vita",
                                overwrite = TRUE,
                                memory = "2g",
                                n_cpus = 1,
                                walltime = "5",
                                partition = partition, ## Set partition in init-global
                                account = account, ## Set account in init-global
                                test_job = FALSE,
                                wait_for_jobs = TRUE,
                                packages = c(
                                  "devtools",
                                  "data.table"
                                ),
                                config_file = config_file,
                                interactive_session = interactive_session)

## Run this after that your jobs are completed
## =======================================
## Resume FDR's result for vita
## =======================================
reg_vita_fdr <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "fdr_empirical_vita"),
  writeable = TRUE,
  conf.file = config_file)
vita_fdr_reg <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique),
    reg = reg_vita_fdr
  ),
  reg = reg_vita_fdr)


## resume filtered results
vita_fdr_DT <- data.table::rbindlist(vita_fdr_reg)

vita_fdr_DT$Method <- "Vita"

saveRDS(object = vita_fdr_DT,
        file = file.path(result_dir_scen1, "vita_cor_fdr.RDS"))


## Send Boruta jobs for k = 10
run_boruta10 <- wrap_batchtools(reg_name = "fdr_boruta10",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = fdr_empirical_function,
                                vec_args = all_param_setting_unique[.k == 10, ],
                                more_args = list(
                                  config_file = config_file,
                                  all_param_settings = all_param_settings[k == 10, ],
                                  reg_dir = file.path(registry_dir_scen1,
                                                      ## Only Vita results in
                                                      ## testing mode; that is, 
                                                      ## we mimic Boruta results
                                                      ifelse(testing_mode,
                                                             "vita-cor",
                                                             "boruta-cor10"))
                                ),
                                name = "fdr_boruta10",
                                overwrite = TRUE,
                                memory = "2g",
                                n_cpus = 1,
                                walltime = "30",
                                partition = partition, ## Set partition in init-global
                                account = account, ## Set account in init-global
                                test_job = FALSE,
                                wait_for_jobs = TRUE,
                                packages = c(
                                  "devtools",
                                  "data.table"
                                ),
                                config_file = config_file,
                                interactive_session = interactive_session)


## Send Boruta jobs for k = 50
run_boruta50 <- wrap_batchtools(reg_name = "fdr_boruta50",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = fdr_empirical_function,
                                vec_args = all_param_setting_unique[.k == 50, ],
                                more_args = list(
                                  config_file = config_file,
                                  all_param_settings = all_param_settings[k == 50, ],
                                  reg_dir = file.path(registry_dir_scen1,
                                                      ## Only Vita results in
                                                      ## testing mode; that is, 
                                                      ## we mimic Boruta results
                                                      ifelse(testing_mode,
                                                             "vita-cor",
                                                             "boruta-cor50"))
                                ),
                                name = "fdr_boruta50",
                                overwrite = TRUE,
                                memory = "2g",
                                n_cpus = 1,
                                walltime = "30",
                                partition = partition, ## Set partition in init-global
                                account = account, ## Set account in init-global
                                test_job = FALSE,
                                wait_for_jobs = TRUE,
                                packages = c(
                                  "devtools",
                                  "data.table"
                                ),
                                config_file = config_file,
                                interactive_session = interactive_session)


## Run this after that your jobs are completed
## ----------------------------------------------
## Resume jaccard's result for vita for k = 10
## ----------------------------------------------
##
reg_boruta_fdr10 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "fdr_boruta10"), writeable = TRUE,
  conf.file = config_file)
boruta_fdr_reg10 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique[.k == 10, ]),
    reg = reg_boruta_fdr10
  ),
  reg = reg_boruta_fdr10)


## resume filtered results
boruta_fdr_DT10 <- data.table::rbindlist(boruta_fdr_reg10)


## Run this after that your jobs are completed
## ----------------------------------------------
## Resume jaccard's result for vita for k = 50
## ----------------------------------------------
##
reg_boruta_fdr50 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "fdr_boruta50"), writeable = TRUE,
  conf.file = config_file)
boruta_fdr_reg50 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique[.k == 50, ]),
    reg = reg_boruta_fdr50
  ),
  reg = reg_boruta_fdr50)


## resume filtered results
boruta_fdr_DT50 <- data.table::rbindlist(boruta_fdr_reg50)

boruta_fdr_DT <- data.table::rbindlist(list(boruta_fdr_DT10,
                                            boruta_fdr_DT50))

## Only Vita in testing mode; that is, we mimic Boruta results
boruta_fdr_DT$Method <- ifelse(testing_mode, "Vita", "Boruta")

saveRDS(object = boruta_fdr_DT,
        file = file.path(result_dir_scen1, "boruta_cor_fdr.RDS"))
## Re-set the current directory.
setwd(main_dir)