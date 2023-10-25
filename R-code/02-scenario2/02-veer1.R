## This file is for submitting jobs for evaluating Vita and Boruta in study 2.
source("init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((partition == "xxxx") | account == "xxxx") & (!interactive_session)){
  stop("Configure your batchtools account.")
}
data("VEER1")

## Further parameter
alpha <- 0.05
no.threads <- 2L

## Parameter setting
data_veer = t(VEER1@assayData$exprs)
data_veer <- data_veer[complete.cases(data_veer), ]

n <- nrow(data_veer)
p <- ncol(data_veer)

betas = seq(from = -0.8, to = 0.8, 0.1)
n_beta = 200
effect_seed = 1
subset = FALSE
subsetsize = 100
null_case = FALSE
independence = TRUE
alpha <- 0.05
## Just 10 replicates if the system is in the testing mode, and 100 otherwise.
seed <- seed <- if(testing_mode){
  1:10
} else {
  1:100
} 
effect_seed<- ifelse(testing_mode, 11:20, 101:200)
## Random forests hyperparameter settings
replace <- c(TRUE, FALSE)
sample.fraction <- c(0.200, 0.400, 0.632, 0.800, 1.000)
mtry <- c(0.5, 0.33, 0.25, 0.1, sqrt(p) / p)
nodesize.prop <- c(0.01, 0.05, 0.1, 0.2, 1/n)#seq(from = 1/n, to = 21/n, 3/n)
num.trees <- p*3

holdout <- FALSE

hyperparam_settings <- expand.grid(nodesize.prop,
                                   replace,
                                   sample.fraction,
                                   mtry,
                                   num.trees,
                                   holdout)
names(hyperparam_settings) <- c("nodesize.prop",
                                "replace",
                                "sample.fraction",
                                "mtry.prop",
                                "num.trees",
                                "holdout")
hyperparam_settings <- data.table::as.data.table(hyperparam_settings)
hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 & replace == FALSE), ]

q_seed <- data.frame(seed = seed, effect_seed = effect_seed,
                     alpha = rep(alpha, each = length(seed)))
expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


all_param_settings <- expand.grid.df(as.data.frame(hyperparam_settings), q_seed)
all_param_settings <- as.data.table(all_param_settings)

## *****************************************************************************
##                             Vita Pomona
## *****************************************************************************
##

run_vita_veer <- wrap_batchtools(reg_name = "vita_veer_mean_all_test",
                                 work_dir = working_dir,
                                 reg_dir = registry_dir_scen2,
                                 r_function = test_binary_pomona,
                                 vec_args = all_param_settings,
                                 more_args = list(
                                   data = data.frame(data_veer),
                                   betas = betas,
                                   n_beta = n_beta,
                                   subset = subset,
                                   fdr.adj = TRUE,
                                   independence = independence,
                                   null_case = FALSE,
                                   subsetsize = subsetsize
                                 ),
                                 name = "vita_veer",
                                 overwrite = TRUE,
                                 memory = "5g",
                                 n_cpus = 2L,
                                 walltime = "30",
                                 partition = partition,
                                 account = account,
                                 test_job = FALSE,
                                 wait_for_jobs = TRUE,
                                 packages = c(
                                   "devtools"
                                 ),
                                 config_file = config_file,
                                 interactive_session = interactive_session)

## Run this after that your jobs are completed
## *****************************************************************************
##                  Save Vita results
## *****************************************************************************
## Load registries
reg_vita_veer <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen2, "vita_veer_mean_all_test"),
  writeable = TRUE,
  conf.file = config_file)
njobs <- nrow(all_param_settings)
vita_veer_res <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:njobs,
    reg = reg_vita_veer
  ),
  reg = reg_vita_veer)


## resume filtered results
vita_veer_res_DT <- data.table::rbindlist(filter_out_empirical(vita_veer_res))

vita_veer_res_DT$Method <- "Vita"
vita_veer_res_DT$min.node.size_prop <- vita_veer_res_DT$nodesize.prop
saveRDS(object = vita_veer_res_DT,
        file = file.path(result_dir_scen2, "vita_veer_mean_res.RDS"))


## *****************************************************************************
##                             Boruta Pomona
## *****************************************************************************
##
run_boruta_veer <- wrap_batchtools(reg_name = "boruta_veer_mean_all_test",
                                    work_dir = working_dir,
                                    reg_dir = registry_dir_scen2,
                                    r_function = test_binary,
                                    vec_args = all_param_settings,
                                    more_args = list(
                                      data = data.frame(data_veer),
                                      beta = betas,
                                      n_beta = n_beta,
                                      null_case = null_case,
                                      pValue = 0.01,
                                      doTrace = TRUE,
                                      boruta_function = Pomona::var.sel.boruta
                                    ),
                                    name = "boruta_veer",
                                    overwrite = TRUE,
                                    memory = "10g",
                                    n_cpus = no.threads,
                                    walltime = "120",
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
## *****************************************************************************
##                  Save Boruta results
## *****************************************************************************
## Load registries
reg_boruta_veer <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen2, "boruta_veer_mean_all_test"),
  writeable = TRUE,
  conf.file = config_file)
njobs <- nrow(all_param_settings)
boruta_veer_res <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:njobs,
    reg = reg_boruta_veer
  ),
  reg = reg_boruta_veer)

## resume filtered results
boruta_veer_res_DT <- data.table::rbindlist(filter_out_empirical(boruta_veer_res))

boruta_veer_res_DT$Method <- "Boruta"
boruta_veer_res_DT$min.node.size_prop <- boruta_veer_res_DT$nodesize.prop
saveRDS(object = boruta_veer_res_DT,
        file = file.path(result_dir_scen2, "boruta_veer_mean_res.RDS"))

