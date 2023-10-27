## Prepare and send the jobs for simulation study 1 and Boruta selection
setwd(file.path(main_dir, "01-scenario1"))
## method to the remote cluster.
source("init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((is.null(partition)) | is.null(account)) & (!interactive_session)){
  stop("Configure your batchtools account.")
}

## Parameter sets for Boruta
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
## nodesize.prop correspnds to min.node.size.prop in manuscript but for consis-
## tent reason with Pomona's functions we call it nodesize.prop
nodesize.prop <- c(0.01, 0.05, 0.1, 0.15, 0.2)
num.trees <- 1e4

importance = "impurity_corrected"
holdout = FALSE

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
hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 &
                                               replace == FALSE), ]

## Just few replicates if the system is in the testing mode, and 100 otherwise.
seed <- if(testing_mode){
  ## Variation of min.node.size
  nodesize.prop.var <- data.frame(nodesize.prop = nodesize.prop,
                                  no.threads = no.threads,
                                  replace = TRUE,
                                  sample.fraction = 0.632,
                                  mtry = 0.014,
                                  num.trees = num.trees,
                                  holdout = holdout)
  ## Variation of replace
  replace.var <- data.frame(nodesize.prop = 0.01,
                                  no.threads = no.threads,
                                  replace = replace,
                                  sample.fraction = 0.632,
                                  mtry = 0.014,
                                  num.trees = num.trees,
                                  holdout = holdout)
  ## Variation of sample.fraction
  sample.fraction.var <- data.frame(nodesize.prop = 0.01,
                            no.threads = no.threads,
                            replace = TRUE,
                            sample.fraction = sample.fraction,
                            mtry = 0.014,
                            num.trees = num.trees,
                            holdout = holdout)
  ## Variation of mtry
  mtry.var <- data.frame(nodesize.prop = 0.01,
                                    no.threads = no.threads,
                                    replace = TRUE,
                                    sample.fraction = 0.632,
                                    mtry = mtry,
                                    num.trees = num.trees,
                                    holdout = holdout)
  hyperparam_settings <- data.table::rbindlist(list(
    nodesize.prop.var,
    replace.var,
    sample.fraction.var,
    mtry.var
  ))
  1:4
} else {
  1:100
} 

k_seed <- data.frame(k = rep(k, each = length(seed)),
                     pValue = rep(pValue,
                                  each = length(rep(k, each = length(seed)))),
                     seed = seed)

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL),
                                       list(...))


all_param_settings <- expand.grid.df(as.data.frame(hyperparam_settings), k_seed)
all_param_settings <- as.data.table(all_param_settings)
## Send jobs
run_boruta10 <- wrap_batchtools(reg_name = "boruta-cor10",
                              work_dir = working_dir,
                              reg_dir = registry_dir_scen1,
                              r_function = alternative_cor_boruta,
                              vec_args = all_param_settings[k == 10, ],
                              more_args = list(
                                n = n,
                                q = q,
                                p = p,
                                null_case = null_case,
                                doTrace = doTrace
                              ),
                              name = "boruta-cor10",
                              overwrite = TRUE,
                              memory = "1g",
                              n_cpus = no.threads,
                              walltime = "60",
                              partition = partition, ## Set partition in init-global
                              account = account, ## Set account in init-global
                              test_job = FALSE,
                              wait_for_jobs = FALSE,
                              packages = c(
                                "devtools",
                                "Pomona",
                                "data.table"
                              ),
                              config_file = config_file,
                              interactive_session = interactive_session)


run_boruta50 <- wrap_batchtools(reg_name = "boruta-cor50",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = alternative_cor_boruta,
                                vec_args = all_param_settings[k == 50, ],
                                more_args = list(
                                  n = n,
                                  q = q,
                                  p = p,
                                  null_case = null_case,
                                  doTrace = doTrace
                                ),
                                name = "boruta-cor50",
                                overwrite = TRUE,
                                memory = "1g",
                                n_cpus = no.threads,
                                walltime = "60",
                                partition = partition, ## Set partition in init-global
                                account = account, ## Set account in init-global
                                test_job = FALSE,
                                wait_for_jobs = FALSE,
                                packages = c(
                                  "devtools",
                                  "Pomona",
                                  "data.table"
                                ),
                                config_file = config_file,
                                interactive_session = interactive_session)
## Re-set the current directory.
setwd(main_dir)