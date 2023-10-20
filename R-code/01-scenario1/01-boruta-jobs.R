## Prepare and send the jobs for simulation study 1 and Boruta selection
## method to the remote cluster.

source("init.R", chdir = TRUE)
source("../functions/alternative_cor_boruta.R")
## Parameter sets for Borutaf
n <- 100
q <- c(10, 50)
g <- 1:6
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

## Just 10 replicates if the system is in the testing mode, and 100 otherwise.
seed <- if(testing_mode){
  1:10
} else {
  1:100
} 

q_seed <- data.frame(q = rep(q, each = length(seed)),
                     pValue = rep(pValue,
                                  each = length(rep(q, each = length(seed)))),
                     seed = seed)

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL),
                                       list(...))


all_param_seetings <- expand.grid.df(as.data.frame(hyperparam_settings), q_seed)
all_param_seetings <- as.data.table(all_param_seetings)
## Send jobs
run_boruta <- wrap_batchtools(reg_name = "boruta-cor",
                               work_dir = working_dir,
                               reg_dir = registry_dir_scen1,
                               r_function = alternative_cor_boruta,
                               vec_args = all_param_seetings,
                               more_args = list(
                                 n = n,
                                 g = g,
                                 p = p,
                                 null_case = null_case,
                                 doTrace = 1
                               ),
                               name = "boruta-cor",
                               overwrite = TRUE,
                               memory = "10g",
                               n_cpus = no.threads,
                               walltime = "60",
                               partition = partition,
                               account = account,
                               test_job = FALSE,
                               wait_for_jobs = TRUE,
                               packages = c(
                                 "devtools",
                                 "Pomona",
                                 "data.table"
                               ),
                               config_file = config_file)
