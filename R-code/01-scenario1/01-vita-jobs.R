## Prepare and send the jobs for simulation study 1 and Vita selection
if(basename(getwd()) == "R-code"){
  source("init-global.R", chdir = TRUE)
}
setwd(file.path(main_dir, "01-scenario1"))
## method to the remote cluster.
source("init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((is.null(partition)) | is.null(account)) & (!interactive_session)){
  stop("Configure your batchtools account.")
}
## Build jobs for alternative case
## Parameter sets for Vita
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
nodesize.prop <- c(0.01, 0.05, 0.1, 0.15, 0.2)#seq(from = 1/n, to = 21/n, 3/n)
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
## Exclude settings with sample.fraction == 1 & replace == FALSE
hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 &
                                               replace == FALSE), ]


## Just 10 replicates if the system is in the testing mode, and 100 otherwise.
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
  1:5
} else {
  1:100
} 

k_seed <- data.frame(k = rep(k, each = length(seed)),
                     alpha = rep(alpha,
                                 each = length(rep(k, each = length(seed)))),
                     seed = seed)

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL),
                                       list(...))


all_param_settings <- expand.grid.df(as.data.frame(hyperparam_settings), k_seed)
all_param_settings <- as.data.table(all_param_settings)
## Send jobs
run_vita_cor <- wrap_batchtools(reg_name = "vita-cor",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = alternative_cor_vita,
                                vec_args = all_param_settings,
                                more_args = list(
                                  n = n,
                                  q = q,
                                  p = p,
                                  null_case = null_case,
                                  doTrace = doTrace
                                ),
                                name = "vita-cor",
                                overwrite = TRUE,
                                memory = "1g",
                                n_cpus = no.threads,
                                walltime = "20",
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
