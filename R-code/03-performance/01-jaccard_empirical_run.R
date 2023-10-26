source("init.R", chdir = TRUE)
source("../01-scenario1/init.R", chdir = TRUE)
## Ensure partition and account are set up.
if(((partition == "xxxx") | account == "xxxx") & (!interactive_session)){
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

## Just 1 replicates if the system is in the testing mode, and 100 otherwise.
seed <- if(testing_mode){
  ## Variation of min.node.size
  nodesize.prop.var <- data.frame(min.node.size_prop = min.node.size_prop,
                                  no.threads = no.threads,
                                  replace = TRUE,
                                  sample.fraction = 0.632,
                                  mtry = 0.014,
                                  num.trees = num.trees,
                                  holdout = holdout)
  ## Variation of replace
  replace.var <- data.frame(min.node.size_prop = 0.01,
                            no.threads = no.threads,
                            replace = replace,
                            sample.fraction = 0.632,
                            mtry = 0.014,
                            num.trees = num.trees,
                            holdout = holdout)
  ## Variation of sample.fraction
  sample.fraction.var <- data.frame(min.node.size_prop = 0.01,
                                    no.threads = no.threads,
                                    replace = TRUE,
                                    sample.fraction = sample.fraction,
                                    mtry = 0.014,
                                    num.trees = num.trees,
                                    holdout = holdout)
  ## Variation of mtry
  mtry.var <- data.frame(min.node.size_prop = 0.01,
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
  1:3
} else {
  1:100
} 

k_seed <- data.frame(k = rep(k, each = length(seed)),
                     alpha = rep(alpha, each = length(rep(k, each = length(seed)))),
                     seed = seed)

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
run_vita <- wrap_batchtools(reg_name = "jaccard_empirical_vita",
                            work_dir = working_dir,
                            reg_dir = registry_dir_scen1,
                            r_function = jaccard_empirical_function,
                            vec_args = all_param_setting_unique,
                            more_args = list(
                              config_file = config_file,
                              all_param_settings = all_param_settings,
                              reg_dir = file.path(registry_dir_scen1,
                                                  "vita-cor")
                            ),
                            name = "jaccard_vita",
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
reg_vita_jaccard <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1,
                       "jaccard_empirical_vita"), writeable = TRUE,
  conf.file = config_file)
vita_jaccard_reg <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique),
    reg = reg_vita_jaccard
  ),
  reg = reg_vita_jaccard)


## resume filtered results
vita_jaccard_DT <- data.table::rbindlist(vita_jaccard_reg)

vita_jaccard_DT$Method <- "Vita"

saveRDS(object = vita_jaccard_DT,
        file = file.path(result_dir_scen1, "vita_cor_jaccard.RDS"))


## Send Boruta jobs for k = 10
run_boruta10 <- wrap_batchtools(reg_name = "jaccard_boruta10",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = jaccard_empirical_function,
                                vec_args = all_param_setting_unique[.k == 10, ],
                                more_args = list(
                                  config_file = config_file,
                                  all_param_settings = all_param_settings[k == 10, ],
                                  reg_dir = file.path(registry_dir_scen1,
                                                      "boruta-cor10")
                                ),
                                name = "jaccard_boruta10",
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
run_boruta50 <- wrap_batchtools(reg_name = "jaccard_boruta50",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = jaccard_empirical_function,
                                vec_args = all_param_setting_unique[.k == 50, ],
                                more_args = list(
                                  config_file = config_file,
                                  all_param_settings = all_param_settings[k == 50, ],
                                  reg_dir = file.path(registry_dir_scen1,
                                                      "boruta-cor50")
                                ),
                                name = "jaccard_boruta50",
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
reg_boruta_jaccard10 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "jaccard_boruta10"),
  writeable = TRUE,
  conf.file = config_file)
boruta_jaccard_reg10 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique),
    reg = reg_boruta_jaccard10
  ),
  reg = reg_boruta_jaccard10)


## resume filtered results
boruta_jaccard_DT10 <- data.table::rbindlist(boruta_jaccard_reg10)


## Run this after that your jobs are completed
## ----------------------------------------------
## Resume jaccard's result for vita for k = 50
## ----------------------------------------------
##
reg_boruta_jaccard50 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1,
                       "jaccard_boruta50"), writeable = TRUE,
  conf.file = config_file)
boruta_jaccard_reg50 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_setting_unique),
    reg = reg_boruta_jaccard50
  ),
  reg = reg_boruta_jaccard50)


## resume filtered results
boruta_jaccard_DT50 <- data.table::rbindlist(boruta_jaccard_reg50)

boruta_jaccard_DT <- data.table::rbindlist(list(boruta_jaccard_DT10,
                                                boruta_jaccard_DT50))


boruta_jaccard_DT$Method <- "Boruta"

saveRDS(object = boruta_jaccard_DT,
        file = file.path(result_dir_scen1, "boruta_cor_jaccard.RDS"))
