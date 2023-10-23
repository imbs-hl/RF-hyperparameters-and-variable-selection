## Ensure partition and account are set up.
if((partition == "xxxx") | account == "xxxx"){
  stop("Configure your batchtools account.")
}
source("init.R", chdir = TRUE)

## Build jobs for alternative case
## Parameter sets
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
min.node.size_prop <- c(0.01, 0.05, 0.1, 0.15, 0.2)
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

## Just 10 replicates if the system is in the testing mode, and 100 otherwise.
seed <- ifelse(testing_mode, 1:10, 1:100)
q_seed <- data.frame(q = rep(q, each = length(seed)),
                     alpha = rep(alpha, each = length(rep(q, each = length(seed)))),
                     seed = seed)

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


all_param_seetings <- expand.grid.df(as.data.frame(hyperparam_settings), q_seed)
all_param_seetings <- as.data.table(all_param_seetings)

all_param_seeting_unique <- unique(all_param_seetings,
                                   by = c("min.node.size_prop",
                                          "no.threads",
                                          "replace",
                                          "sample.fraction",
                                          "mtry",
                                          "num.trees",
                                          "holdout",
                                          "q"))
names(all_param_seeting_unique) <- c(".min.node.size",
                                     "no.threads",
                                     ".replace",
                                     ".sample.fraction",
                                     ".mtry.prop",
                                     "num.trees",
                                     "holdout",
                                     ".q", "alpha", "seed")

## ****** Remove unecessary parameters
all_param_seeting_unique$no.threads <- NULL
all_param_seeting_unique$num.trees <- NULL
all_param_seeting_unique$seed <- NULL
all_param_seeting_unique$alpha <- NULL
all_param_seeting_unique$holdout <- NULL


## Send jobs
run_vita <- wrap_batchtools(reg_name = "spec_empirical_vita",
                            work_dir = working_dir,
                            reg_dir = registry_dir_scen1,
                            r_function = spec_empirical_function,
                            vec_args = all_param_seeting_unique,
                            more_args = list(
                              all_param_seetings = all_param_seetings,
                              reg_dir = file.path(registry_dir_scen1, "vita-cor")
                            ),
                            name = "spec_vita",
                            overwrite = FALSE,
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
                            config_file = config_file)

## =======================================
## Resume FDR's result for vita
## =======================================
reg_vita_spec <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1,
                       "spec_empirical_vita"), writeable = TRUE)
vita_spec_reg <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_seeting_unique),
    reg = reg_vita_spec
  ),
  reg = reg_vita_spec)


## resume filtered results
vita_spec_DT <- data.table::rbindlist(vita_spec_reg)

vita_spec_DT$Method <- "Vita"

saveRDS(object = vita_spec_DT,
        file = file.path(result_dir_scen1, "vita_cor_spec.RDS"))


## Send Boruta jobs for q = 10
run_boruta10 <- wrap_batchtools(reg_name = "spec_boruta10",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = spec_empirical_function,
                                vec_args = all_param_seeting_unique[.q == 10, ],
                                more_args = list(
                                  all_param_seetings = all_param_seetings[q == 10, ],
                                  reg_dir = file.path(registry_dir_scen1, "boruta-cor10")
                                ),
                                name = "spec_boruta10",
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
                                config_file = config_file)


## Send Boruta jobs for q = 50
run_boruta50 <- wrap_batchtools(reg_name = "spec_boruta50",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = spec_empirical_function,
                                vec_args = all_param_seeting_unique[.q == 50, ],
                                more_args = list(
                                  all_param_seetings = all_param_seetings[q == 50, ],
                                  reg_dir = file.path(registry_dir_scen1, "boruta-cor50")
                                ),
                                name = "spec_boruta50",
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
                                config_file = config_file)

## ----------------------------------------------
## Resume jaccard's result for vita for q = 10
## ----------------------------------------------
##
reg_boruta_spec10 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "spec_boruta10"), writeable = TRUE)
boruta_spec_reg10 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_seeting_unique),
    reg = reg_boruta_spec10
  ),
  reg = reg_boruta_spec10)


## resume filtered results
boruta_spec_DT10 <- data.table::rbindlist(boruta_spec_reg10)


## ----------------------------------------------
## Resume jaccard's result for vita for q = 50
## ----------------------------------------------
##
reg_boruta_spec50 <- batchtools::loadRegistry(
  file.dir = file.path(registry_dir_scen1, "spec_boruta50"), writeable = TRUE)
boruta_spec_reg50 <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:nrow(all_param_seeting_unique),
    reg = reg_boruta_spec50
  ),
  reg = reg_boruta_spec50)

## resume filtered results
boruta_spec_DT50 <- data.table::rbindlist(boruta_spec_reg50)

boruta_spec_DT <- data.table::rbindlist(list(boruta_spec_DT10,
                                            boruta_spec_DT50))


boruta_spec_DT$Method <- "Boruta"

saveRDS(object = boruta_spec_DT,
        file = file.path(result_dir_scen1, "boruta_cor_spec.RDS"))
