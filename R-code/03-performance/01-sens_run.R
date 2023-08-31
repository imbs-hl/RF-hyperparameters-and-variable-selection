source("init.R", chdir = TRUE)
## Parameter sets for Borutaf
n <- 100
q <- c(10, 50)
g <- 1:6
p <- 5000
null_case <- FALSE
doTrace <- 1
alpha <- 0.05
no.threads <- 4L

## Random forests hyperparameter settings
replace <- c(TRUE, FALSE)
sample.fraction <- c(0.200, 0.400, 0.632, 0.800, 1.000)
mtry.prop <- c(0.5, 0.33, 0.25, 0.1, sqrt(p) / p)
min.node.size_prop <- c(0.01, 0.05, 0.1, 0.2, 5/n)
num.trees <- 1e4
holdout = c(FALSE)

hyperparam_settings <- expand.grid(min.node.size_prop,
                                   no.threads,
                                   replace,
                                   sample.fraction,
                                   mtry.prop,
                                   num.trees,
                                   holdout)
names(hyperparam_settings) <- c("min.node.size_prop",
                                "no.threads",
                                "replace",
                                "sample.fraction",
                                "mtry.prop",
                                "num.trees",
                                "holdout")
hyperparam_settings <- data.table::as.data.table(hyperparam_settings)
## Exclude settings with sample.fraction == 1 & replace == FALSE
hyperparam_settings <- hyperparam_settings[!(sample.fraction == 1 & replace == FALSE), ]


## Just 10 replicates if the system is in the testing mode, and 100 otherwise.
seed <- ifelse(testing_mode, 1:10, 1:100)
q_seed <- data.frame(seed = seed, q = rep(q, each = length(seed)), 
                     alpha = rep(alpha, each = length(rep(q, each = length(seed)))))

expand.grid.df <- function(...) Reduce(function(...) merge(..., by=NULL), list(...))


all_param_seetings <- expand.grid.df(as.data.frame(hyperparam_settings), q_seed)
all_param_seetings <- as.data.table(all_param_seetings)

all_param_seeting_unique <- unique(all_param_seetings,
                                   by = c("min.node.size_prop",
                                          "no.threads",
                                          "replace",
                                          "sample.fraction",
                                          "mtry.prop",
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
                                     "seed", ".q", "alpha")

## ****** Remove unecessary parameters
all_param_seeting_unique$no.threads <- NULL
all_param_seeting_unique$num.trees <- NULL
all_param_seeting_unique$seed <- NULL
all_param_seeting_unique$alpha <- NULL
all_param_seeting_unique$holdout <- NULL


## Send jobs
run_vita <- wrap_batchtools(reg_name = "sens_vita",
                            work_dir = working_dir,
                            reg_dir = registry_dir_scen1,
                            r_function = sens_function,
                            vec_args = all_param_seeting_unique,
                            more_args = list(
                              all_param_seetings = all_param_seetings,
                              reg_dir = file.path("/imbs/home/cesaire/projects/urf_mtry_paper/tuning/R-code/registry/scenario1", "vita")
                            ),
                            name = "sens_vita",
                            overwrite = FALSE,
                            memory = "5g",
                            n_cpus = 1,
                            walltime = "5",
                            partition = "prio",
                            account = "dzhkomics",
                            test_job = FALSE,
                            wait_for_jobs = TRUE,
                            packages = c(
                              "devtools",
                              "data.table"
                            ),
                            config_file = "/imbs/home/cesaire/projects/URF_Shi_and_Harvath/Random-Forest-Clustering/99_batchtools/batchtools.conf.R")


## Resume jaccard's result for vita
reg_vita_sens <- batchtools::loadRegistry(
  file.dir = file.path("/imbs/home/cesaire/projects/urf_mtry_paper/tuning/R-code/registry/scenario1", "sens_vita"), writeable = TRUE)
vita_sens_reg <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:360,
    reg = reg_vita_sens
  ),
  reg = reg_vita_sens)


## resume filtered results
vita_sens_DT <- data.table::rbindlist(vita_sens_reg)

vita_sens_DT$Method <- "Vita"

saveRDS(object = vita_sens_DT,
        file = file.path(result_dir_scen1, "vita_sens.RDS"))


## Send jobs
run_vita_new <- wrap_batchtools(reg_name = "sens_vita_new",
                                work_dir = working_dir,
                                reg_dir = registry_dir_scen1,
                                r_function = sens_function,
                                vec_args = all_param_seeting_unique,
                                more_args = list(
                                  all_param_seetings = all_param_seetings,
                                  reg_dir = file.path("/imbs/home/cesaire/projects/urf_mtry_paper/tuning/R-code/registry/scenario1", "vita_new")
                                ),
                                name = "sens_vita_new",
                                overwrite = FALSE,
                                memory = "5g",
                                n_cpus = 1,
                                walltime = "30",
                                partition = "prio",
                                account = "dzhkomics",
                                test_job = FALSE,
                                wait_for_jobs = TRUE,
                                packages = c(
                                  "devtools",
                                  "data.table"
                                ),
                                config_file = "/imbs/home/cesaire/projects/URF_Shi_and_Harvath/Random-Forest-Clustering/99_batchtools/batchtools.conf.R")

## Resume jaccard's result for vita
reg_vita_new_sens <- batchtools::loadRegistry(
  file.dir = file.path("/imbs/home/cesaire/projects/urf_mtry_paper/tuning/R-code/registry/scenario1", "sens_vita_new"), writeable = TRUE)
vita_new_sens_reg <- batchtools::reduceResultsList(
  ids = batchtools::findDone(
    ids = 1:360,
    reg = reg_vita_new_sens
  ),
  reg = reg_vita_new_sens)


## resume filtered results
vita_new_sens_DT <- data.table::rbindlist(vita_new_sens_reg)

vita_new_sens_DT$Method <- "Vita.Neue"

saveRDS(object = vita_new_sens_DT,
        file = file.path(result_dir_scen1, "vita_new_sens.RDS"))
