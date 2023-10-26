#' This function estimate the FDR from simulated scenarios
#'
#' @param reg_dir Registry name
#' @param all_param_settings Parameter settings
#' @param .min.node.size Minimal node size is varying
#' @param .replace Draw strategy is varying
#' @param .sample.fraction Sample fraction is varying
#' @param .mtry.prop Number of split candidate is varying
#' @param .k Number of correlated variables 
fdr_empirical_function <- function(reg_dir,
                                   config_file = config_file,
                                   all_param_settings = all_param_settings,
                                   .min.node.size = 0.01,
                                   .replace = TRUE,
                                   .sample.fraction = 0.632,
                                   .mtry.prop = 0.01,
                                   .k = 10){
  source("init.R", chdir = TRUE)
  ## ************************************
  ## Load subset of parameter settings
  ## ************************************
  ##
  all_param_settings[ , id := 1:.N]
  # all_param_settings[ , mtry := round(.mtry.prop, 3)]
  all_param_settings_subset <- all_param_settings[(min.node.size_prop ==  .min.node.size) &
                                                    (replace ==  .replace) &
                                                    (sample.fraction ==  .sample.fraction) &
                                                    (mtry == .mtry.prop) &
                                                    (k == .k),
  ]
  ## Load registries
  # print(head(all_param_settings_subset, n = 5))
  my_reg <- batchtools::loadRegistry(
    file.dir = reg_dir, writeable = FALSE,
    conf.file = config_file)
  result_reg <- batchtools::reduceResultsList(
    ids = batchtools::findDone(
      ids = all_param_settings_subset$id,
      reg = my_reg
    ),
    reg = my_reg)
  
  ## *******************************
  ##  Jaccard function
  ## *******************************
  my_fdr <- function(truth, selected) {
    fdr_res <- sum(selected & !truth) / sum(selected)
    return (fdr_res)
  }
  ## With adjustment
  fdr_adj <- unlist(lapply(result_reg, function(i){
    return(my_fdr(truth = (i$varindex %in% 1:3),
                  selected = (i$decision == "Confirmed")))
  }))
  return(
    data.table(min.node.size = .min.node.size,
               replace = .replace,
               sample.fraction = .sample.fraction,
               mtry.prop = .mtry.prop,
               k = .k,
               FDR = round(fdr_adj, 5))
  )
}

if(FALSE){
  tmp <- fdr_empirical_function(reg_dir = file.path(registry_dir_scen1,
                                                    "boruta-cor"),
                                all_param_settings = all_param_settings[k == 10, ],
                                config_file = config_file,
                                .min.node.size = 0.01,
                                .replace = TRUE,
                                .sample.fraction = 0.632,
                                .mtry.prop = 0.014,
                                .k = 10)
}
