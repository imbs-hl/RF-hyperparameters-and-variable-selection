#' This function estimate the sensitivity from simulated scenarios
#'
#' @param reg_dir Registry name
#' @param all_param_settings Parameter settings
#' @param .min.node.size Minimal node size is varying
#' @param .replace Draw strategy is varying
#' @param .sample.fraction Sample fraction is varying
#' @param .mtry.prop Number of split candidate is varying
#' @param .k Number of correlated variables 
sens_empirical_function <- function(reg_dir,
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
  all_param_settings_subset <- all_param_settings[(min.node.size_prop ==  .min.node.size) &
                                                    (replace ==  .replace) &
                                                    (sample.fraction ==  .sample.fraction) &
                                                    (mtry == .mtry.prop) &
                                                    (k == .k),
  ]
  ## Load registries
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
  ##  sensitivity function
  ## *******************************
  my_sens <- function(truth, selected) {
    sens_res <- sum(truth & selected) / sum(truth)
    return (sens_res)
  }
  ## With adjustment
  sens_adj <- unlist(lapply(result_reg, function(i){
    return(my_sens(truth = (i$varindex %in% 1:3),
                  selected = (i$decision == "Confirmed")))
  }))
  return(
    data.table(min.node.size = .min.node.size,
               replace = .replace,
               sample.fraction = .sample.fraction,
               mtry.prop = .mtry.prop,
               k = .k,
               SENS = round(sens_adj, 5))
  )
}

