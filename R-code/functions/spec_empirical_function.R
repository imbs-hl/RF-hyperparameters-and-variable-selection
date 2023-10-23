#' This function estimate the specificity from simulated scenarios
#'
#' @param reg_dir Registry name
#' @param all_param_settings Parameter settings
#' @param .min.node.size Minimal node size is varying
#' @param .replace Draw strategy is varying
#' @param .sample.fraction Sample fraction is varying
#' @param .mtry.prop Number of split candidate is varying
#' @param .q Number of correlated variables 
spec_empirical_function <- function(reg_dir,
                                    config_file = config_file,
                                    all_param_settings = all_param_settings,
                                    .min.node.size = 0.01,
                                    .replace = TRUE,
                                    .sample.fraction = 0.632,
                                    .mtry.prop = 0.01,
                                    .q = 10){
  ## ************************************
  ## Load subset of parameter settings
  ## ************************************
  ##
  all_param_settings[ , id := 1:.N]
  all_param_settings_subset <- all_param_settings[(min.node.size_prop ==  .min.node.size) &
                                                    (replace ==  .replace) &
                                                    (sample.fraction ==  .sample.fraction) &
                                                    (mtry == .mtry.prop) &
                                                    (q == .q),
  ]
  ## Load registries
  my_reg <- batchtools::loadRegistry(
    file.dir = reg_dir, writeable = FALSE,
    config.file = config_file)
  result_reg <- batchtools::reduceResultsList(
    ids = batchtools::findDone(
      ids = all_param_settings_subset$id,
      reg = my_reg
    ),
    reg = my_reg)

  ## *******************************
  ##  sensitivity function
  ## *******************************
  my_spec <- function(truth, selected) {
    spec_res <- sum(!truth & !selected) / sum(!truth)
    return (spec_res)
  }
  ## With adjustment
  my_spec <- unlist(lapply(result_reg, function(i){
    return(my_spec(truth = (i$varindex %in% 1:3),
                   selected = (i$decision == "Confirmed")))
  }))
  return(
    data.table(min.node.size = .min.node.size,
               replace = .replace,
               sample.fraction = .sample.fraction,
               mtry.prop = .mtry.prop,
               q = .q,
               SPEC = round(my_spec, 5))
  )
}

