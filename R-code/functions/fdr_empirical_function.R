#' This function estimate the FDR from simulated scenarios
#'
#' @param reg_dir Registry name
#' @param all_param_seetings Parameter settings
#' @param .min.node.size Minimal node size is varying
#' @param .replace Draw strategy is varying
#' @param .sample.fraction Sample fraction is varying
#' @param .mtry.prop Number of split candidate is varying
#' @param .q Number of correlated variables 
fdr_empirical_function <- function(reg_dir,
                                   all_param_seetings = all_param_seetings,
                                   .min.node.size = 0.01,
                                   .replace = TRUE,
                                   .sample.fraction = 0.632,
                                   .mtry.prop = 0.01,
                                   .q = 10){
  ## ************************************
  ## Load subset of parameter settings
  ## ************************************
  ##
  all_param_seetings[ , id := 1:.N]
  # all_param_seetings[ , mtry := round(.mtry.prop, 3)]
  all_param_seetings_subset <- all_param_seetings[(min.node.size_prop ==  .min.node.size) &
                                                    (replace ==  .replace) &
                                                    (sample.fraction ==  .sample.fraction) &
                                                    (mtry == .mtry.prop) &
                                                    (q == .q),
  ]
  ## Load registries
  # print(head(all_param_seetings_subset, n = 5))
  my_reg <- batchtools::loadRegistry(
    file.dir = reg_dir, writeable = FALSE)
  result_reg <- batchtools::reduceResultsList(
    ids = batchtools::findDone(
      ids = all_param_seetings_subset$id,
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
               q = .q,
               FDR = round(fdr_adj, 5))
  )
}

if(FALSE){
  tmp <- fdr_empirical_function(reg_dir = file.path("/imbs/home/cesaire/projects/urf_mtry_paper/tuning/R-code/registry/scenario1", "boruta-cor50"),
                                all_param_seetings = all_param_seetings[q == 50, ],
                                .min.node.size = 0.01,
                                .replace = TRUE,
                                .sample.fraction = 0.2,
                                .mtry.prop = 0.5,
                                .q = 50)
}
