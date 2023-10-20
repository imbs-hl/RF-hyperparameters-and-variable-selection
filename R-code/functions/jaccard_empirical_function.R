#' This function estimate the jaccard index from simulated scenarios
#'
#' @param reg_dir Registry name
#' @param all_param_seetings Parameter settings
#' @param .min.node.size Minimal node size is varying
#' @param .replace Draw strategy is varying
#' @param .sample.fraction Sample fraction is varying
#' @param .mtry.prop Number of split candidate is varying
#' @param .q Number of correlated variables 
jaccard_empirical_function <- function(reg_dir,
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
  all_param_seetings_subset <- all_param_seetings[(min.node.size_prop ==  .min.node.size) &
                                                    (replace ==  .replace) &
                                                    (sample.fraction ==  .sample.fraction) &
                                                    (mtry == .mtry.prop) &
                                                    (q == .q),
  ]
  ## Load registries
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
  my_jaccard <- function(a, b) {
    intersection = sum(a == b)
    union = length(a) + length(b) - intersection
    return (intersection/union)
  }
  ## With adjustment
  result_list_adj <- lapply(result_reg, function(i){
    return(i$p_adj_decision)
  })
  dist_adj <- unlist(lapply(combn(result_list_adj, 2, simplify = FALSE),
                            function(x) {
                              my_jaccard(a = x[[1]], b = x[[2]])
                            }))
  return(
    data.table(min.node.size = .min.node.size,
               replace = .replace,
               sample.fraction = .sample.fraction,
               mtry.prop = .mtry.prop,
               q = .q,
               Jaccard = round(dist_adj, 5))
  )
}

