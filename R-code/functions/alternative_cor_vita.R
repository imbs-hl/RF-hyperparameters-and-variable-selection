#' This function runs the simulation scenario 1 with the vita testing
#' procedure
#'
#' @param n Number of individuals
#' @param k Basis variable
#' @param q Index of correlated variables
#' @param p Number of predictor variables
#' @param null_case Logical variable indicating if a null case is simulated
#' @param holdout Should the holdout variable importance be used?
#' @param doTrace Trace the execution of not
#' @param num.trees Number of decision trees to be used
#' @param seed Seed to be used
#' @param mtry.prop Proportion of variables to be selected as split candidates, called mtry in manuscript
#' @param nodesize.prop Proportion for minimal node size, called min.node.size in manuscript.
#' @param no.threads Number of threads
#' @param type Either "classification" or "regression"
#' @param replace To be pass to Pomona
#' @param sample.fraction Sample fraction
alternative_cor_vita <- function(n = 100,
                                 k = 10,
                                 q = 1:6,
                                 p = 5000,
                                 null_case = FALSE,
                                 holdout = FALSE,
                                 doTrace = 1,
                                 num.trees = 1e4,
                                 seed = 123,
                                 alpha = 0.05,
                                 mtry.prop = 0.33,
                                 nodesize.prop = 0.1,
                                 no.threads = 1,
                                 type = "classification",
                                 replace = TRUE,
                                 sample.fraction = ifelse(replace, 1, 0.632)
){
  mtry_fct <- function(p){
    floor(p * mtry.prop)
  }
  source("init.R", chdir = TRUE)
  ## Simulate data
  set.seed(seed = seed)
  train_data <- simulate_cor_bin(n = n,
                                 k = k,
                                 q = q,
                                 p = p,
                                 beta_seed = seed,
                                 null_case = null_case)
  y <- train_data[ , (p+1)]
  train_data <- train_data[ , -(p+1)]
  ## Importance testing
  timeStart <- Sys.time()
  ## Null model
  set.seed(seed = seed)
  
  
  # testing_resf <- if(!missing(alpha)){
  testing_resf <- Pomona::var.sel.vita(x = as.matrix(train_data),
                                       y = y,
                                       p.t = alpha,
                                       fdr.adj = TRUE,
                                       ntree = num.trees,
                                       mtry.prop = mtry.prop,
                                       nodesize.prop = nodesize.prop,
                                       no.threads = no.threads,
                                       type = type,
                                       importance = "impurity_corrected",
                                       replace = replace,
                                       sample.fraction = sample.fraction,
                                       holdout = holdout,
                                       case.weights = NULL)
  
  finalDecision <- rep("Rejected", length(testing_resf$info$selected))
  finalDecision[testing_resf$info$selected == 1] <- "Confirmed"
  p.adj.Decision <- rep("Rejected", length(testing_resf$info$selected))
  p.adj.Decision[testing_resf$info$selected == 1] <- "Confirmed"
  
  timeEnd <- Sys.time()
  time_diff <- difftime(timeEnd, timeStart, units = 'mins')
  ## Build result data.frame
  res_df <- data.frame(
    n = n,
    k = k,
    n_q = length(q),
    p = p,
    null_case = null_case,
    num.trees = num.trees,
    mtry.prop = mtry.prop,
    nodesize.prop = nodesize.prop,
    replace = replace,
    sample.fraction = sample.fraction,
    holdout = holdout,
    alpha = alpha,
    seed = seed,
    varindex = c(rep(q, k), (k * length(q) + 1):p),
    varname = paste("x", c(rep(q, k), (k * length(q) + 1):p), sep = ""),
    decision = finalDecision,
    p_adj_decision = as.character(p.adj.Decision),
    runtime = as.numeric(time_diff)
  )
  return(res_df)
}
