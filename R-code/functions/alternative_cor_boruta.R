#' This function runs the simulation scenario 1 with the Boruta selection
#' procedure
#'
#' @param n Number of individuals
#' @param q Basis variable
#' @param g Index of correlated variables
#' @param p Number of predictor variables
#' @param null_case Logical variable indicating if a null case is simulated
#' @param holdout Should the holdout variable importance be used?
#' @param doTrace Trace the execution of not
#' @param num.trees Number of decision trees to be used
#' @param seed Seed to be used
#' @param pValue p value threshold for each Boruta testing iteration
#' @param mtry.prop Proportion of variables to be selected as split candidates
#' @param nodesize.prop Proportion for minimal node size
#' @param no.threads Number of threads
#' @param type Either "classification" or "regression"
#' @param replace To be pass to Pomona
#' @param sample.fraction Sample fraction
#'
#' @return
#' @export
#'
#' @examples
alternative_cor_boruta <- function(n = 100,
                                   q = 10,
                                   g = 1:6,
                                   p = 5000,
                                   null_case = FALSE,
                                   holdout = FALSE,
                                   doTrace = 1,
                                   num.trees = 1e4,
                                   seed = 123,
                                   pValue = 0.01,
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
                                 q = q,
                                 g = g,
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
  testing_resf <- Pomona::var.sel.boruta(x = as.matrix(train_data),
                                         y = y,
                                         pValue = pValue,
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
    q = q,
    n_g = length(g),
    p = p,
    null_case = null_case,
    num.trees = num.trees,
    mtry.prop = mtry.prop,
    nodesize.prop = nodesize.prop,
    replace = replace,
    sample.fraction = sample.fraction,
    holdout = holdout,
    alpha = pValue,
    seed = seed,
    varindex = c(rep(g, q), (q * length(g) + 1):p),
    varname = paste("x", c(rep(g, q), (q * length(g) + 1):p), sep = ""),
    decision = finalDecision,
    p_adj_decision = as.character(p.adj.Decision),
    runtime = as.numeric(time_diff)
  )
  return(res_df)
}
