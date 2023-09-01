#' This function runs the simulation scenario 1 with the vita testing
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
data_only_scen1 <- function(n = 100,
                            q = 10,
                            g = 1:6,
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
                                 q = q,
                                 g = g,
                                 p = p,
                                 beta_seed = seed,
                                 null_case = null_case)
  res_data <- list(seed = seed,
                   n = n,
                   q = q,
                   g = g,
                   p = p,
                   data = train_data)
  return(res_data)
}
