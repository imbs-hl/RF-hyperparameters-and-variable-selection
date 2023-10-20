#' This function runs simulation scenario 2 with Vita variable selection method
#' from the R package Pomona
#'
#' @param data Empirical data with predictor variables only.
#' @param betas Effect sizes.
#' @param n_beta Number of effect variables.
#' @param effect_seed Seed for drawing effect sizes.
#' @param subset Should only a subset of predictor be used?
#' @param subsetsize How much predictor in the subset?
#' @param independence Do noise variables being independent?
#' @param null_case If TRUE a null with no relationship between predictors and 
#'                  the response variables is simulated.
#' @param p.t See the Pomona package.
#' @param fdr.adj Indicate wheter p values should be adjusted.
#' @param num.trees Number of trees.
#' @param seed Seed for random number generator.
#' @param alpha Significance threshold.
#' @param mtry.prop Proportion of split candidates.
#' @param nodesize.prop Proportion of minimal node size.
#' @param no.threads Number of threads for tree growing.
#' @param type Either "classification" or "regression".
#' @param importance Importance measure to be used.
#' @param replace Draw strategy.
#' @param sample.fraction Sample fraction.
#' @param holdout If TRUE, the holdout importance is used.
#' @param vita_function Vita variable selection function to used.
data_only_scen2 <- function(data,
                            betas = c(-0.5, -1, -2, -3, 0.5, 1, 2, 3),
                            n_beta,
                            effect_seed,
                            subset = FALSE,
                            subsetsize = 100,
                            independence = TRUE,
                            null_case = TRUE,
                            p.t = 0.05,
                            fdr.adj = TRUE,
                            num.trees,
                            seed,
                            alpha,
                            mtry.prop = 0.1,
                            nodesize.prop = 0.1,
                            no.threads = 5,
                            type = "classification",
                            importance = "impurity_corrected",
                            replace = TRUE,
                            sample.fraction =  ifelse(replace, 1, 0.632),
                            holdout = FALSE,
                            vita_function = Pomona::var.sel.vita
){
  source("init.R", chdir = TRUE)
  train_data <- simulate_alt_bin(seed = seed,
                                 x = data,
                                 effectset = betas,
                                 nsignal = n_beta,
                                 subset = subset,
                                 subsetsize = subsetsize,
                                 independence = independence)
  if(null_case){
    set.seed(seed = seed)
    train_data$data$y <- sample(train_data$data$y)
  }
  res <- list(betas = betas,
              n_beta = n_beta,
              effect_seed = effect_seed,
              subset = subset,
              subsetsize = subsetsize,
              independence = independence,
              null_case = null_case,
              data = train_data)
  return(res)
}