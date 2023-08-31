#' This function runs simulation scenario 2 with Boruta variable selection 
#' method from the R package Pomona.
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
#' @param boruta_function Vita variable selection function to used.
#'
#' @return
#' @export
#'
#' @examples
test_binary <- function(data,
                        betas = c(-0.5, -1, -2, -3, 0.5, 1, 2, 3),
                        n_beta,
                        effect_seed,
                        subset = FALSE,
                        subsetsize = 100,
                        independence = TRUE,
                        null_case = TRUE,
                        pValue = 0.01, ## Just to harmonize output results
                        doTrace = 1,
                        maxRuns = 100,
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
                        boruta_function = Pomona::var.sel.boruta
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

  ## Importance testing with Boruta
  my_data <- train_data$data
  my_data$data <- NULL
  my_data$y <- NULL
  timeStart <- Sys.time()
  testing_resf <- boruta_function(x = my_data,
                                  y = train_data$data$y,
                                  pValue = pValue,
                                  maxRuns = maxRuns,
                                  ntree = num.trees,
                                  mtry.prop = mtry.prop,
                                  nodesize.prop = nodesize.prop,
                                  no.threads = no.threads,
                                  type = type,
                                  importance = importance,
                                  replace = replace,
                                  sample.fraction = sample.fraction,
                                  holdout = holdout,
                                  case.weights = NULL)
  timeEnd <- Sys.time()
  time_diff <- difftime(timeEnd, timeStart, units = 'mins')
  res <- data.frame(
    n = nrow(data),
    p = ncol(data),
    n_beta = n_beta,
    alpha = alpha,
    null_case = null_case,
    seed = seed,
    effect_seed = effect_seed,
    beta = train_data$beta,
    ## hyperparameters
    replace = replace,
    sample.fraction = sample.fraction,
    mtry.prop = mtry.prop,
    nodesize.prop = nodesize.prop,
    holdout = holdout,
    ntree = num.trees,
    varname = paste("x", abs(train_data$beta), sep = ""),
    pValue = pValue,
    decision = testing_resf$info$selected,
    pvalue_adj = NA,
    p_adj_decision = testing_resf$info$selected,
    runtime = as.numeric(time_diff),
    OOB = NA,
    FDR = sum((testing_resf$info$selected) & train_data$beta == 0) /
      sum(testing_resf$info$selected),
    FDR.adj = sum((testing_resf$info$selected) & train_data$beta == 0) /
      sum(testing_resf$info$selected),
    SENS = sum((testing_resf$info$selected) & train_data$beta != 0) /
      sum(train_data$beta != 0),
    SENS.adj = sum((testing_resf$info$selected) & train_data$beta != 0) /
      sum(train_data$beta != 0),
    SPEC = sum(!(testing_resf$info$selected) & train_data$beta == 0) /
      sum(train_data$beta == 0),
    SPEC.adj = sum(!(testing_resf$info$selected) & train_data$beta == 0) /
      sum(train_data$beta == 0)
  )
  return(res)
}
