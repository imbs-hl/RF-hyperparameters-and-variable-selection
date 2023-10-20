#' This function simulate an alternative scenario with relationship between 
#' predictor variables and the simulated response variable.
#'
#' @param seed Initial value of random number generator.
#' @param x Design matrix with experimental data.
#' @param nsignal Number of signal.
#' @param subset Should only a subset of predictor be used?
#' @param subsetsize How much predictor in the subset?
#' @param independence Do noise variables being independent?
#' @param prop.noise.obs Proportion of noise observation.
#' @param effectset Set of effect sizes.
simulate_alt_bin <- function(seed,
                             x,
                             nsignal = 25,
                             subset = FALSE,
                             subsetsize = 100,
                             independence,
                             prop.noise.obs = 0.2,
                             effectset = c(-0.5, -1, -2, -3, 0.5, 1, 2, 3)){
  # reduce design matrix if using only a subset
  if(subset){
    set.seed(seed)
    predindex <- sample(1:ncol(x), size = subsetsize)
    x <- x[,predindex]
  }

  # scale the design matrix to make effects comparable
  x <- scale(x)
  # determine beta coefficients
  beta <- rep(effectset, length = nsignal)[order(abs(rep(effectset,
                                                         length = nsignal)),
                                                 decreasing = T)]

  # randomly draw nsignal predictors which are defined as signal predictors and
  # reorder data
  set.seed(seed)
  predno <- sample(1:ncol(x), size = length(beta))
  x_not_null <- x[ , predno]

  if(independence == TRUE){ # only for Study III

    # randomly permute predictor variables to make them independent
    set.seed(seed)
    x_null <- apply(x[ , which(!(1:ncol(x)) %in% predno)], 2, sample)
  }

  set.seed(seed)
  y <- sample(x = 0:1, size = nrow(x), replace = TRUE)
  ## Noise effect
  x_not_null[y == 1, ] <- t(t(x_not_null[y == 1, ]) + beta)
  x_not_null[y == 0, ] <- t(t(x_not_null[y == 0, ]) - beta)
  # x_null <- t(t(x_null) + beta_noise)
  data <- data.frame(cbind(x_not_null, x_null))
  data$y <- as.factor(y)
  ## Noise effect
  noised <- lapply(X = predno, FUN = function(i, mydata){
  set.seed(i)
    noise_index <- sample(x = 1:nrow(mydata),
                          size = nrow(mydata)*prop.noise.obs,
                          replace = TRUE)
    mydata[noise_index , i] <- sample(mydata[noise_index , i])
    return(mydata[ , i])
  }, mydata = data)
 data[ , predno] <- as.data.frame(do.call(what = "cbind", noised))
 return(list(data = data,
             beta = c(beta, rep(0, ncol(data) - length(beta) - 1))))
}

