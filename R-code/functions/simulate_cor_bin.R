## Simulate variables with correlated predictor variables
##
#' Title
#'
#' @param n number of individuals
#' @param k size of group, referred to as n in Degenhardt et al. (2019)
#' @param q vector of groups, referred to as i in Degenhardt et al. (2019)
#' @param p total number of predictors
#' @param null_case if TRUE, the null case scenario is simulated
simulate_cor_bin <- function(n = 100,
                             k = 10,
                             q = 1:6,
                             p = 5000,
                             beta_seed = 1,
                             null_case = FALSE){
  beta <- if((beta_seed %% 2) == 0){
    c(3, 2, 1)
  } else {
    -c(3,2,1)
  }
  ## Draw x1,..,x6
  x1_6 <- sapply(q, function(group){
    runif(n = n, min = 0, max = 1)
  })
  ## Build the target variable
  # generate response
  y <- as.factor(rbinom(n = nrow(x1_6),
                        size = 1,
                        p = plogis(scale(as.matrix(x1_6[ , 1:length(beta)])) %*% beta)))
  
  # check if there are enough observations in both classes
  while(min(table(y)) < 20){
    y <- as.factor(rbinom(n = nrow(x1_6),
                          size = 1,
                          p = plogis(as.matrix(x1_6[,1:length(beta)]) %*% beta)))
  }
  ## Simulate correlated predictor variables mu
  x_cor <- lapply(1:k, function(j){
    x1_6 + (0.01 + (0.5 * ((j - 1) / (k - 1)))) * rnorm(n = n,
                                                        mean = 0,
                                                        sd = 0.3)
  })
  x_cor <- Reduce(f = "cbind", x = x_cor)
  ## Additional non correlated covariates
  noises <- sapply(1:(p - ncol(x_cor)), function(j){
    runif(n = n, min = 0, max = 1)
  })
  data <- data.frame(cbind(x_cor, noises))
  names(data) <- paste("x", 1:ncol(data), sep = "")
  data$y <- if(!null_case){
    y
  } else {
    sample(y)
  }
  return(data)
}
