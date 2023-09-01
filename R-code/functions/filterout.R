
## ********************************************************
## Filter out some noise effects
## ********************************************************
## Sceanario 1
filter_out_scen1 <- function(data = vita_power_reg){
  i <- 1
  data_filtered <- lapply(data, function(res){
    set.seed(i)
    message(sprintf("Processing iter = %s\n", i))
    i <<- i + 1
    res <- as.data.table(res)
    res$FDR <- sum(!(res$varindex %in% 1:6) & (res$pvalues < 0.05)) / sum(res$pvalues < 0.05)
    res$FDR.adj <- sum(!(res$varindex %in% 1:6) & (res$pvalue_adj < 0.05)) / sum(res$pvalue_adj < 0.05)
    res_effect <- res[(res$varindex %in% 1:6), ]
    res_noise <- res[!(res$varindex %in% 1:6), ]
    res_noise <- res_noise[sample(x = 1:nrow(res_noise),
                                  size = 20), ]
    return(rbindlist(list(res_effect, res_noise)))
  })
  return(data_filtered)
}


filter_empirical_scen1 <- function(data = vita_power_reg){
  i <- 1
  data_filtered <- lapply(data, function(res){
    set.seed(i)
    message(sprintf("Processing iter = %s\n", i))
    i <<- i + 1
    res <- as.data.table(res)
    res$FDR <- sum(!(res$varindex %in% 1:6) & (res$decision == "Confirmed")) / sum(res$decision == "Confirmed")
    res$FDR.adj <- sum(!(res$varindex %in% 1:6) & (res$decision == "Confirmed")) / sum(res$decision == "Confirmed")
    res_effect <- res[(res$varindex %in% 1:6), ]
    res_noise <- res[!(res$varindex %in% 1:6), ]
    res_noise <- res_noise[sample(x = 1:nrow(res_noise),
                                  size = 20), ]
    return(rbindlist(list(res_effect, res_noise)))
  })
  return(data_filtered)
}

## ********************************************************
## Filter out some noise effects
## ********************************************************
## Scenario 2
filter_out <- function(data = vita_veer_res){
  i <- 1
  data_filtered <- lapply(data, function(res){
    set.seed(i)
    message(sprintf("Processing iter = %s\n", i))
    i <<- i + 1
    res <- as.data.table(res)
    res$FDR <- sum((res$beta == 0) & (res$pvalue < 0.05)) / sum(res$pvalue < 0.05)
    res$FDR.adj <- sum((res$beta == 0) & (res$pvalue_adj < 0.05)) / sum(res$pvalue_adj < 0.05)
    res$SENS <- sum((res$beta != 0) & (res$pvalue < 0.05)) / sum(res$beta != 0)
    res$SENS.adj <- sum((res$beta != 0) & (res$pvalue_adj < 0.05)) / sum(res$beta != 0)
    res$SPEC <- sum((res$beta == 0) & !(res$pvalue < 0.05)) / sum(res$beta == 0)
    res$SPEC.adj <- sum((res$beta == 0) & !(res$pvalue_adj < 0.05)) / sum(res$beta == 0)
    res_effect <- res[beta != 0, ]
    res_noise <- res[beta == 0, ]
    res_noise <- res_noise[sample(x = 1:nrow(res_noise),
                                  size = 20), ]
    return(rbindlist(list(res_effect, res_noise)))
  })
  return(data_filtered)
}


filter_out_empirical <- function(data = vita_veer_res){
  i <- 1
  data_filtered <- lapply(data, function(res){
    set.seed(i)
    message(sprintf("Processing iter = %s\n", i))
    i <<- i + 1
    res <- as.data.table(res)
    res$FDR <- sum((res$beta == 0) & (res$decision)) / sum(res$decision)
    res$FDR.adj <- sum((res$beta == 0) & (res$decision)) / sum(res$decision)
    res$SENS <- sum((res$beta != 0) & (res$decision)) / sum(res$beta != 0)
    res$SENS.adj <- sum((res$beta != 0) & (res$decision)) / sum(res$beta != 0)
    res$SPEC <- sum((res$beta == 0) & !(res$decision)) / sum(res$beta == 0)
    res$SPEC.adj <- sum((res$beta == 0) & !(res$decision)) / sum(res$beta == 0)
    res_effect <- res[beta != 0, ]
    res_noise <- res[beta == 0, ]
    res_noise <- res_noise[sample(x = 1:nrow(res_noise),
                                  size = 20), ]
    return(rbindlist(list(res_effect, res_noise)))
  })
  return(data_filtered)
}
