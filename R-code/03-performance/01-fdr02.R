#' Functions in this file are used to generate the plots shows in the paper. The
#' have the same parameters described as followed.
#'
#' @param res_vita_file Vita results for scenario 2 in RDS format
#' @param res_boruta_file Vita results for scenario 2 in RDS format
#' @param default_param Set up parameters to be kept constant
#'
#' @return
#' @export
#'
#' @examples
fdr02_replace <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)),
                             fill = TRUE)
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)

  ## *********** Filter data results according **********
  data_results <- if("sample.fraction" %in% names(default_param)){
    data_results[sample.fraction == default_param["sample.fraction"], ]
  } else {
    data_results
  }

  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }

  data_results <- if("min.node.size_prop" %in% names(default_param)){
    data_results[min.node.size_prop == default_param["min.node.size_prop"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , Method := as.character(Method)]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , sample.fraction := as.character(sample.fraction)]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := min.node.size_prop]
  ## ********** Power with BH adjustment *************
  fdr_adj <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                           by = list(#mu,
                             seed,
                             replace,
                             sample.fraction,
                             mtry,
                             min.node.size,
                             holdout,
                             Method)]
  names(fdr_adj)[names(fdr_adj) == "V1"] <- "FDR"
  fdr_adj$Method <- factor(x = fdr_adj$Method,
                             levels = c("Vita", "Boruta"))
  ## Now plot power depending on hyperparamter
  plot_fdr <- ggplot2::ggplot(data = fdr_adj,
                              aes(x = replace, y = FDR)) +
    # geom_line(aes(linetype = Zurücklegen, color = Zurücklegen)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.05,
               linetype = "dashed",
               color = "black", size = 0.5) +
    theme(legend.position = "bottom") +
    # xlab(expression(mu)) +
    ylab("Empirical FDR") +
    theme(legend.position = "bottom",
          text = element_text(size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 12),
          panel.spacing = unit(1, "lines")) +
    facet_wrap( ~ Method, labeller=label_parsed)

  return(plot_fdr)
}


plot_fdr2_replace <- fdr02_replace(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01)
)

plot_fdr2_replace

ggsave(filename = file.path(result_dir, "02-scenario/paper/fdrReplace02.pdf"),
       plot = plot_fdr2_replace,
       width = 5, height = 3.5)

## ***********************************************
##            Sample fraction
## ***********************************************
##
fdr02_sample_frac <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01)
){
  ## ************ Set current tuning parameter **********
  tuning_param <- setdiff(c("sample.fraction",
                            "mtry.prop",
                            "min.node.size_prop"),
                          names(default_param))
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)

  ## *********** Filter data results according **********
  data_results <- if("replace" %in% names(default_param)){
    data_results[replace == default_param["replace"], ]
  } else {
    data_results
  }

  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }

  data_results <- if("min.node.size_prop" %in% names(default_param)){
    data_results[min.node.size_prop == default_param["min.node.size_prop"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , sample.fraction := as.character(sample.fraction)]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := min.node.size_prop]
  ## ********** FDR with BH adjustment *************
  fdr_adj <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                             by = list(#mu,
                               seed,
                               replace,
                               sample.fraction,
                               mtry,
                               min.node.size,
                               holdout,
                               Method)]
  names(fdr_adj)[names(fdr_adj) == "V1"] <- "FDR"
  fdr_adj$Method <- factor(x = fdr_adj$Method,
                           levels = c("Vita", "Boruta"))
  ## Now FDR power depending on hyperparamter
  plot_fdr <- ggplot2::ggplot(data = fdr_adj,
                                aes(x = sample.fraction, y = FDR)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.05,
               linetype = "dashed",
               color = "black", size = 0.5) +
    theme(legend.position = "bottom") +
    ylab("Empirical FDR") +
    theme(legend.position = "bottom",
          text = element_text(size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 12),
          panel.spacing = unit(1, "lines")) +
    facet_wrap( ~ Method, labeller=label_parsed)
  return(plot_fdr)
}

plot_fdr2_sample_frac <- fdr02_sample_frac(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01,
                    "replace" = TRUE)
)

plot_fdr2_sample_frac

ggsave(filename = file.path(result_dir, "02-scenario/paper/fdrSampleFrac02.pdf"),
       plot = plot_fdr2_sample_frac,
       width = 5, height = 3.5)

## ***********************************************
##            mtry
## ***********************************************
##
fdr02_mtry_prop <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ Set current tuning parameter **********
  tuning_param <- setdiff(c("sample.fraction",
                            "mtry.prop",
                            "min.node.size_prop"),
                          names(default_param))
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data),
                                       fill = TRUE))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)

  ## *********** Filter data results according **********
  data_results <- if("sample.fraction" %in% names(default_param)){
    data_results[sample.fraction == default_param["sample.fraction"], ]
  } else {
    data_results
  }

  data_results <- if("replace" %in% names(default_param)){
    data_results[replace == default_param["replace"], ]
  } else {
    data_results
  }

  data_results <- if("min.node.size_prop" %in% names(default_param)){
    data_results[min.node.size_prop == default_param["min.node.size_prop"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , mtry.prop := as.character(mtry.prop)]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := min.node.size_prop]
  ## ********** Power with BH adjustment *************
  fdr_adj <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                           by = list(#mu,
                             seed,
                             replace,
                             sample.fraction,
                             mtry,
                             min.node.size,
                             holdout,
                             Method)]
  names(fdr_adj)[names(fdr_adj) == "V1"] <- "FDR"
  fdr_adj$Method <- factor(x = fdr_adj$Method,
                           levels = c("Vita", "Boruta"))
  ## Now FDR power depending on hyperparamter
  plot_fdr <- ggplot2::ggplot(data = fdr_adj,
                              aes(x = mtry, y = FDR)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.05,
               linetype = "dashed",
               color = "black", size = 0.5) +
    theme(legend.position = "bottom") +
    ylab("Empirical FDR") +
    theme(legend.position = "bottom",
          text = element_text(size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 12),
          panel.spacing = unit(1, "lines")) +
    facet_wrap( ~ Method, labeller = label_parsed)
  return(plot_fdr)
}


plot_fdr2_mtry_prop <- fdr02_mtry_prop(
  res_vita_file = file.path(result_dir_scen2, "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2, "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01,
                    "replace" = TRUE)
)

plot_fdr2_mtry_prop

ggsave(filename = file.path(result_dir, "02-scenario/paper/fdrMtryProp02.pdf"),
       plot = plot_fdr2_mtry_prop,
       width = 5, height = 3.5)


## ***********************************************
##            min.node.size
## ***********************************************
##
fdr02_min_node <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.01)
){
  ## ************ Set current tuning parameter **********
  tuning_param <- setdiff(c("sample.fraction",
                            "mtry.prop",
                            "min.node.size_prop"),
                          names(default_param))
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data), fill = TRUE))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
  ## *********** Filter data results according **********
  data_results <- if("sample.fraction" %in% names(default_param)){
    data_results[sample.fraction == default_param["sample.fraction"], ]
  } else {
    data_results
  }

  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }

  data_results <- if("replace" %in% names(default_param)){
    data_results[replace == default_param["replace"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , min.node.size_prop := as.character(min.node.size_prop)]
  data_results[ , replace := sprintf("replace == '%s'",
                                     as.character(replace))]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := min.node.size_prop]

  ## ********** Power with BH adjustment *************
  fdr_adj <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                           by = list(#mu,
                             seed,
                             replace,
                             sample.fraction,
                             mtry,
                             min.node.size,
                             holdout,
                             Method)]
  names(fdr_adj)[names(fdr_adj) == "V1"] <- "FDR"
  fdr_adj$Method <- factor(x = fdr_adj$Method,
                           levels = c("Vita", "Boruta"))
  ## Now FDR power depending on hyperparamter
  plot_fdr <- ggplot2::ggplot(data = fdr_adj,
                              aes(x = min.node.size, y = FDR)) +
    geom_boxplot() +
    geom_hline(yintercept = 0.05,
               linetype = "dashed",
               color = "black", size = 0.5) +
    theme(legend.position = "bottom") +
    # xlab(expression(mu)) +
    ylab("Empirical FDR") +
    theme(legend.position = "bottom",
          text = element_text(size=14),
          axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
          axis.text = element_text(size = 12),
          panel.spacing = unit(1, "lines")) +
    facet_wrap( ~ Method, labeller=label_parsed)
  return(plot_fdr)
}


plot_fdr2_min_node <- fdr02_min_node(
  res_vita_file = file.path(result_dir_scen2, "vita_veer_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2, "vita_new_veer_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.01,
                    "replace" = TRUE)
)

plot_fdr2_min_node

ggsave(filename = file.path(result_dir, "fdrMinNodeProp02.pdf"),
       plot = plot_fdr2_min_node,
       width = 5, height = 3.5)
