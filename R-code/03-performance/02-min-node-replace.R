#' Functions in this file are used to generate the plots shows in the paper. The
#' have the same parameters described as followed. replace and min.node.size are
#' varying.
#'
#' @param res_vita_file Vita results for scenario 2 in RDS format
#' @param res_boruta_file Vita results for scenario 2 in RDS format
#' @param default_param Set up parameters to be kept constant
sens02_min_node_prop <- function(
  res_vita_file,
  res_vita_new_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_vita_new_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)))
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
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
  
  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := as.factor(min.node.size_prop)]
  ## ********** Power with BH adjustment *************
  data_results <- data_results[ , mean(SENS.adj, na.rm = TRUE),
                            by = list(#mu,
                              seed,
                              replace,
                              sample.fraction,
                              mtry,
                              min.node.size,
                              holdout,
                              Method)]
  names(data_results)[names(data_results) == "V1"] <- "SENS"
  data_results$Method <- factor(x = data_results$Method,
                            levels = c("Vita", "Boruta"))
  data_results[ , SENS := mean(SENS, na.rm = TRUE),
                by = c("min.node.size", "Method")]
  data_results <- unique(x = data_results, 
                         by = c("min.node.size", "Method"))
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method")]$V1])
  ## Now plot sensitivity depending on hyperparameter
  plot_sens <- ggplot(data_results,
                      aes(x = as.numeric(min.node.size),
                          y = SENS)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab(label = "min.node.size.prop") +
    ylab(label = "Empirical sensitivity") +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1),
           shape = guide_legend(order = 2)) +
    ggtitle("(b)") 
  
  plot_sens <- plot_sens + scale_x_discrete(
    limits = names(table(data_results$min.node.size)),
    breaks = names(table(data_results$min.node.size)),
    labels = names(table(data_results$min.node.size))) 
  return(plot_sens)
}


plot_sens2_min_node_prop <- sens02_min_node_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_vita_new_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.01,
                    "replace" = TRUE)
)

plot_sens2_min_node_prop


sens02_replace_prop <- function(
  res_vita_file,
  res_vita_new_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_vita_new_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  
  ## *********** Filter data results according **********
  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }
  
  data_results <- if("sample.fraction" %in% names(default_param)){
    data_results[sample.fraction == default_param["sample.fraction"], ]
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
  data_results <- data_results[ , mean(SENS.adj, na.rm = TRUE),
                            by = list(#mu,
                              seed,
                              replace,
                              sample.fraction,
                              min.node.size,
                              holdout,
                              Method)]
  names(data_results)[names(data_results) == "V1"] <- "SENS"
  data_results$Method <- factor(x = data_results$Method,
                            levels = c("Vita", "Boruta"))
  data_results[ , SENS := mean(SENS, na.rm = TRUE),
                by = c("replace", "Method")]
  data_results <- unique(x = data_results, 
                         by = c("replace", "Method"))
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method")]$V1])
  ## Now plot sensitivity depending on hyperparameter
  plot_sens <- ggplot(data_results,
                      aes(x = replace,
                          y = SENS)) +
    geom_point(aes(color = Method)) +
    xlab(label = "replace") +
    ylab(label = "Empirical sensitivity") +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
          # plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle("(d)") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1),
           shape = guide_legend(order = 2))
  return(plot_sens)
}


plot_sens2_replace <- sens02_replace_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_vita_new_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01,
                    sample.fraction = 0.632)
)

plot_sens2_replace

## ***********************************************
##            FDR
## ***********************************************
##
fdr02_min_node_prop <- function(
  res_vita_file,
  res_vita_new_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_vita_new_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)))
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
  
  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }
  data_results[ , mu := paste("mu", abs(beta), sep = " == ")]
  data_results[ , beta := as.factor(abs(beta))]
  data_results[ , mtry.prop := as.character(mtry.prop)]
  data_results[ , mtry := mtry.prop]
  data_results[ , min.node.size := round(min.node.size_prop, 2)]
  ## ********** Power with BH adjustment *************
  data_results <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                           by = list(#mu,
                             seed,
                             replace,
                             sample.fraction,
                             mtry,
                             min.node.size,
                             holdout,
                             Method)]
  names(data_results)[names(data_results) == "V1"] <- "FDR"
  data_results$Method <- factor(x = data_results$Method,
                           levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR, na.rm = TRUE),
                by = c("min.node.size", "Method")]
  data_results <- unique(x = data_results, 
                         by = c("min.node.size", "Method"))
  print(data_results[data_results[ , .I[which.min(FDR)],
                                   by = c("Method")]$V1])
  ## Now plot sensitivity depending on hyperparameter
  plot_fdr <- ggplot(data_results,
                     aes(x = as.numeric(factor(min.node.size)),
                         y = FDR)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "min.node.size.prop") +
    ylab(label = "Empirical FDR") +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1),
           shape = guide_legend(order = 2)) +
    ggtitle("(a)") 
  
  plot_fdr <- plot_fdr + scale_x_discrete(
    limits = names(table(data_results$min.node.size)),
    breaks = names(table(data_results$min.node.size)),
    labels = names(table(data_results$min.node.size))) 
  return(plot_fdr)
}


plot_fdr2_min_node_prop <- fdr02_min_node_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_vita_new_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry" = 0.01,
                    "replace" = TRUE)
)

plot_fdr2_min_node_prop


fdr02_replace <- function(
  res_vita_file,
  res_vita_new_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  vita_new_data <- readRDS(res_vita_new_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            vita_new_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  
  ## *********** Filter data results according **********
  data_results <- if("mtry.prop" %in% names(default_param)){
    data_results[mtry.prop == default_param["mtry.prop"], ]
  } else {
    data_results
  }
  
  data_results <- if("sample.fraction" %in% names(default_param)){
    data_results[sample.fraction == default_param["sample.fraction"], ]
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
  data_results <- data_results[ , mean(FDR.adj, na.rm = TRUE),
                           by = list(#mu,
                             seed,
                             replace,
                             sample.fraction,
                             min.node.size,
                             holdout,
                             Method)]
  names(data_results)[names(data_results) == "V1"] <- "FDR"
  data_results$Method <- factor(x = data_results$Method,
                           levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR, na.rm = TRUE),
                by = c("replace", "Method")]
  data_results <- unique(x = data_results, 
                         by = c("replace", "Method"))
  print(data_results[data_results[ , .I[which.min(FDR)],
                                   by = c("Method")]$V1])
  ## Now plot sensitivity depending on hyperparameter
  plot_fdr <- ggplot(data_results,
                     aes(x = replace,
                         y = FDR)) +
    geom_point(aes(colour = Method)) +
    # geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "replace") +
    ylab(label = "Empirical FDR") +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1),
           shape = guide_legend(order = 2)) +
    ggtitle("(c)") 
  return(plot_fdr)
}


plot_fdr2_replace <- fdr02_replace(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_vita_new_file = file.path(result_dir_scen2,
                                "boruta_veer_mean_res.RDS"),
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01,
                    "sample.fraction" = 0.632)
)

plot_fdr2_replace

## =============================================================================
##                      ## Arrange plots
## =============================================================================
##
arrange_MinReplace02 <- ggpubr::ggarrange(
  plotlist = list(plot_fdr2_min_node_prop, #(a)
                  plot_sens2_min_node_prop, #(b)
                  plot_fdr2_replace, #(c)
                  plot_sens2_replace #(d)
  ),
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2,
  align = "hv"
)

arrange_MinReplace02

ggsave(filename = file.path(result_dir, "02-scenario/paper/MinReplace02.pdf"),
       plot = arrange_MinReplace02,
       width = 7, height = 7)
