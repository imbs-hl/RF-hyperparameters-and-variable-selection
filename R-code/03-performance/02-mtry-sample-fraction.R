#' Functions in this file are used to generate the plots shows in the paper. The
#' have the same parameters described as followed. mtry and sample.fraction are
#' varying.
#'
#' @param res_vita_file Vita results for scenario 2 in RDS format
#' @param res_boruta_file Vita results for scenario 2 in RDS format
#' @param default_param Set up parameters to be kept constant
sens02_mtry_prop <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  boruta_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            boruta_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
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
  sens_adj <- data_results[ , mean(SENS.adj, na.rm = TRUE),
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
                by = c("mtry", "Method")]
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method")]$V1])
  data_results <- unique(x = data_results, 
                         by = c("mtry", "Method"))
  ## Now plot sensitivity depending on hyperparameter
  plot_sens <- ggplot(data_results,
                      aes(x = as.numeric(factor(mtry)),
                          y = SENS)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab(label = "mtry.prop") +
    ylab(label = "Empirical sensitivity") +
    ylim(c(0.475, 0.575)) +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          plot.title.position = "plot"#,
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(b)") 
  
  plot_sens <- plot_sens + scale_x_discrete(
    limits = names(table(data_results$mtry.prop)),
    breaks = names(table(data_results$mtry.prop)),
    labels = names(table(data_results$mtry.prop))) 
  return(plot_sens)
}


plot_sens2_mtry_prop <- sens02_mtry_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                              "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01,
                    "replace" = TRUE)
)

plot_sens2_mtry_prop


sens02_sam_frac_prop <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  boruta_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            boruta_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
  ## *********** Filter data results according **********
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
                by = c("sample.fraction", "Method")]
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method")]$V1])
  data_results <- unique(x = data_results, 
                         by = c("sample.fraction", "Method"))
  ## Now plot sensitivity depending on hyperparameter
  plot_sens <- ggplot(data_results,
                      aes(x = as.numeric(as.factor(sample.fraction)),
                          y = SENS)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab(label = "sample.fraction") +
    ylab(label = "Empirical sensitivity") +
    ylim(c(0.473, 0.575)) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(d)") 
  
  plot_sens <- plot_sens + scale_x_discrete(
    limits = names(table(data_results$sample.fraction)),
    breaks = names(table(data_results$sample.fraction)),
    labels = names(table(data_results$sample.fraction))) 
  return(plot_sens)
}


plot_sens2_sample_frac_prop <- sens02_sam_frac_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                              "boruta_veer_mean_res.RDS"),
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01,
                    replace = TRUE)
)

plot_sens2_sample_frac_prop

## ***********************************************
##            FDR
## ***********************************************
##
fdr02_mtry_prop <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  boruta_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            boruta_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
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
                by = c("mtry", "Method")]
  print(data_results[data_results[ , .I[which.min(FDR)],
                                   by = c("Method")]$V1])
  data_results <- unique(x = data_results, 
                         by = c("mtry", "Method"))
  ## Now plot sensitivity depending on hyperparameter
  plot_fdr <- ggplot(data_results,
                     aes(x = as.numeric(as.factor(mtry)),
                         y = FDR)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "mtry.prop") +
    ylab(label = "Empirical FDR") +
    ylim(c(0.005, 0.0575)) +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          plot.title.position = "plot"#,
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    )  +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(a)") 
  plot_fdr <- plot_fdr + scale_x_discrete(
    limits = names(table(data_results$mtry)),
    breaks = names(table(data_results$mtry)),
    labels = names(table(data_results$mtry))) 
  return(plot_fdr)
}


plot_fdr2_mtry_prop <- fdr02_mtry_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                              "boruta_veer_mean_res.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01,
                    "replace" = TRUE)
)

plot_fdr2_mtry_prop


fdr02_sam_frac_prop <- function(
  res_vita_file,
  res_boruta_file,
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size_prop" = 0.01)
){
  ## ************ load result data **********************
  vita_data <- readRDS(res_vita_file)
  boruta_data <- readRDS(res_boruta_file)
  data_results <- data.table(rbindlist(list(vita_data,
                                            boruta_data)))
  data_results$mtry.prop <- round(data_results$mtry.prop, 2)
  data_results$min.node.size_prop <- round(data_results$min.node.size_prop, 2)
  ## *********** Filter data results according **********
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
                by = c("sample.fraction", "Method")]
  print(data_results[data_results[ , .I[which.min(FDR)],
                                   by = c("Method")]$V1])
  data_results <- unique(x = data_results, 
                         by = c("sample.fraction", "Method"))
  ## Now plot sensitivity depending on hyperparameter
  plot_fdr <- ggplot(data_results,
                     aes(x = as.numeric(as.factor(sample.fraction)),
                         y = FDR)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab("sample.fraction") +
    ylab(label = "Empirical FDR") +
    ylim(c(0.005, 0.0575)) +
    theme(legend.position = "bottom",
          text = element_text(size = 14),
          plot.title.position = "plot"#,
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(c)") 
  
  plot_fdr <- plot_fdr + scale_x_discrete(
    limits = names(table(data_results$sample.fraction)),
    breaks = names(table(data_results$sample.fraction)),
    labels = names(table(data_results$sample.fraction))) 
  return(plot_fdr)
}


plot_fdr2_sample_frac_prop <- fdr02_sam_frac_prop(
  res_vita_file = file.path(result_dir_scen2,
                            "vita_veer_mean_res.RDS"),
  res_boruta_file = file.path(result_dir_scen2,
                              "boruta_veer_mean_res.RDS"),
  default_param = c("mtry.prop" = 0.01,
                    "min.node.size_prop" = 0.01,
                    replace = TRUE)
)

plot_fdr2_sample_frac_prop

## =============================================================================
##                      ## Arrange plots
## =============================================================================
##
arrange_mtrySamp02 <- ggpubr::ggarrange(
  plotlist = list(
    plot_fdr2_mtry_prop, #(a)
    plot_sens2_mtry_prop, #(b)
    plot_fdr2_sample_frac_prop, #(c)
    plot_sens2_sample_frac_prop #(d)
  ),
  common.legend = TRUE,
  legend = "bottom",
  ncol = 2,
  nrow = 2,
  align = "hv"
)

arrange_mtrySamp02

ggsave(filename = file.path(result_dir_scen2, "MtrySampFrac02.pdf"),
       plot = arrange_mtrySamp02,
       width = 7, height = 6)
