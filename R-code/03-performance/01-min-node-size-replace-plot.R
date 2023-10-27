setwd(file.path(main_dir, "03-performance"))
#' Functions in this file are used to generate the plots shows in the paper. The
#' have the same parameters described as followed.
#'
#' @param res_vita_file Vita results for scenario 1 in RDS format
#' @param res_boruta_file Vita results for scenario 1 in RDS format
#' @param default_param Set up parameters to be kept constant
plot_sens_min_node_scen1 <- function(res_vita_file,
                                     res_boruta_file,
                                     default_param = c("mtry.prop" = 0.33,
                                                       "replace" =  TRUE,
                                                       "min.node.size" = 1)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
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
  data_results[ , Methode := Method]
  data_results[ , Methode := as.character(Method)]
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(x = data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , SENS := mean(SENS), by = c("min.node.size", "Method", "k")]
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method", "k")]$V1])
  ## Plot
  sens_all <- ggplot(data_results,
                     aes(x = as.numeric(min.node.size),
                         y = SENS, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab(label = "min.node.size.prop") +
    ylab("Empirical sensitivity") +
    ylim(c(0.398, 0.56)) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot",
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(b)") 
  return(sens_all)
}

sens_min_node_plot <- plot_sens_min_node_scen1(res_vita_file = file.path(result_dir_scen1, "vita_cor_sens.RDS"),
                                               res_boruta_file = file.path(result_dir_scen1, "boruta_cor_sens.RDS"),
                                               default_param = c("sample.fraction" = 0.632,
                                                                 "mtry.prop" = 0.014,
                                                                 replace = TRUE))
sens_min_node_plot

## =============================================================================
##                                replace
## =============================================================================
##
plot_fdr_replace_scen1 <- function(res_vita_file,
                                   res_boruta_file,
                                   default_param = c("mtry.prop" = 0.33,
                                                     "replace" =  TRUE,
                                                     "min.node.size" = 5)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR, na.rm = TRUE),
                by = c("replace", "Method", "k")]
  print(data_results[data_results[ , .I[which.min(FDR)],
                                   by = c("Method", "k")]$V1])
  
  ## Plot
  fdr_all <- ggplot(data_results,
                    aes(x = replace,
                        y = FDR)) +
    geom_point(aes(colour = Method, shape = k)) +
    # geom_line(aes(linetype = k, colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "replace") +
    ylab(label = "Empirical FDR") +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot",
          plot.margin = margin(0.22,1,0.22,0, "cm")
          # plot.title = element_text(hjust = 0.5)
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(d)") #+
  return(fdr_all)
}

## ******* fdr sample.fraction *************************************************

fdr_replace_plot <- plot_fdr_replace_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_fdr.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_fdr.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "sample.fraction" = 0.632))
fdr_replace_plot


## ****** sensibility sample.fraction ******************************************

plot_sens_replace_scen1 <- function(res_vita_file,
                                    res_boruta_file,
                                    default_param = c("mtry.prop" = 0.33,
                                                      "replace" =  TRUE,
                                                      "min.node.size" = 5)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
  data_results <- if("mtry" %in% names(default_param)){
    data_results[mtry == default_param["mtry"], ]
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
  data_results[ , Methode := Method]
  data_results[ , Methode := as.character(Method)]
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(x = data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , SENS := mean(SENS), by = c("replace", "Method", "k")]
  print(data_results[data_results[ , .I[which.max(SENS)],
                                   by = c("Method", "k")]$V1])
  ## Plot
  sens_all <- ggplot(data_results,
                     aes(x = replace,
                         y = SENS, shape = k)) +
    geom_point(aes(colour = Method)) +
    xlab(label = "replace") +
    ylab(label = "Empirical sensitivity") +
    ylim(c(0.398, 0.56)) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot",
          plot.margin = margin(0.22,1,0.22,0, "cm")
    ) +
    ggtitle("(e)") + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) #+
  return(sens_all)
}

sens_replace_plot <- plot_sens_replace_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_sens.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_sens.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "sample.fraction" = 0.632))
sens_replace_plot


plot_jaccard_min_node_scen1 <- function(res_vita_file,
                                        res_boruta_file,
                                        default_param = c("mtry.prop" = 0.014,
                                                          "replace" =  TRUE,
                                                          "min.node.size" = 1)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , Jaccard := mean(Jaccard), by = c("min.node.size",
                                                   "Method", "k")]
  ## Plot
  jaccard_all <- ggplot(data_results,
                        aes(x = as.numeric(min.node.size),
                            y = Jaccard, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab("min.node.size.prop") +
    ylab("Empirical stability") +
    theme(legend.position = "right",
          text = element_text(size = 14),
          plot.title.position = "plot",
          # plot.title = element_text(hjust = 0.5)
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(c)")
  return(jaccard_all)
}


jaccard_min_node_plot <- plot_jaccard_min_node_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_jaccard.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_jaccard.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.014,
                    "replace" = TRUE))
jaccard_min_node_plot


## New plot FDR
plot_fdr_min_mode_scen1 <- function(res_vita_file,
                                    res_boruta_file,
                                    default_param = c("mtry.prop" = 0.33,
                                                      "replace" =  TRUE,
                                                      "min.node.size" = 5)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR), by = c("min.node.size", "Method", "k")]
  ## Plot
  fdr_all <- ggplot(data_results,
                    aes(x = as.numeric(min.node.size),
                        y = FDR, shape = k, linetype = k)) +
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
          plot.title.position = "plot",
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(a)") 
  
  return(fdr_all)
}

fdr_min_node_plot <- plot_fdr_min_mode_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_fdr.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_fdr.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "mtry.prop" = 0.014,
                    replace = TRUE))
fdr_min_node_plot


## ******* Jaccard replace ************
## 
plot_jaccard_replace_scen1 <- function(res_vita_file,
                                       res_boruta_file,
                                       default_param = c("mtry.prop" = 0.014,
                                                         "replace" =  TRUE,
                                                         "min.node.size" = 1)){
  ## --------------------------------------------
  ##    Load result files
  ## --------------------------------------------
  ##
  res_vita <- readRDS(res_vita_file)
  res_vita[ , index := 1:.N]
  res_boruta <- readRDS(res_boruta_file)
  res_boruta[ , index := 1:.N]
  data_results <- rbindlist(list(res_vita, res_boruta))
  ## --------------------------------------------
  ##    Extract results with default parameters
  ## --------------------------------------------
  ##
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , Jaccard := mean(Jaccard),
                by = c("sample.fraction", "Method", "k")]
  ## Plot
  jaccard_all <- ggplot(data_results,
                        aes(x = replace,
                            y = Jaccard,
                            shape = k)) +
    geom_point(aes(colour = Method)) +
    # geom_line(aes(linetype = k, colour = Method)) +
    xlab("replace") +
    ylab(label = "Empirical stability") +
    theme(legend.position = "right",
          text = element_text(size = 14),
          plot.title.position = "plot",
          # axis.ticks.y = element_blank(),
          # axis.text.y = element_blank(),
          plot.margin = margin(0.22,1,0.22,0, "cm")
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(f)") #+
  return(jaccard_all)
}


jaccard_replace_plot <- plot_jaccard_replace_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_jaccard.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_jaccard.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "sample.fraction" = 0.632))
jaccard_replace_plot



## =============================================================================
##                      ## Arrange plots
## =============================================================================
##
MinReplace01 <- ggpubr::ggarrange(
  plotlist = list(fdr_min_node_plot, #(a)
                  sens_min_node_plot, #(b)
                  jaccard_min_node_plot, #(c)
                  fdr_replace_plot, #(d)
                  sens_replace_plot, #(e)
                  jaccard_replace_plot #(f)
  ),
  common.legend = TRUE,
  legend = "bottom",
  ncol = 3,
  nrow = 2,
  align = "hv"
)

theme_set(theme_bw())
MinReplace01

theme_set(theme_bw())
ggsave(filename = file.path(result_dir_scen1, "MinReplace01.pdf"),
       plot = MinReplace01,
       width = 10, height = 7)
## Re-set the current directory.
setwd(main_dir)
