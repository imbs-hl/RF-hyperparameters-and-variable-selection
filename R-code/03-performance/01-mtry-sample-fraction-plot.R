setwd(file.path(main_dir, "03-performance"))
#' Functions in this file are used to generate the plots shows in the paper. The
#' have the same parameters described as followed.
#'
#' @param res_vita_file Vita results for scenario 1 in RDS format
#' @param res_boruta_file Vita results for scenario 1 in RDS format
#' #' @param testing_mode TRUE for testing mode. Used to adjusted y-axis range in some plots, but not always necessary.
#' @param default_param Set up parameters to be kept constant
plot_sens_mtry_scen1 <- function(res_vita_file,
                                 res_boruta_file,
                                 default_param = c("mtry.prop" = 0.33,
                                                   "replace" =  TRUE,
                                                   "min.node.size" = 5),
                                 testing_mode = TRUE){
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
  data_results[ , SENS := mean(SENS, na.rm = TRUE),
                by = c("mtry", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("mtry", "Method", "k"))
  print(data_results[data_results[ , .I[SENS == max(SENS)],
                                   by = c("Method", "k")]$V1])
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(NA, 0.58)
  }
  sens_all <- ggplot(data_results,
                     aes(x = as.numeric(mtry),
                         y = SENS,
                         shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab(label = "mtry.prop") +
    ylab(label = "Empirical sensitivity") +
    coord_cartesian(ylim = y_lim) +
    # ylim(0.32, 0.57) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot",
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank()
          # plot.title = element_text(hjust = 0.5)
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(b)")
  
  sens_all <- sens_all + scale_x_discrete(
    limits = names(table(data_results$mtry.prop)),
    breaks = names(table(data_results$mtry.prop)),
    labels = names(table(data_results$mtry.prop)))
  return(sens_all)
}

sens_mtry_plot <- plot_sens_mtry_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_sens.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_sens.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size" = 1,
                    replace = TRUE),
  testing_mode = testing_mode)
sens_mtry_plot



## =============================================================================
##                                sample.fraction
## =============================================================================
##
## ****** sensibility sample.fraction ******************************************

plot_sens_sample_frac_scen1 <- function(res_vita_file,
                                        res_boruta_file,
                                        default_param = c("mtry.prop" = 0.33,
                                                          "replace" =  TRUE,
                                                          "min.node.size" = 5),
                                        testing_mode){
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
  data_results[ , Methode := Method]
  data_results[ , Methode := as.character(Method)]
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(x = data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , SENS := mean(SENS), by = c("sample.fraction", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("sample.fraction", "Method", "k"))
  print(data_results[data_results[ , .I[SENS == max(SENS)],
                                   by = c("Method", "k")]$V1])
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(0.30, 0.58)
  }
  sens_all <- ggplot(data_results,
                     aes(x = as.numeric(factor(sample.fraction)),
                         y = SENS, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    # geom_hline(yintercept = 0.05,
    #            size = 0.5,
    #            color = "black",
    #            linetype = "dashed") +
    xlab(label = "sample.fraction") +
    ylab(label = "Empirical sensitivity") +
    ylim(y_lim[1], y_lim[2]) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot",
          # axis.ticks.y = element_blank(),
          # axis.text.y = element_blank(),
          # axis.ticks.x = element_blank(),
          # axis.text.x = element_blank(),
          plot.margin = margin(0.25,1,0.25,0, "cm")
    ) +
    ggtitle("(e)") +
    labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) #+
  sens_all <- sens_all + scale_x_discrete(
    limits = names(table(data_results$sample.fraction)),
    breaks = names(table(data_results$sample.fraction)),
    labels = names(table(data_results$sample.fraction)))
  return(sens_all)
}

sens_sample_frac_plot <- plot_sens_sample_frac_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_sens.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_sens.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "replace" = TRUE),
  testing_mode = testing_mode)
sens_sample_frac_plot


## New plot FDR
plot_fdr_mtry_scen1 <- function(res_vita_file,
                                res_boruta_file,
                                default_param = c("mtry.prop" = 0.33,
                                                  "replace" =  TRUE,
                                                  "min.node.size" = 5),
                                testing_mode = TRUE){
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
  
  data_results <- if("min.node.size_prop" %in% names(default_param)){
    data_results[min.node.size_prop == default_param["min.node.size_prop"], ]
  } else {
    data_results
  }
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR), by = c("mtry", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("mtry", "Method", "k"))
  print(data_results[data_results[ , .I[FDR == min(FDR)],
                                   by = c("Method", "k")]$V1])
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(0, 0.235)
  }
  fdr_all <- ggplot(data_results,
                    aes(x = as.numeric(mtry),
                        y = FDR, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "mtry.prop") +
    ylab(label = "Empirical FDR") +
    ylim(y_lim[1], y_lim[2]) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    labs(linetype = "k", shape = "k") +
    ggtitle("(a)") 
  
  fdr_all <- fdr_all + scale_x_discrete(
    limits = names(table(data_results$mtry.prop)),
    breaks = names(table(data_results$mtry.prop)),
    labels = names(table(data_results$mtry.prop)))
  
  return(fdr_all)
}

fdr_mtry_plot <- plot_fdr_mtry_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_fdr.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_fdr.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size" = 1,
                    replace = TRUE),
  testing_mode = testing_mode)
fdr_mtry_plot

plot_fdr_sample_frac_scen1 <- function(res_vita_file,
                                       res_boruta_file,
                                       default_param = c("mtry.prop" = 0.33,
                                                         "replace" =  TRUE,
                                                         "min.node.size" = 5),
                                       testing_mode = TRUE){
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , FDR := mean(FDR, na.rm = TRUE),
                by = c("sample.fraction", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("sample.fraction", "Method", "k"))
  print(data_results[data_results[ , .I[FDR == min(FDR)],
                                   by = c("Method", "k")]$V1])
  
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(0, 0.235)
  }
  fdr_all <- ggplot(data_results,
                    aes(x = as.numeric(factor(sample.fraction)),
                        y = FDR, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    geom_hline(yintercept = 0.05,
               size = 0.5,
               color = "black",
               linetype = "dashed") +
    xlab(label = "sample.fraction") +
    ylab(label = "Empirical FDR") +
    coord_cartesian(ylim = y_lim) +
    theme(legend.position = "none",
          text = element_text(size = 14),
          plot.title.position = "plot"
    ) +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(d)") +
    labs(linetype = "k", shape = "k") 
  
  fdr_all <- fdr_all + scale_x_discrete(
    limits = names(table(data_results$sample.fraction)),
    breaks = names(table(data_results$sample.fraction)),
    labels = names(table(data_results$sample.fraction)))
  return(fdr_all)
}

## ******* fdr sample.fraction *************************************************

fdr_sam_frac_plot <- plot_fdr_sample_frac_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_fdr.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_fdr.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "replace" = TRUE),
  testing_mode = testing_mode)
fdr_sam_frac_plot


plot_jaccard_mtry_scen1 <- function(res_vita_file,
                                    res_boruta_file,
                                    default_param = c("mtry.prop" = 0.014,
                                                      "replace" =  TRUE,
                                                      "min.node.size" = 1),
                                    testing_mode = TRUE){
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
  
  data_results <- if("min.node.size_prop" %in% names(default_param)){
    data_results[min.node.size_prop == default_param["min.node.size_prop"], ]
  } else {
    data_results
  }
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results[ , mtry := as.factor(mtry)]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , Jaccard := mean(Jaccard), by = c("mtry", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("mtry", "Method", "k"))
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(0.978, 0.990)
  }
  jaccard_all <- ggplot(data_results,
                        aes(x = as.numeric(mtry),
                            y = Jaccard, shape = k, linetype = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method)) +
    xlab("mtry.prop") +
    ylab("Empirical stability") +
    coord_cartesian(ylim = y_lim) +
    theme(legend.position = "right",
          text = element_text(size = 14),
          plot.title.position = "plot"
          # plot.title = element_text(hjust = 0.5)
    ) + labs(linetype = "k", shape = "k") + 
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(c)")
  
  jaccard_all <- jaccard_all + scale_x_discrete(
    limits = names(table(data_results$mtry.prop)),
    breaks = names(table(data_results$mtry.prop)),
    labels = names(table(data_results$mtry.prop)))
  return(jaccard_all)
}


jaccard_vita_mtry_plot <- plot_jaccard_mtry_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_jaccard.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_jaccard.RDS"),
  default_param = c("sample.fraction" = 0.632,
                    "min.node.size" = 1,
                    "replace" = TRUE),
  testing_mode = testing_mode)
jaccard_vita_mtry_plot



## ******* sample.fraction vita ************
## 
plot_jaccard_sample_frac_scen1 <- function(res_vita_file,
                                           res_boruta_file,
                                           default_param = c("mtry.prop" = 0.014,
                                                             "replace" =  TRUE,
                                                             "min.node.size" = 1),
                                           testing_mode = TRUE){
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
  data_results[ , mtry := mtry.prop]
  data_results[ , k := paste(sprintf("%s", k))]
  data_results$Method <- factor(data_results$Method,
                                levels = c("Vita", "Boruta"))
  data_results[ , Jaccard := mean(Jaccard),
                by = c("sample.fraction", "Method", "k")]
  data_results <- unique(x = data_results, 
                         by = c("sample.fraction", "Method", "k"))
  ## Plot
  y_lim <- if(testing_mode){
    c(NA, NA)
  } else {
    c(0.978, 0.998)
  }
  jaccard_all <- ggplot(data_results,
                        aes(x = as.numeric(factor(sample.fraction)),
                            y = Jaccard, shape = k)) +
    geom_point(aes(colour = Method)) +
    geom_line(aes(colour = Method, linetype = k)) +
    xlab("sample.fraction") +
    ylab(label = "Empirical stability") +
    coord_cartesian(ylim = y_lim) +
    theme(legend.position = "right",
          text = element_text(size = 14),
          plot.title.position = "plot",
          plot.margin = margin(0.25,1,0.25,0, "cm")
    ) + labs(linetype = "k", shape = "k") +
    guides(color = guide_legend(override.aes = list(size = 0.5),
                                order = 1)) +
    ggtitle("(f)") 
  
  jaccard_all <- jaccard_all + scale_x_discrete(
    limits = names(table(data_results$sample.fraction)),
    breaks = names(table(data_results$sample.fraction)),
    labels = names(table(data_results$sample.fraction)))
  return(jaccard_all)
}


jaccard_sample_frac_plot <- plot_jaccard_sample_frac_scen1(
  res_vita_file = file.path(result_dir_scen1, "vita_cor_jaccard.RDS"),
  res_boruta_file = file.path(result_dir_scen1, "boruta_cor_jaccard.RDS"),
  default_param = c("mtry.prop" = 0.014,
                    "min.node.size" = 1,
                    "replace" = TRUE),
  testing_mode = testing_mode)
jaccard_sample_frac_plot


## =============================================================================
##                      ## Arrange plots
## =============================================================================
##
arrange_mtrySamp <- ggpubr::ggarrange(
  plotlist = list(fdr_mtry_plot, #(a)
                  sens_mtry_plot, #(b)
                  jaccard_vita_mtry_plot, #(c)
                  fdr_sam_frac_plot, #(d)
                  sens_sample_frac_plot, #(e)
                  jaccard_sample_frac_plot #(f)
  ),
  common.legend = TRUE,
  legend = "bottom",
  ncol = 3,
  nrow = 2,
  align = "hv"
)

arrange_mtrySamp

theme_set(theme_bw())
ggsave(filename = file.path(result_dir_scen1, "MtrySampFrac01.pdf"),
       plot = arrange_mtrySamp,
       width = 10, height = 7)
## Re-set the current directory.
setwd(main_dir)

