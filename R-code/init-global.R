## Turn this parameter to FALSE if you want to run all replicates. Otherwise
## simulations will be ran in testing mode and just 10 replicates by parame-
## ters combination will be ran.
testing_mode <- TRUE

## This file is used to set all initial configurations for the rest of the code.

if(!("Pomona" %in% installed.packages())){
  devtools::install_github("imbs-hl/Pomona")
}

if(!("pacman" %in% installed.packages())){
  install.apckages("pacman")
}

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

if(!("cancerdata" %in% installed.packages())){
  BiocManager::install("cancerdata")
}

pacman::p_load(
  Pomona,
  ranger,
  Boruta,
  data.table,
  ggplot2,
  lemon, ## For positioning the ggplot legend
  tikzDevice,
  batchtools,
  cancerdata, ## to load geneExpr data of veer et al, 2002,
  bayesbio,
  caret,
  ComplexUpset
)

## =============================================================================
##      Dirs
## =============================================================================
## Set the main directory here
user <- Sys.info()[["user"]]
main_dir <- if(user == "fouodo"){
  ## On my local computer
  "/Users/fouodo/projects/RF-hyperparameters-and-variable-selection/R-code"
} else {
  ## On our remote cluster
  "/imbs/home/cesaire/projects/RF-hyperparameters-and-variable-selection/R-code" 
}

result_dir <- file.path(main_dir, "results")
dir.create(result_dir, showWarnings = FALSE)

img_dir <- file.path(main_dir, "img")
dir.create(img_dir, showWarnings = FALSE)

functions_dir <- file.path(main_dir, "functions")
dir.create(functions_dir, showWarnings = FALSE)

data_dir <- file.path(dirname(main_dir), "data")
dir.create(data_dir, showWarnings = FALSE)

registry_dir <- file.path(main_dir, "registry")
dir.create(registry_dir, showWarnings = FALSE) 

## =============================================================================
##      Batchtools configurations
## =============================================================================
##
## Batchtools wrapper
source(file.path(functions_dir, "batchtoolswrapper.R"), chdir = TRUE)
## Batchtools configuration file
config_file <- file.path(main_dir, "batchtools-config/batchtools.conf.R")
template <- file.path(main_dir, "batchtools-config/.batchtools.slurm.tmpl")
nodename <- "login001"

## SLURM partion and acount
partition = "xxxx"
account = "xxxx"
