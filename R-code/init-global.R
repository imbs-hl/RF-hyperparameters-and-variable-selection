## ****************************************************************************/
##                    Set global configurations
## ****************************************************************************/
## Note: This part need to be checked by the user to ensure variables match with 
## indications mentioned in the Readme file.

## Do the scripts will be run in testing or in normal mode?
testing_mode <- TRUE

## Do the jobs should be run in interactive session?
interactive_session <- TRUE

## Set the main directory
main_dir <- "/imbs/home/cesaire/projects/RF-hyperparameters-and-variable-selection/R-code"

## Set your current working directory to "R-code" (setwd("path/to/R-code"))

## Note: If you are in interactive session, you are done with configuration. 
## Otherwise, configure your batchtools system.

## +++++++++++++++++++++++++++++++#
## batchtools' configurations     #
## +++++++++++++++++++++++++++++++#
##
if(!interactive_session){
  ## Template file
  ## Example: template <- file.path(main_dir, "batchtools-config/.batchtools.slurm.tmpl")
  config_file <- "path/to/batchtools/config/file"
  template <- "path/to/batchtools/template/file"
  ##  (e.g. SLURM) Node, partition and account (See batchtools for details)
  nodename <- "node_name"
  partition = "partition_name"
  account = "your_account"
} else {
  ## Do not modify this line
  config_file <- file.path(main_dir, "batchtools-config/batchtools.multicore.R")
}
## Batchtools wrapper. Do not motify this line
source(file.path(functions_dir, "batchtoolswrapper.R"), chdir = TRUE)

## Set your current working directory to "R-code" and go back to the Readme.
## ********************** End of global configuration *************************/


## ****************************************************************************/
##                  Set directory paths                                       */
## ****************************************************************************/
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

## ****************************************************************************/
##                           Packages loading                                 */
## ****************************************************************************/
##
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