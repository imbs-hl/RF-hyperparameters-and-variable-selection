## ****************************************************************************/
##                    Set global configurations
## ****************************************************************************/
## Note: This part need to be checked by the user to ensure variables match with 
## indication mentioned in the Readme file.

## Do the scripts will be run in testing or in normal mode?
testing_mode <- TRUE
## Do the jobs should be ran in interactive session?
interactive_session <- TRUE
## Set the main directory
main_dir <- "/imbs/home/cesaire/projects/RF-hyperparameters-and-variable-selection/R-code"
## Note: If you are in interactive session, you are done with configuration. Set
## your current working directory to "R-code" and go back to the Readme.
## Otherwise, configure your batchtools system.

## +++++++++++++++++++++++++++++++#
## batchtools configuration       #
## +++++++++++++++++++++++++++++++#
## Configuration file: Skip config_file in testing mode. Otherwise replace
## "xxxx" by the correct path if your in normal mode.
## Example: config_file <-file.path(main_dir, "batchtools-config/batchtools.conf.R")
config_file <- if(interactive_session){
  file.path(main_dir, "batchtools-config/batchtools.multicore.R")
} else {
  "xxxx"
}
## Template file
## Example: template <- file.path(main_dir, "batchtools-config/.batchtools.slurm.tmpl")
template <- "xxxx"
## Cluster node name
nodename <- "xxxx"
## SLURM partition and account
partition = "xxxx"
account = "xxxx"

## End of configuration.
## Set your current working directory to "R-code" and go back to the Readme.
## ****************************************************************************/


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
## Batchtools wrapper
source(file.path(functions_dir, "batchtoolswrapper.R"), chdir = TRUE)