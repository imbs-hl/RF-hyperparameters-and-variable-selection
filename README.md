# Effect of hyperparameters on variable selection in random forests

Supplementary materials for "Effect of hyperparameters on variable selection in random forests".

All results presented in the paper can be obtained by running the R scripts of this repository. We recommend to run the code on R version  $\geq$ 4.0.3. The scripts can be run either in a testing or normal mode. The testing mode runs only a small number of replicates, whereas the normal is designed to reproduce the manuscript results. Simulations are parallelized using the R package ```batchtools``` version 0.9.15. We share our ```batchtools```'s configuration files to be potentially used and configured to fit the user's computational platform.

Use this command to clone the git repository:

```git clone git@github.com:imbs-hl/RF-hyperparameters-and-variable-selection.git```

## Testing mode

The script is set in testing mode as default. An interactive session can be used, and batchtools can also be run in an interactive mode. To run the script in testing mode:

- In the file ```init.global.R```, set the variable ```main_dir``` to your the directory ```R-code``` of your cloned repository.
- Set your current working directory in R to the directory ```R-code``` of your cloned repository.
- Run the following code:

```R
# Scenario 1

## Run replicates
source("init-global.R")
source("01-scenario1/01-vita-jobs.R")
## Estimate performance measures
source("03-performance/01-fdr_empirical_run.R")
source("03-performance/01-jaccard_empirical_run.R")
source("03-performance/01-sens_empirical_run.R")
source("03-performance/01-spec_empirical_run.R")
## Plot results
source("03-performance/01-mtry-sample-fraction-plot.R")

# Scenario 2

## Run replicates
source("02-scenario2/02-veer1.R")

## Estimate and plot results

source("03-performance/02-mtry-sample-fraction.R")
```

Figures are saved in ```results```.

Note: Results of Vita may not mimic the tendency shown in the manuscript in testing mode because the method is unstable.

## Normal mode

The normal mode reproduces the results of the manuscript. We recommend running the code on a high-performance computer (HPC) in normal mode. In our case,  we used a SLURM cluster scheduler.

### init-global.R
- Ensure the required R packages are installed.
- Set the main directory (```man_dir```) to the directory ```R-code```.
- Set the computation mode: ```testing_mode = FALSE``` (for normal).
- Set the global variable ```interactive_session = TRUE``` (default) if you want to test the code in an interactive session, and ```interactive_session = FALSE``` if you want to run it in a cluster session. Note that the Boruta jobs will take longer than the Vita ones.
- Configure your batchtools' resources accordingly: the path to your batchtools' configuration file, your scheduler (e.g. SLURM) partition, and your user account. Please ensure the partition (See batchtools) you use allows you the ```walltime``` (see batchtools) you set.
- Set the path of your scheduler template file in ```template``` and the name of your cluster node in ```nodename```. 
- Ensure you set the correct R library path in your scheduler template file.

## Simulations

### Study 1
- Set the directory ```01-scenario1``` as the current working directory.
- Run the files ```01-vita.R``` and ```01-boruta.R``` to start jobs for the Vita and the Boruta variable selection methods and wait until they are completed.

### Study 2
- Set the directory ```02-scenario2``` as the current working directory.
- Run the files ```01-veer1``` to start jobs for the Vita and the Boruta variable selection methods and wait until they are completed.

## Performance measures
Set the directory ```03-perforamnce``` as the current directory. 

### Study 1
Only run files in this directory once your submitted jobs from ```01-scenario1``` are done.

- Use ```01-fdr_empirical_run.R``` to start jobs to estimate the FDR for Vita and Boruta for simulation study 1.
- Use ```01-jaccard_empirical_run.R``` to start jobs to estimate the stability for Vita and Boruta for simulation study 1.
- Use ```01-sens_empirical_run.R``` to start jobs to estimate the sensitivity for Vita and Boruta for simulation study 1.
- Use ```01-spec_empirical_run.R``` to start jobs to estimate the specificity for Vita and Boruta for simulation study 1.
- To generate the figures from study 1 in the article, run the files ```01-mtry-sample-fraction-plot.R``` and ```01-min-node-size-replace-plot.R```.

### Study 2
Only run files in this directory once your submitted jobs from ```02-scenario2``` are done.

To generate the figures from study 2 in the article, run the files ```02-mtry-sample-fraction.R``` and ```02-min-node-replace```.

Figures are saved in ```results```.

## Generate data only
Use the files ```01-data-only.R``` and ```02-data-only.R``` to generate the simulated data only. For each study, 100 replicates will be generated. We also provide simulated data at Zenodo under DOI: 10.5281/zenodo.8308235.

<p align="center";style="font-size:10%;margin : 0; padding-top:0;"><em>By: Cesaire J. K. Fouodo; Email: cesaire.kuetefouodo(@)uni-luebeck.de</em></p>