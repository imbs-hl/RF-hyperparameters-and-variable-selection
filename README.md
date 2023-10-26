# Effect of hyperparameters on variable selection in random forests
Supplementary materials for "Effect of hyperparameters on variable selection in random forests".

All results presented in the paper can be obtained by running the R scripts of this repository. We used the R version 3.5.0 to run our code. The scripts can be run either in a testing or normal mode.

In the testing mode, an interactive session can be used; however, it can require a longer computation time, and just a minimal number of replicates will be run.

In the normal mode, all replicates will be run and the manuscript results will be reproduced. We recommend running the code on a high-performance computer (HPC) in normal mode. Simulations are parallelized using the R package ```batchtools``` version 0.9.15 and, in our case, with a SLURM cluster scheduler.  We share our ```batchtools```'s configuration files to be used and configured to fit the user's computational platform. User configuration is required only for the normal mode, not the testing mode.

## init-global.R
- Ensure the required R packages are installed.
- Set the main directory (```man_dir```) to the directory ```R-code```.
- Set the computation mode: ```testing_mode = TRUE``` (for testing, default) or ```testing_mode = FALSE``` (for normal).
- Set the global variable ```interactive_session = TRUE``` (default) if you want to test the code in an interactive session, and ```interactive_session = FALSE``` if you want to run it in a cluster session. Note that the Boruta jobs will take longer than the Vita ones.

This is required for normal mode on HPC and can be skipped in testing mode:

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

## Generate data only
Use the files ```01-data-only.R``` and ```02-data-only.R``` to generate the simulated data only. For each study, 100 replicates will be generated. We also provide simulated data at Zenodo under DOI: 10.5281/zenodo.8308235.