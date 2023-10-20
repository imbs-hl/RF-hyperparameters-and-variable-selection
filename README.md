# Effect of hyperparameters on variable selection in random forests
Supplementary Material for "Effect of hyperparameters on variable selection in random forests."

All results presented in the paper can be obtained by running the R scripts of this repository. We used the R version 3.5.0 to run our code.

Simulations are parallelized using the R package ```batchtools``` version 0.9.15. We share our ```batchtools```'s configuration files to be used and configured to fit the user's computational platform. 

## init.global.R
- Ensure the required R packages are installed
- Set the main directory (```man_dir```) to the directory ```R-code```
- Configure your batchtools' resources, the path to your batchtools' configuration file, your schedule (e.g. SLURM) partition, and your user account. Please ensure the partition (See batchtools) you use allows you the ```walltime``` (See batchtools) you set.

## Simulation study 1
- Set the directory ```01-scenario1``` as the current working directory.
- Run the files ```01-vita.R``` and ```01-boruta.R``` to submit jobs for the Vita and the Boruta variable selection methods.

## Simulation study 2
- Set the directory ```02-scenario2``` as the current working directory.
- Run the files ```01-veer1``` to submit jobs for the Vita and the Boruta variable selection methods.

## Performance measures
Set the directory ```03-perforamnce``` as current directory. 
### Study 1
Only run files in this directory once your submitted jobs from ```01-scenario1``` are done.

- Use ```01-fdr_empirical_run.R``` to submit jobs to estimate the FRD for Vita and Boruta for simulation study 1.
- Use ```01-jaccard_empirical_run.R``` to submit jobs to estimate the stability for Vita and Boruta for simulation study 1.
- Use ```01-fdr_empirical_run.R``` to submit jobs to estimate the FRD for Vita and Boruta for simulation study 1.
- Use ```01-sens_empirical_run.R``` to submit jobs to estimate the sensitivity for Vita and Boruta for simulation study 1.

To generate the figures from study 1 in the article, run the files ```01-mtry-sample-fraction.R``` and ```01-min-node-size-replace.R```.

### Study 2
Only run files in this directory once your submitted jobs from ```02-scenario2``` are done.

To generate the figures from study 2 in the article, run the files ```02-mtry-sample-fraction.R``` and ```02-min-node-replace```.

## Generate data only
Use the files ```01-data-only.R``` and ```02-data-only.R``` to generate the simulated data only. For each study, 100 replicates will be generated. We also provide simulated data at Zenodo under DOI: 10.5281/zenodo.8308235.