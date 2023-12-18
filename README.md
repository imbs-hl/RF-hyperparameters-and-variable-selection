# Effect of hyperparameters on variable selection in random forests

Supplementary materials for "Effect of hyperparameters on variable selection in random forests".

All results presented in the paper can be obtained by running the R scripts of this repository. We recommend to run the code on R version  $\geq$ 4.0.3. The scripts can be run either in a testing or normal mode. The testing mode runs only a small number of replicates for the Vita method and varies only the hyperparameter ```mtry```, whereas the normal is designed to reproduce the manuscript results. Simulations are parallelized using the R package ```batchtools``` version 0.9.15. We share our ```batchtools```'s configuration files to be potentially used and configured to fit the user's computational platform.

Use this command to clone the git repository:

```git clone https://github.com/imbs-hl/RF-hyperparameters-and-variable-selection.git```

## Testing mode

The script is set in testing mode as default. Scenario 1 simulations are conducted for $k = 10$ and $k = 50$. For scenarios 1 and 2, the script will generate similar figures to figures 1-(a) and 2-(a) of the manuscript; that is, for each value of  ```mtry```, the mean value of the estimated FDR. The estimated FDR in each replicate is calculated with the significant threshold of $0.05$ and using the adjusted empirical $p$-values of the Vita testing method, stored as a ```data.frame```, which also contains values of the hyperparameters used to grow the random forest.  Simulated data and related meta information are also stored in the testing mode. For scenario 1, a list containing the following information is stored for each replicate.

- ```seed``` The seed value for initializing the random number generator.
- ```k```: The size of the correlation block.
- ```q```: The indexes of the correlated variables $q$.
- ```p```: The total number of predictor variables $p$.
- ```data```: The simulated dataset (as ```data.frame```).

 For scenario 2, a list containing the following information is stored for each replicate.
 
 - ```betas```: The effect values.
 - ```n_beta```: The number of effect variables.
 - ```effect_seed```: The seed.
 - ```subset```: The subset of effect predictor variables.
 - ```subsetsize```: The size of the subset of effect predictor variables.
 - ```independence```: The flag indicating whether predictor variables are independent.
 - ```null_case```: The flag indicating whether the null case is simulated, i.e., no relationship between predictor variables and the response variable.
 - ```data```: The simulated dataset (as ```data.frame```).

An interactive session can be used to run the script in testing mode, and batchtools can also be run in an interactive mode.

- Set your current working directory in R to the directory ```R-code``` of your cloned repository.
- Run the following code:

```R
source("init-global.R")
# Scenario 1: simulation of five replicates of scenario 1 as described in the manuscript.
## Simulate data: data sets and meta information will be stored in a single rds object, which can be found under results/scenario1/study1.rds
source("01-scenario1/01-data-only.R")
## Test the Vita approach: results of the variable selection will be saved in the bacthtools registry called "vita-cor" indicated by the function wrap_batchtools.
source("01-scenario1/01-vita-jobs.R")
## Estimate of the FDR: variable selection results will be loaded from the registry to estimate the FDR.
source("03-performance/01-fdr_empirical_run.R")
## Plot: results will be stored in results/scenario1/MtryTest1.pdf
source("03-performance/01-mtry-sample-fraction-plot.R")

# Scenario 2: simulation of five replicates of scenario 2 as described in the manuscript.
## Simulate data: simulated data will be stored in results/scenario2/study2.rds
source("02-scenario2/02-data-only.R")
## Test the Vita approach: variable selection results will be saved in the batchtools registry called vita_veer_mean_all.
source("02-scenario2/02-veer1.R")
## Estimate the FDR and plot results: variable selection results will be loaded from the registry to estimate the FDR, and results will be plotted and saved in results/scenario2/MtryTest2.pdf.
source("03-performance/02-mtry-sample-fraction.R")
```

Note: Vita's results may not mimic the tendency shown in the manuscript in testing mode because the method is unstable.

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
Use the files ```01-data-only.R``` and ```02-data-only.R``` to generate the simulated data only. For each study, 100 replicates will be generated in the normal mode. We also provide simulated data at Zenodo under DOI: 10.5281/zenodo.8308235.

<p align="center";style="font-size:10%;margin : 0; padding-top:0;"><em>By: Cesaire J. K. Fouodo; Email: cesaire.kuetefouodo(at)uni-luebeck.de</em></p>