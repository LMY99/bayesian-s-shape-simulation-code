# Code to reproduce the simulation result in "Bayesian shape-constrained regression for quantifying Alzheimer's disease progression with biomarkers" by Li et al. 2024

This repository contains instructions and scripts used to reproduce the simulation results in *Bayesian shape-constrained regression for quantifying Alzheimer's disease progression with biomarkers* by Li et al. 2024.

## Cloning This Repository

Part of this repository is used for reproducing the simulation result table in Section 4.
It simulates 100 datasets for six model settings for four underlying true progression curves.
A complete cloning can be done in the terminal in the following way, where only the master branch is needed:
```
git clone https://github.com/LMY99/bayesian-s-shape-simulation-code.git <some directory>
cd <repo directory you just cloned into>
git checkout -b master origin/master
```
For parallel computation on clusters, it is advised to clone twice onto two separate folders, and then only perform the `git checkout` clause for one of them.
This way, two batches of datasets can be run simultaneously.

## Setting up Dependencies

The scripts uses multiple existing packages in R, including **tidyverse, splines2, TruncatedNormal, mvtnorm, matrixStats, VGAM, abind** and **hdtg**. The first nine packages can be installed directly through CRAN by using the following code in R console:

```         
install.packages(c(
'tidyverse',
'splines2',
'TruncatedNormal',
'mvtnorm',
'matrixStats',
'VGAM',
'abind'
))
```

The **hdtg** package can be installed through GitHub using the following code in R console:

```         
remotes::install_github("https://github.com/suchard-group/hdtg", build = FALSE)
```

## Running the Code in Parallel on SLURM-equipped Cluster to Reproduce Result

Since there are 1000 dataset to simulate and analyze to reproduce the table, 
it is highly advised to run the batch jobs on a cluster managed by a job scheduler like SLURM.
The table in the said article is generated via *Joint High Performance Computing Exchange(JHPCE)* in the Department of Biostatistics at the Johns Hopkins Bloomberg School of Public Health.
For other type of job schedulers, please refer to their manuals.

There are 7 bash script files that are used as SLURM batch commands in this project.
It can be used in the following way: Run the **deploy.sh** code on the cluster to distribute the 500 jobs.
```
bash deploy.sh
```
Each job will generate an *.rda* file that is named with model-specific prefix and the seed, and contains the bias/variance/MSEs, credible intervals and underlying true values for the fixed effect parameters, progression curve, individual random effects and variance estimates. 
The prefix are 
- **flex** (flexible spline with 0-120 bound) from **main_flex.R**
- **sflex** (flexible spline with 30-90 bound) from **main_flex_short.R**
- **S** (S-shape spline with 0-120 bound) from **main_S.R**
- **sS** (S-shape spline with 30-90 bound) from **main_S_short.R**
- **para** (logistic model) from **main_para.R**

In the case that there are failed jobs, repeat the **deploy.sh** command. You can check the number of succeeded jobs using this command, where an output of less than 500 indicates failed jobs.
```
ls *.rda | wc -l
```
After all jobs succeeded, run
```
bash combine.sh
```
to combine the **rda** files for each model into two, one containing the curve summary and one containing milestones. 
At this time, the individual files can be deleted via
```
rm *_CIs_???.rda
```
You can change the **mother_dir** and/or **directory** variable in the script file to the respective name you choose for the folders.

For the demonstration graph in the appendix, run
```
Rscript plot_fitted.R
```
where the resulting plot will appear as a PDF file **flex_diagnosis.pdf**.

The R script files `plot_MSE.R` and `plot_coverage2.R` and `make_coverage2.R` are for calculating MSE and coverage probabilities using the combined files.

## Running the Code without Cluster

Since there are thousands of jobs needed for reproducing the table, running the jobs sequentially is not recommended. The code below are for demonstration only or for testing the code with one seed value.
For example, to run the parametric model simulation with select seed, run the following command in the terminal:
```
Rscript main_para.R <seed>
```
and to reproduce the result for the parametric model only, run
```
# (Not Recommended)
for setting in 1 4; do
for curve in {1..4}; do
for seed in {001..100}; do
  Rscript main_para.R $seed $setting $curve
done
done
done
```

The BARS code are copied from the package of Wallstrom et al, which can be directly downloaded from https://www.stat.cmu.edu/~kass/bars/bars.html. 
The version we included does not currently support running on Linux-based clusters. It is suggested to run it on windows sequentially:
```
bash deploy_bars.sh
```