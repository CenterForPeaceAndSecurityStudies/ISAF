
# Keeping Your Friends Close, But Acquaintances Closer

This is a package, documentation, and replication repository for:

Gannon, J. Andrés, and Daniel Kent. “Keeping Your Friends Close, but Acquaintances Closer: Why Weakly Allied States Make Committed Coalition Partners.” Journal of Conflict Resolution, (December 2020). https://doi.org/10.1177/0022002720978800.


The Paper:

[Final print](https://journals.sagepub.com/doi/full/10.1177/0022002720978800)

[Pre-print](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/paper/2020-11-15_BurdenShare_GannonKent.pdf)

The Authors:

* [J Andres Gannon](https://jandresgannon.com/) (University of California San Diego)

* [Daniel Kent](https://dnkent.github.io/) (The Ohio State University)

For any questions, please email: [jagannon@ucsd.edu](mailto:jagannon@ucsd.edu).

## Replication Code and Analysis

### Self Contained Package

All of the files necessary for reproducing our analysis are including in a self contained R package "ISAF." You can install the package ISAF from github with the instructions below:

```{r gh-installation, eval = FALSE}
if(!require(devtools)) install.packages("devtools")
devtools::install_github("CenterForPeaceAndSecurityStudies/ISAF")
```

### R-Notebooks

The analysis and figures in the paper and statistical appendix are produced in a number of R Notebooks. 

File Preparation - Dependent Variable:

* [01 Load Data for DV](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/01_DataLoad_DV.Rmd): Loads a novel dataset of national troop contributions to the ISAF operation in Afghanistan (2001-2014).
* [02 Clean Data for DV](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/02_Cleaning_DV.Rmd): Cleans and combines the newly created data

Summary Statistics:

* [03a Summary Stats: Troop Contributions](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/03a_SummaryStats_TroopContributions.Rmd)
* [03b Summary Stats: Alliance Network](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/03b_SummaryStats_AllianceNetwork.Rmd)
* [03c Summary Stats: General](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/03c_SummaryStats_General.Rmd)

File Preparation - Independent Variables:

* [04 Load and Clean Data for EV](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/04_DataLoad-Cleaning_EV.Rmd): Takes in data for the independent variable of interest and loads and merged all control variables.

Model:

* [05 Model](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/docs/05_Model.Rmd): Statistical model that provides the main results of the paper.
