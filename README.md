# ISAF

You can find replication materials and general paper information for "Keeping Your Friends Close, But Acquaintances Closer: Why Weakly Allied States Make Committed Coalition Partners" by J. Andres Gannon and Daniel Kent.

For any questions, you can email: [jagannon@ucsd.edu](mailto:jagannon@ucsd.edu).



---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


```{r, echo = FALSE}
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "README-"
)
``` 
 
# Keeping Your Friends Close, But Acquaintances Closer

This is a package, documentation, and replication repository for the paper "Keeping Your Friends Close, But Acquaintances Closer: Why Weakly Allied States Make Committed Coalition Partners" (accepted for publication at the Journal of Conflict Resolution) 2020

The Paper:

[Keeping Your Friends Close, But Acquaintances Closer - Read the Paper](https://github.com/CenterForPeaceAndSecurityStudies/ISAF/blob/master/paper/2020-11-14_BurdenShare_GannonKent.pdf)

The Authors:

* [J Andres Hannon](https://jandresgannon.com/)(University of California San Diego)

* [Daniel Kent](https://dnkent.github.io/)(The Ohio State University)


## Replication Code and Analysis

### Self Contained Package

All of the files necessary for reproducing our analysis are including in a self contained R package "ISAF." You can install the package ISAF from github with the instructions below:

```{r gh-installation, eval = FALSE}
if(!require(devtools)) install.packages("devtools")
devtools::install_github("CenterForPeaceAndSecurityStudies/ISAF")
```

### R-Notebooks

The analysis and figures in the paper and statistical appendix are produced in a number of R Notebooks. 

NOTE: Several parts of this analysis are stochastic, specific coefficient estimates and p-values will vary with each execution. Substantive results will be consistent across runs. We encourage the reader to run the replication multiple times and observe the variation. 

* [00 Project Setup](https://rexdouglass.github.io/MeasuringLandscape/00_project_setup.nb.html): Useful commands for installing necessary packages and setting up the project.

File Preparation:

* [01 Prep Events Counts](https://rexdouglass.github.io/MeasuringLandscape/01_prep_events_counts.nb.html): Loads and cleans a novel dataset of violent events observed during the 1950s Mau Mau Rebellion.
* [02 Prep Gazetteers](https://rexdouglass.github.io/MeasuringLandscape/02_prep_gazeteers.nb.html): Cleans and combines a large number of gazetteer of place names for looking up locations by name and retrieving their coordinates.

Fuzzy Matcher: A supervised learning pipeline for matching two placenames to one another even when they are spelled slightly differently.

* [03 Fuzzy Matcher Stage 1 (Locality Sensitive Hashing)](https://rexdouglass.github.io/MeasuringLandscape/03_fuzzy_matcher_stage_1_lsh.nb.html)
* [04 Fuzzy Matcher Stage 2 (XGBoost)](https://rexdouglass.github.io/MeasuringLandscape/04_fuzzy_matcher_stage_2_xgboost.nb.html)

Georeferencer: A supervised learning pipeline for assigning a real-world coordinate to a placename.

* [05 Georeferencer](https://rexdouglass.github.io/MeasuringLandscape/05_georeferencer.nb.html): Takes in locations of events described as text and returns all possible matches across different gazetteers.
* [06 Ensemble and Hand Rules](https://rexdouglass.github.io/MeasuringLandscape/06_ensemble_and_hand_rules.nb.html): Ranks the returned matches from best to worst. First, using simple hand rules of what kind of match to prefer over others. Then second, with a supervised model that attempts to predict which match will be geographically closest to the true location (fewest kilometers away from the right answer).


Analysis: Main analysis of the paper.

* [07 Recall Accuracy](https://rexdouglass.github.io/MeasuringLandscape/07_recall_accuracy.nb.html): Rate georeferencing options in terms of recall (how many event locations they recover) and accuracy (how far away their imputed locations tend to be from the true location)
* [08 Predict Missingness DV](https://rexdouglass.github.io/MeasuringLandscape/08_predict_missingness_lhs.nb.html): Rate georeferencing options in terms of how systematic they are at recovering locations for certain kinds of events but not others.
* [09 Predicted Effects](https://rexdouglass.github.io/MeasuringLandscape/09_predicted_effects.nb.html): Demonstrate what kinds of events tend to systematically get excluded. Here, in terms of whether the event would have received an original military coordinate or not.
* [10 Bias](https://rexdouglass.github.io/MeasuringLandscape/10_bias.nb.html): Demonstrate that the kinds of locations that are imputed are different from the true locations, in terms of things like population, distance from roads, ruggedness, etc.
* [11 So What](https://rexdouglass.github.io/MeasuringLandscape/11_so_what.nb.html): Demonstrate that different georeferencing decisions will produce different results in a simple linear regression model in terms of both statistical significance and substantive effects.

* [12 Kenya Events with Suggested Codings](https://rexdouglass.github.io/MeasuringLandscape/12_Kenya_events_with_suggested_codings.nb.html): Release the event dataset with a single georeferencing based on the ensemble method.
