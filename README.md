# State Legitimacy
This repository contains the replication material for "Institutional Legitimacy in sub-Saharan Africa," which is forthcoming in *Democratization* (2019). 

## Authors
Sarah K. Dreier and Milli Lake.

## Recode and prepare data

The original dataset [merged_r6_data_2016_36countries2.sav](http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav) contains the merged Afrobarometer Round 6 (2016) survey data. It is available to be downloaded at from the [Afrobarometer website](http://afrobarometer.org).

[variable_recode.R](source/variable_recode.R) cleans and recodes variables from the source Afrobarometer data and subsets into a new dataset (afro_courts_police.RData).

## Main analysis

"main_analysis.R" runs the models and plots featured in the article's main analysis. 

The new dataset (afro_courts_police.RData) can be loaded from the Rproj Repo load(file="afro_courts_police.RData"). Its object name is "data."

desc_stats.R features code for descriptive statistics tables and plots.


