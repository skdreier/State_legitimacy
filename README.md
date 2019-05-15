# State Legitimacy
This repository contains the replication material for the article: "Institutional Legitimacy in sub-Saharan Africa," which is forthcoming in *Democratization* (2019). 

## Authors
Sarah K. Dreier and Milli Lake.

## Recode and prepare data

The original dataset [merged_r6_data_2016_36countries2.sav](http://afrobarometer.org/sites/default/files/data/round-6/merged_r6_data_2016_36countries2.sav) contains the merged Afrobarometer Round 6 (2016) survey data. It is available to be downloaded from the [Afrobarometer website](http://afrobarometer.org).

[variable_recode.R](source/variable_recode.R) recodes variables from the source Afrobarometer data for the purposes of this analysis. This script also subsets the recoded variables into the new dataset.

## Main analysis

[main_analysis.R](source/main_analysis.R) runs the models and simulation . plots featured in the article's main analysis. The recoded, subseted used in this script, [afro_courts_police.RData](afro_courts_police.RData), can be loaded from this repository. Its object name is "data." 

[binary.R](source/ R) simulates results from the article's main binary-IV models. 

All resulting [figures](figures/) are also posted in the repository. 

## Descriptive statistics and robustness checks

Original, messy scripts for descriptive statistics tables and figures () and for additional robustness-check models () are available in the xx folder.

## Questions and contact information

All questions may be directed to Sarah Dreier, who is the article's first author and repository ownder (skdreier at uw dot edu).

