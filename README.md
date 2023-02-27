# Party cues or Policy Information?

This repository contains the replication material for “Party Cues or Policy Information? The Differential Influence of Financial and Economic Literacy on Economic Policy Preferences” published in the Journal of Public Policy. Below, I describe each of the files in this repository.

## Build/Clean Data 

- run multiple_imp.R to clean the original datasets (“survey_experiment_final4.csv” and “CB_survey_final.csv”) and do multiple imputation (the multiply imputed datasets have been saved under “imputations_it2.RData”). Create final variables to be used in the models. 

## Models 

All models are run on the multiply imputed imputations_it2.RData.
To obtain tables 1, 2 and 3 and figures 3, 4, 5, 6, 7 and 8 run in this order (keep in mind that to obtain quantities of interest we run several simulations, hence the final plotted estimates will slightly differ from time to time):
- run multiple_imp_no_matching.R 
- run multiple_imp_matching.R
- run education_robust.R (table 3)
- run plots_EV_FD.R (figures 3 to 8)
- run reg_tables.R (tables 1 and 2)

To obtain figure 1:
- run theorey_survey_new.R

## Appendix

For materials in the online appendix:
- run plots_price_controls.R  (to obtain figures A1, A2, A3)
- run appendix.R (to obtain tables B1 through B5 and regression tables C1 through C3)
