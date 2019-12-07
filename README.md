### Applied analytics: machine learning pipeline course

This repository contains the code associated with our research project. The code to conduct the analysis can be found in the following files:


1.) "setup 13-14 data with closure flags.R" - retrieves school data from the NCES CCD, adds closure flags

2.) "merge census data, impute missing values.R" - retrieves census data, merges on Zip = ZCTA, create imputation flags, impute values, other minor cleaning. 

3.) "analysis_SG_Dec6.Rmd" - fit logistic regression model using GLM

4.) "build NN.RMD" - build and train neural net model, generate density charts. 

5.) "compare models.Rmd" - fits decision tree, QDA, random forest. Reads in predictions from 3 and NN model from 4, generates precision - recall curves. 

6.) "build SVM.RMD" - not used in paper, but this code builds and tunes an SVM using paralell CPU processing. While we were not able to use this due to hardware constraints, this could be useful for future extensions. 

Data files: 

"For Modeling.Rdata" - loads two data frames, "with_imputations" and "dropped_nas". The latter is unused since we opted to use the missing indicator strategy to deal with missing data. 


