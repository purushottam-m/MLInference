
# Homework

#--------------------------------------------------------------------------------------------------------------------
# This program obtains empirical results for the paper "Generic Machine Learning Discovery 
# and Classification Analysis of Heterogenous Treatment Effects in Randomized Experiments"
# by V. CHERNOZHUKOV, M. DEMIRER, E. DUFLO, I. FERNANDEZ-VAL
#--------------------------------------------------------------------------------------------------------------------

# Authors: V. CHERNOZHUKOV, M. DEMIRER, E. DUFLO, I. FERNANDEZ-VAL

# This program returns three outputs
# 1) A plot for each outcome variable reporting ATE by groups
# 2) het_results: Latex table reporting ATE and heterogeneity loading coefficient 
# 3) test_results: A latex table reporting  estimated ATE_group1 - ATE_group5 and its p-value
# 4) best: A latex table reporting best ML methods
# 5) MSE: A latex table of mean squared errors from tuning


# more changes

# some more changes

rm(list=ls(all=TRUE))
vec.pac= c("foreign", "quantreg", "gbm", "glmnet",
           "MASS", "rpart", "doParallel", "sandwich", "randomForest",
           "nnet", "matrixStats", "xtable", "readstata13", "car", "lfe", "doParallel",
           "caret", "foreach", "multcomp","cowplot")

# more changes 

### 


This is a change made by P



