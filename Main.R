library(FSelector)

source("Hyperparameters.R")
source("FeatureSelection.R")
source("Models.R")

writeLines("Which Classifier Model do you want to run? \n 
           1. Decision_Tree()\n
           2. Random_Forest()\n
           3. SVM()\n
           4. GBM()\n
           5. Lasso()\n")
