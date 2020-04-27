library(mlr)
library(rpart)
library(rpart.plot)
library(kernlab)
library(glmnet)
library(ROCR)


# Classifiers:

Decision_Tree <- function(){
  # Deciding which Feature Selection Method to take data from:
  {
    option <- readline((prompt = "Which Feature Selection method would you like to use? \n 1. Correlation-Based FS \n 2. Information Gain \n 3. Chi-Sqaure \n 4. Genetic Forward Algorithm \n 5. Genetic Backward Algorithm \n 6. Random \n 7. No Feature Selection \n"))
    if(option==1){
      data <- readr::read_csv(here::here("Data","Features-With-Correlational-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==2){
      data <- readr::read_csv(here::here("Data","Features-With-InfoGain-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==3){
      data <- readr::read_csv(here::here("Data","Features-With-Chi-Squared-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==4){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialForward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==5){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialBackward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==6){
      data <- readr::read_csv(here::here("Data","Features-With-Random-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==7){
      data <- readr::read_csv(here::here("Data", "Data.csv"))
      data$gender <- ifelse(dt$gender == 'f', 1, 0)
      data$pathology <- factor(ifelse(data$pathology == 1, 1, 0))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    
  }
  ncol(data)
  
  # Check Dimensions:
  dim(training); dim(testing);
  training$pathology <- factor(training$pathology)
  testing$pathology <- factor(testing$pathology)
  
  # Hyperparameters for various Classifier Models:
  train_task <- makeClassifTask(data = training, target = "pathology")
  test_task <- makeClassifTask(data = testing, target = "pathology")
  train_task
  
  # Decision Tree :
  #make tree learner
  makeatree <- makeLearner("classif.rpart", predict.type = "prob")
  
  #Search for hyperparameters
  gs <- makeParamSet(
    makeIntegerParam("minsplit",lower = 10, upper = 50),
    makeIntegerParam("minbucket", lower = 5, upper = 50),
    makeNumericParam("cp", lower = 0.001, upper = 0.2)
  )
  #do a grid search
  gscontrol <- makeTuneControlGrid()
  
  #hypertune the parameters
  stune <- tuneParams(learner = makeatree, resampling = rdesc, task = train_task, par.set = gs, control = gscontrol, measures = acc)
  #check best parameter
  stune$x
  
  #cross validation result
  stune$y
  
  #using hyperparameters for modeling
  t.tree <- setHyperPars(makeatree, par.vals = stune$x)
  
  #train the model
  t.rpart <- mlr::train(t.tree, train_task)
  getLearnerModel(t.rpart)
  t.rpart
  
  
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Accuracy, AUC and Training Time for Train Data: ");print(mlr::performance(tptrain, measures = list(acc, auc, timetrain), model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc, auc), model = t.rpart))
  
}



Random_Forest <- function(){
  # Deciding which Feature Selection Method to take data from:
{
  option <- readline((prompt = "Which Feature Selection method would you like to use? \n 1. Correlation-Based FS \n 2. Information Gain \n 3. Chi-Sqaure \n 4. Genetic Forward Algorithm \n 5. Genetic Backward Algorithm \n 6. Random \n 7. No Feature Selection \n"))
  if(option==1){
    data <- readr::read_csv(here::here("Data","Features-With-Correlational-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==2){
    data <- readr::read_csv(here::here("Data","Features-With-InfoGain-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==3){
    data <- readr::read_csv(here::here("Data","Features-With-Chi-Squared-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==4){
    data <- readr::read_csv(here::here("Data","Features-With-SequentialForward-Algorithm-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==5){
    data <- readr::read_csv(here::here("Data","Features-With-SequentialBackward-Algorithm-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==6){
    data <- readr::read_csv(here::here("Data","Features-With-Random-Algorithm-FS.csv"))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
  if(option==7){
    data <- readr::read_csv(here::here("Data", "Data.csv"))
    data$gender <- ifelse(dt$gender == 'f', 1, 0)
    data$pathology <- factor(ifelse(data$pathology == 1, 1, 0))
    # Data Slicing:
    set.seed(3033)
    intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
    training <- data[intrain,]
    testing <- data[-intrain,]
  }
    
  }
  ncol(data)
  
  # Check Dimensions:
  dim(training); dim(testing);
  training$pathology <- factor(training$pathology)
  testing$pathology <- factor(testing$pathology)
  
  # Hyperparameters for various Classifier Models:
  train_task <- makeClassifTask(data = training, target = "pathology")
  test_task <- makeClassifTask(data = testing, target = "pathology")
  train_task
  
  
  #create a learner
  rf <- makeLearner("classif.randomForest", predict.type = "prob", par.vals = list(ntree = 200, mtry = 3))
  rf$par.vals <- list(importance = TRUE)
  
  #set tunable parameters
  #grid search to find hyperparameters
  rf_param <- makeParamSet(
    makeIntegerParam("ntree",lower = 50, upper = 500),
    makeIntegerParam("mtry", lower = 3, upper = 10),
    makeIntegerParam("nodesize", lower = 10, upper = 50)
  )
  
  #let's do random search for 50 iterations
  rancontrol <- makeTuneControlRandom(maxit = 50L)
  
  #hypertuning
  rf_tune <- tuneParams(learner = rf, resampling = rdesc, task = train_task, par.set = rf_param, control = rancontrol, measures = acc)
  #cv accuracy
  rf_tune$y
  #best parameters
  rf_tune$x
  #using hyperparameters for modeling
  rf.tree <- setHyperPars(rf, par.vals = rf_tune$x)
  #train a model
  rforest <- mlr::train(rf.tree, train_task)
  getLearnerModel(rforest)
  
  #make predictions
  rftrain <- predict(rforest, train_task)
  rfmodel <- predict(rforest, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: ");print(calculateConfusionMatrix(rftrain))
  print("Accuracy, AUC and Training Time for Train Data: ");print(mlr::performance(rftrain, measures = list(acc, auc, timetrain), model = rforest))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(rfmodel, measures = list(acc, auc), model = rforest))
}

SVM <- function(){
  # Deciding which Feature Selection Method to take data from:
  {
    option <- readline((prompt = "Which Feature Selection method would you like to use? \n 1. Correlation-Based FS \n 2. Information Gain \n 3. Chi-Sqaure \n 4. Genetic Forward Algorithm \n 5. Genetic Backward Algorithm \n 6. Random \n 7. No Feature Selection \n"))
    if(option==1){
      data <- readr::read_csv(here::here("Data","Features-With-Correlational-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==2){
      data <- readr::read_csv(here::here("Data","Features-With-InfoGain-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==3){
      data <- readr::read_csv(here::here("Data","Features-With-Chi-Squared-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==4){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialForward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==5){
      data <- readr::read_csv(here:here("Data","Features-With-SequentialBackward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==6){
      data <- readr::read_csv(here::here("Data","Features-With-Random-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==7){
      data <- readr::read_csv(here::here("Data", "Data.csv"))
      data$gender <- ifelse(dt$gender == 'f', 1, 0)
      data$pathology <- factor(ifelse(data$pathology == 1, 1, 0))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
  }
  ncol(data)
  
  # Check Dimensions:
  dim(training); dim(testing);
  training$pathology <- factor(training$pathology)
  testing$pathology <- factor(testing$pathology)
  
  # Hyperparameters for various Classifier Models:
  train_task <- makeClassifTask(data = training, target = "pathology")
  test_task <- makeClassifTask(data = testing, target = "pathology")
  train_task
  
  
  #load svm
  ksvm <- makeLearner("classif.ksvm", predict.type = "prob")
  
  #Set parameters
  pssvm <- makeParamSet(
    makeDiscreteParam("C", values = 2^c(-8,-4,-2,0)), #cost parameters
    makeDiscreteParam("sigma", values = 2^c(-8,-4,0,4)) #RBF Kernel Parameter
  )
  
  #specify search function
  ctrl <- makeTuneControlGrid()
  
  #tune model
  res <- tuneParams(ksvm, task = train_task, resampling = rdesc, par.set = pssvm, control = ctrl,measures = acc)
  
  #CV accuracy
  res$y
  
  #set the model with best params
  t.svm <- setHyperPars(ksvm, par.vals = res$x)
  
  #train
  par.svm <- mlr::train(ksvm, train_task)
  
  #test
  svmtrain <- predict(par.svm, train_task)
  svmtest <- predict(par.svm, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: ");print(calculateConfusionMatrix(svmtest))
  print("Accuracy, AUC and Training Time for Train Data: ");print(mlr::performance(svmtrain, measures = list(acc, auc, timetrain), model = par.svm))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(svmtest, measures = list(acc, auc), model = par.svm))
}

GBM <- function(){
  # Deciding which Feature Selection Method to take data from:
  {
    option <- readline((prompt = "Which Feature Selection method would you like to use? \n 1. Correlation-Based FS \n 2. Information Gain \n 3. Chi-Sqaure \n 4. Genetic Forward Algorithm \n 5. Genetic Backward Algorithm \n 6. Random \n 7. No Feature Selection \n"))
    if(option==1){
      data <- readr::read_csv(here::here("Data","Features-With-Correlational-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==2){
      data <- readr::read_csv(here::here("Data","Features-With-InfoGain-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==3){
      data <- readr::read_csv(here::here("Data","Features-With-Chi-Squared-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==4){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialForward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==5){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialBackward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==6){
      data <- readr::read_csv(here:here("Data","Features-With-Random-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==7){
      data <- readr::read_csv(here::here("Data", "Data.csv"))
      data$gender <- ifelse(dt$gender == 'f', 1, 0)
      data$pathology <- factor(ifelse(data$pathology == 1, 1, 0))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    
  }
  ncol(data)
  
  # Check Dimensions:
  dim(training); dim(testing);
  training$pathology <- factor(training$pathology)
  testing$pathology <- factor(testing$pathology)
  
  # Hyperparameters for various Classifier Models:
  train_task <- makeClassifTask(data = training, target = "pathology")
  test_task <- makeClassifTask(data = testing, target = "pathology")
  train_task
  
  
  #load GBM
  g.gbm <- makeLearner("classif.gbm", predict.type = "prob")
  
  #specify tuning method
  rancontrol <- makeTuneControlRandom(maxit = 1L)
  
  #parameters
  gbm_par<- makeParamSet(
    makeDiscreteParam("distribution", values = "bernoulli"),
    makeIntegerParam("n.trees", lower = 100, upper = 1000), #number of trees
    makeIntegerParam("interaction.depth", lower = 2, upper = 10), #depth of tree
    makeIntegerParam("n.minobsinnode", lower = 1, upper = 10),
    makeNumericParam("shrinkage",lower = -4, upper = -1,
                     trafo = function(x) 10^x),
    makeNumericParam("bag.fraction", lower = 0.2, upper = 0.8)
    
  )
  
  #tune parameters
  tune_gbm <- tuneParams(learner = g.gbm, task = train_task, resampling = rdesc, measures = acc,par.set = gbm_par, control = rancontrol )
  
  #check CV accuracy
  tune_gbm$y
  
  #set parameters
  final_gbm <- setHyperPars(learner = g.gbm, par.vals = tune_gbm$x)
  
  #train
  to.gbm <- mlr::train(final_gbm, train_task)
  
  #test
  gbmtrain <- predict(to.gbm, train_task)
  gbmtest <- predict(to.gbm, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: ");print(calculateConfusionMatrix(gbmtest))
  print("Accuracy, AUC and Training Time for Train Data: ");print(mlr::performance(gbmtrain, measures = list(acc, auc, timetrain), model = to.gbm))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(gbmtest, measures = list(acc, auc), model = to.gbm))
}

Lasso <- function(){
  # Deciding which Feature Selection Method to take data from:
  {
    option <- readline((prompt = "Which Feature Selection method would you like to use? \n 1. Correlation-Based FS \n 2. Information Gain \n 3. Chi-Sqaure \n 4. Genetic Forward Algorithm \n 5. Genetic Backward Algorithm \n 6. Random \n 7. No Feature Selection \n"))
    if(option==1){
      data <- readr::read_csv(here::here("Data","Features-With-Correlational-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==2){
      data <- readr::read_csv(here::here("Data","Features-With-InfoGain-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==3){
      data <- readr::read_csv(here::here("Data","Features-With-Chi-Squared-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==4){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialForward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==5){
      data <- readr::read_csv(here::here("Data","Features-With-SequentialBackward-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==6){
      data <- readr::read_csv(here::here("Data","Features-With-Random-Algorithm-FS.csv"))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
    if(option==7){
      data <- readr::read_csv(here::here("Data", "Data.csv"))
      data$gender <- ifelse(dt$gender == 'f', 1, 0)
      data$pathology <- factor(ifelse(data$pathology == 1, 1, 0))
      # Data Slicing:
      set.seed(3033)
      intrain <- createDataPartition(y = data$pathology, p= 0.7, list = FALSE)
      training <- data[intrain,]
      testing <- data[-intrain,]
    }
  }
  ncol(data)
  
  # Check Dimensions:
  dim(training); dim(testing);
  training$pathology <- factor(training$pathology)
  testing$pathology <- factor(testing$pathology)
  # Hyperparameters for various Classifier Models:
  train_task <- makeClassifTask(data = training, target = "pathology")
  test_task <- makeClassifTask(data = testing, target = "pathology")
  train_task
  
  
  train_cont <- trainControl(method = "repeatedcv",
                                 number = 10,
                                 repeats = 5,
                                 search = "random",
                                 verboseIter = TRUE)
  
  dummies <- dummyVars(pathology ~ ., data = data)
  
  train_dummies = predict(dummies, newdata = training)
  
  test_dummies = predict(dummies, newdata = testing)
  
  print(dim(train_dummies)); print(dim(test_dummies))
  
  
  
  x = as.matrix(train_dummies)
  y_train = training$pathology
  
  x_test = as.matrix(test_dummies)
  y_test = testing$pathology
  
  
  lambdas <- 10^seq(2, -3, by = -.1)
  
  time <- system.time(lasso <- glmnet(x, y_train, nlambda = 25, alpha = 1, family = 'binomial', lambda = lambdas, trctrl = train_cont))
  lasso_cv <- cv.glmnet(x, y_train, alpha = 1,family = "binomial", lambda = lambdas, standardize = TRUE, nfolds = 5)
  
  # Best 
  lambda_best <- lasso_cv$lambda.min 
  lambda_best
  
  # Predictions:
  predictions_train <- predict(lasso, s = lambda_best, newx = x)
  predictions_train <- ifelse(predictions_train <= 0.5, 0, 1)
  predictions_train
  predictions_test <- predict(lasso, s = lambda_best, newx = x_test)
  predictions_test <- ifelse(predictions_test <= 0.5, 0, 1)
  
  # Performance
  # Accuracy:
  acc_train <- signif(mean(predictions_train == y_train ), digits = 3)
  acc_test <- signif(mean(predictions_test == y_test), digits = 3)
  
  print(paste0("Training Accuracy: " , acc_train))
  print(paste0("Testing Accuracy: " , acc_test))
  
  # Auc :
  pred_train <- prediction(predictions_train, y_train)
  pred_test <- prediction(predictions_test, y_test)
  auc_train <- signif(attr(ROCR::performance(pred_train, "auc"), "y.values")[[1]], digits = 3)
  auc_test <- signif(attr(ROCR::performance(pred_test, "auc"), "y.values")[[1]], digits = 3)
  
  print(paste0("AUC for Training: ",auc_train ))
  print(paste0("AUC for Testing: ",auc_test ))
  
  # Training Time:
  print("Training Time: "); print(time)
  
}








