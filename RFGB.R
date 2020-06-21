library(mlr)
library(caret)
library(ggplot2)
library(rJava)
library(RWeka)
library("FSelector")
library(FSelectorRcpp)
library(Biocomb)
library(mlr)
library(rpart)
library(rpart.plot)
library(kernlab)
library(glmnet)
library(ROCR)
library(mlbench)



Correlation <- function(){
  
  set.seed(123)
  str(training$gender)
  training$gender <- ifelse(training$gender == "1",1,2)
  testing$gender <- ifelse(testing$gender == "1",1,2)
  # Correlation Based Feature Selection:
  results <- select.cfs(training)
  features <- results$Biomarker
  
  train <- training[, features]
  train$gender <- factor(training$gender)
  test <- testing[,features]
  test$gender <- factor(testing$gender)
  
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  model <- caret::train(gender~., data=train, method="rf", preProcess="scale", trControl=control, metric = "Kappa")
  
  tptrain <- predict(model, newdata = train)
  tpmodel <- predict(model, newdata = test)
  
  confusionMatrix(data = tpmodel, reference = test$gender)
  
  
  
  predictions_train <- predict(model, newdata = train)
  predictions_train <- ifelse(predictions_train == "1",1,2)
  predictions_train
  predictions_test <- predict(model, newdata = test)
  predictions_test <- ifelse(predictions_test == "1",1,2)
  predictions_test
  testing$gender
  
  # Performance
  cm <- confusionMatrix(as.factor(predictions_test), reference = as.factor(testing$gender))
  # Accuracy:
  print(paste0("Testing Accuracy: " , cm$overall["Accuracy"]))
  
  # Auc :
  pred_test <- prediction(predictions_test, testing$gender)
  auc_test <- signif(attr(ROCR::performance(pred_test, "auc"), "y.values")[[1]], digits = 3)
  print(paste0("AUC for Testing: ",auc_test ))
  
  # Kappa:
  print(paste0("Kappa value for Testing: ", cm$overall["Kappa"] ))
}




ChiSquare <-function(){
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  lrn = makeFilterWrapper(learner = model, fw.method = "FSelector_chi.squared")
  ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))
  rdesc = makeResampleDesc("CV", iters = 10)
  res = tuneParams(lrn, task = train_task, resampling = rdesc, par.set = ps, measures = kappa,
                   control = makeTuneControlGrid())
  
  t.tree <- makeFilterWrapper(learner = model, fw.method = "FSelector_chi.squared",
                              fw.perc = res$x$fw.perc, C = res$x$C, sigma = res$x$sigma)
  
  #train the model
  t.rpart <- mlr::train(t.tree, train_task)
  getLearnerModel(t.rpart)
  t.rpart
  
  length(getFilteredFeatures(t.rpart))
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}


InfoGain <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  lrn = makeFilterWrapper(learner = model, fw.method = "FSelectorRcpp_information.gain")
  ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))
  rdesc = makeResampleDesc("CV", iters = 10)
  res = tuneParams(lrn, task = train_task, resampling = rdesc, par.set = ps, measures = kappa,
                   control = makeTuneControlGrid())
  
  t.tree <- makeFilterWrapper(learner = model, fw.method = "FSelectorRcpp_information.gain",
                              fw.perc = res$x$fw.perc)
  
  #train the model
  t.rpart <- mlr::train(t.tree, train_task)
  getLearnerModel(t.rpart)
  t.rpart
  
  length(getFilteredFeatures(t.rpart))
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}

SequentialForward <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,
                           control =  makeFeatSelControlSequential(method = "sfs", alpha = 0.02), show.info = TRUE)
  
  #train the model
  t.rpart <- mlr::train(lrn, train_task)
  sfeats <- getFeatSelResult(t.rpart)
  t.rpart
  
  length(sfeats$x)
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}

SequentialBackward <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,
                           control =  makeFeatSelControlSequential(method = "sbs", alpha = 0.02), show.info = TRUE)
  
  #train the model
  t.rpart <- mlr::train(lrn, train_task)
  sfeats <- getFeatSelResult(t.rpart)
  t.rpart
  
  length(sfeats$x)
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}

Genetic <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,
                           control =  makeFeatSelControlGA(maxit = 10), show.info = TRUE, measures = kappa)
  
  #train the model
  t.rpart <- mlr::train(lrn, train_task)
  sfeats <- getFeatSelResult(t.rpart)
  t.rpart
  
  length(sfeats$x)
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}

Random <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.randomForest", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,
                           control =  makeFeatSelControlRandom(maxit = 10), show.info = TRUE, measures = kappa)
  
  #train the model
  t.rpart <- mlr::train(lrn, train_task)
  sfeats <- getFeatSelResult(t.rpart)
  t.rpart
  
  length(sfeats$x)
  
  #make predictions
  tptrain <- predict(t.rpart, train_task)
  tpmodel <- predict(t.rpart, test_task)
  
  #Performance Measures:
  print("Confusion Matrix for Test Data: "); print(calculateConfusionMatrix(tpmodel))
  print("Training Time for Train Data: ");print(mlr::performance(tptrain, measures = timetrain, model = t.rpart))
  print("Accuracy, AUC for Test Data: ");print(mlr::performance(tpmodel, measures = list(acc,auc,kappa), model = t.rpart))
}

