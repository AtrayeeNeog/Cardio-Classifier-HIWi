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
  training$pathology <- ifelse(training$pathology == "1",1,0)
  testing$pathology <- ifelse(testing$pathology == "1",1,0)
  # Correlation Based Feature Selection:
  results <- select.cfs(training)
  features <- results$Biomarker
  
  train <- training[, features]
  train$pathology <- factor(training$pathology)
  test <- testing[,features]
  test$pathology <- factor(testing$pathology)
  
  
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  model <- caret::train(pathology~., data=train, method="rpart", preProcess="scale", trControl=control, metric = "Kappa")
  
  tptrain <- predict(model, newdata = train)
  tpmodel <- predict(model, newdata = test)
  
  confusionMatrix(data = tpmodel, reference = test$pathology)
  
  
  
  predictions_train <- predict(model, newdata = train)
  predictions_train <- ifelse(predictions_train == "1",1,0)
  predictions_train
  predictions_test <- predict(model, newdata = test)
  predictions_test <- ifelse(predictions_test == "1",1,0)
  predictions_test
  testing$pathology
  
  # Performance
  cm <- confusionMatrix(as.factor(predictions_test), reference = as.factor(testing$pathology))
  # Accuracy:
  print(paste0("Testing Accuracy: " , cm$overall["Accuracy"]))
  
  # Auc :
  pred_test <- prediction(predictions_test, testing$pathology)
  auc_test <- signif(attr(ROCR::performance(pred_test, "auc"), "y.values")[[1]], digits = 3)
  print(paste0("AUC for Testing: ",auc_test ))
  
  # Kappa:
  print(paste0("Kappa value for Testing: ", cm$overall["Kappa"] ))
}




ChiSquare <-function(){
  set.seed(123)
  model <- makeLearner("classif.rpart", predict.type = "prob")
  lrn = makeFilterWrapper(learner = model, fw.method = "FSelector_chi.squared")
  ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))
  rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)
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
  model <- makeLearner("classif.rpart", predict.type = "prob")
  lrn = makeFilterWrapper(learner = model, fw.method = "FSelectorRcpp_information.gain")
  ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1),
                    makeIntegerParam("minsplit",lower = 2, upper = 10),
                    makeNumericParam("cp", lower = 0.01, upper = 0.05))
  rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)
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
  model <- makeLearner("classif.rpart", predict.type = "prob")
  kappa_sd <- setAggregation(kappa,test.sd)
  rdesc <-  makeResampleDesc("CV", iters = 10, stratify = TRUE)
  lrn <-  makeFeatSelWrapper(model, resampling = rdesc, measures = list(mlr::kappa,kappa_sd),
                             control =  makeFeatSelControlSequential(method = "sfs", alpha = 0.1), show.info = TRUE)
  
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
  
  rpart.plot(getLearnerModel(t.rpart,more.unwrap = TRUE))
}

SequentialBackward <-function(){
  
  set.seed(123)
  model <- makeLearner("classif.rpart", predict.type = "prob")
  kappa_sd <- setAggregation(kappa,test.sd)
  rdesc <-  makeResampleDesc("CV", iters = 10, stratify = TRUE)
  lrn <-  makeFeatSelWrapper(model, resampling = rdesc, measures = list(mlr::kappa,kappa_sd),
                             control =  makeFeatSelControlSequential(method = "sbs", beta = -0.001), show.info = TRUE)
  
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
  model <- makeLearner("classif.rpart", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,measures = list(mlr::kappa,kappa_sd),
                           control =  makeFeatSelControlGA(maxit = 10L, mu =10L ), show.info = TRUE)
  
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
  model <- makeLearner("classif.rpart", predict.type = "prob")
  rdesc = makeResampleDesc("CV", iters = 10, stratify = TRUE)
  lrn = makeFeatSelWrapper(model, resampling = rdesc,
                           control =  makeFeatSelControlRandom(maxit = 200L), show.info = TRUE, measures = kappa)
  
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

