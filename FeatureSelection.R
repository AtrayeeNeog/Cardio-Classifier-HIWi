# Requirements : Java 11; Set environment variables

library(mlr)
library(caret)
library(ggplot2)
library(rJava)
library(RWeka)
library("FSelector")
library(FSelectorRcpp)



Chi-Squared <- function(){
  
  lrn_tuned = makeFilterWrapper(learner = "classif.randomForest", fw.method = "FSelector_chi.squared")
  res = tuneParams(lrn_tuned, task = dt_task, resampling = rdesc, par.set = ps,
                   control = makeTuneControlRandom(maxit = 10))
  lrn_dt = makeFilterWrapper(learner = "classif.randomForest", fw.method = "FSelector_chi.squared",
                             fw.perc = res$x$fw.perc)
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  getFilteredFeatures(mod_dt)
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  new_data$pathology
  new_data <- new_data[, getFilteredFeatures(mod_dt)]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Chi-Squared-FS.csv", row.names = FALSE)
  return(new_data)
  
}

Info_Gain <- function(){
  
  lrn_tuned = makeFilterWrapper(learner = "classif.randomForest", fw.method = "FSelectorRcpp_information.gain")
  res = tuneParams(lrn_tuned, task = dt_task, resampling = rdesc, par.set = ps,
                   control = makeTuneControlRandom(maxit = 10))
  lrn_dt = makeFilterWrapper(learner = "classif.randomForest", fw.method = "FSelectorRcpp_information.gain",
                             fw.perc = res$x$fw.perc)
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  getFilteredFeatures(mod_dt)
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  new_data$pathology
  new_data <- new_data[, getFilteredFeatures(mod_dt)]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-InfoGain-FS.csv", row.names = FALSE)
  return(new_data)
  
}
  
Sequential_Forward <- function(){
  
  lrn_dt = makeFeatSelWrapper("classif.randomForest", resampling = rdesc,
                              control = makeFeatSelControlSequential(method = "sfs", alpha = 0.01, maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  sfeats$x
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  str(new_data$pathology)
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  str(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-SequentialForward-Algorithm-FS.csv", row.names = FALSE)
  return(new_data)
  
}  
  
Sequential_Backward <- function(){
  
  lrn_dt = makeFeatSelWrapper("classif.randomForest", resampling = rdesc,
                              control = makeFeatSelControlSequential(method = "sbs", alpha = 0.005, maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  sfeats$x
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  new_data$pathology
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-SequentialBackward-Algorithm-FS.csv", row.names = FALSE)
  return(new_data)
  
  
}  
  
Random <- function(){
  
  lrn_dt = makeFeatSelWrapper("classif.randomForest", resampling = rdesc,
                           control = makeFeatSelControlRandom(maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  sfeats$x
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  new_data$pathology
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Random-Algorithm-FS.csv", row.names = FALSE)
  return(new_data)
  

}


CorFS <- function(){
  df <- data.frame(dtr)
  #df$pathology <- ifelse(df$pathology == 'pathologic', 1,0)
  colnames(df)
  # Applying Correlation (to find the corr coeff for each feature except label):
  n=length(colnames(df))
  correlationMatrix <- cor(df[, 1:n-1], use="pairwise.complete.obs")
  
  # checking for NaNs:
  sapply(correlationMatrix, function(x)all(any(is.na(x))))
  sum(is.na(correlationMatrix))
  print(correlationMatrix)
  
  # Find highly correlated attributes (ideally>0.75):
  highlyCorrelated <- findCorrelation(correlationMatrix, cutoff = 0.97)
  print(highlyCorrelated)
  length(highlyCorrelated)
  
  # Delete the highly correlated feature from dataset:
  df <- df[-c(highlyCorrelated)]
  length(colnames(df))
  length(df)
  str(df)
  df[,ncol(df)]
  unique(df$pathology)
  
  # Correlation based Feature Selection (using Recursive Feature Elimination from caret):
  # define the control using Random Forest Selection Function:
  control <- rfeControl(functions = rfFuncs, method = "cv", number = 10)
  # run the RFE algorithm
  results <- rfe(x= df[,1:length(df)-1], y = as.factor(df$pathology), sizes=c(1:(length(df)-1)), rfeControl=control)
  # summarize the results
  print(results)
  # list the chosen features
  features<- predictors(results)
  features
  # plot the results
  plot(results, type=c("g", "o"))
  
  # Creating new dataset with selected features: 
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  new_data$pathology
  new_data <- new_data[, features]
  new_data$pathology <- dt$pathology
  new_data$pathology <- ifelse(new_data$pathology == 1, 1, 0)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Correlational-FS.csv", row.names = FALSE)
  return(new_data)
  

}





  