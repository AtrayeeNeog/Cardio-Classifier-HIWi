# Requirements : Java 11; Set environment variables

library(mlr)
library(caret)
library(ggplot2)
library(rJava)
library(RWeka)
library("FSelector")
library(FSelectorRcpp)
library(Biocomb)

Chi-Squared <- function(){
  
  set.seed(123)
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
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, getFilteredFeatures(mod_dt)]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Chi-Squared-FS.csv", row.names = FALSE)
  
  
}
Info_Gain <- function(){
  
  set.seed(123)
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
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, getFilteredFeatures(mod_dt)]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-InfoGain-FS.csv", row.names = FALSE)
  
}

Sequential_Forward <- function(){
  
  set.seed(123)
  lrn_dt = makeFeatSelWrapper("classif.svm", resampling = rdesc,
                              control = makeFeatSelControlSequential(method = "sfs", alpha = 0.002, maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  sfeats$x
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),]
  sfeats$x
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- (dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == "1", "healthy", "pathologic")
  str(new_data$pathology)
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-SequentialForward-Algorithm-FS.csv", row.names = FALSE)
  
}  

Sequential_Backward <- function(){
  
  set.seed(123)
  lrn_dt = makeFeatSelWrapper("classif.ksvm", resampling = rdesc,
                              control = makeFeatSelControlSequential(method = "sbs", maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  sfeats$x
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-SequentialBackward-Algorithm-FS.csv", row.names = FALSE)
  
  
} 

Genetic <- function(){
  
  set.seed(123)
  lrn_dt = makeFeatSelWrapper("classif.randomForest", resampling = rdesc,
                           control = makeFeatSelControlGA(maxit = 10), show.info = TRUE)
  mod_dt = mlr::train(lrn_dt, task = dt_task)
  mod_dt
  
  # Result of feature selection:
  sfeats = getFeatSelResult(mod_dt)
  sfeats$x
  sfeats$y
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-GeneticAlgorithm.csv", row.names = FALSE)
}

Random <- function(){
  
  set.seed(123)
  lrn_dt = makeFeatSelWrapper("classif.randomForest", resampling = rdesc,
                              control = makeFeatSelControlRandom(maxit = 10))
  mod_dt = mlr::train(lrn_dt, dt_task)
  mod_dt
  sfeats<-getFeatSelResult(mod_dt)
  
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, sfeats$x]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Random-Algorithm-FS.csv", row.names = FALSE)
  
  
}


CorFS <- function(){
  
  set.seed(123)
  df <- data.frame(dtr)
  str(df$pathology)
  unique(df$pathology)
  df$pathology <- ifelse(df$pathology == '1', 1,0)
  ncol(df)
  nrow(df)
  colnames(df)
  # Applying Correlation (to find the corr coeff for each feature except label):
  n=length(colnames(df))
  correlationMatrix <- cor(df[, 1:n-1], use="pairwise.complete.obs")
  
  # checking for NaNs:
  sapply(correlationMatrix, function(x)all(any(is.na(x))))
  sum(is.na(correlationMatrix))
  print(correlationMatrix)
  
  # Correlation Based Feature Selection:
  results <- select.cfs(df)
  features <- results$Biomarker
  
  
  # Saving the most important features:
  new_data<- data.frame(readr::read_csv(here::here("Data", "Data.csv")))
  ncol(new_data)
  nrow(new_data)
  unique(new_data$pathology)
  new_data <- new_data[-(which(new_data$pathology == 3)),] 
  new_data <- new_data[, features]
  new_data$pathology <- factor(dt$pathology)
  unique(dt$pathology)
  new_data$pathology <- ifelse(new_data$pathology == 1, "healthy", "pathologic")
  new_data$pathology <- factor(new_data$pathology)
  colnames(new_data)
  unique(new_data$pathology)
  ncol(new_data)
  write.csv(new_data, file= "Data/Features-With-Correlational-FS.csv", row.names = FALSE)
  
  
}








  