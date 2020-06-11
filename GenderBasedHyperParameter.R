library(mlr)
library(caret)
library(here)
library(tidyverse)

# Loading and Preprocessing the Data:
dt <- readr::read_csv(here::here("Data", "Data.csv"))
head(dt)
ncol(dt)
nrow(dt)
str(dt$pathology)
unique(dt$pathology)
dt <- dt %>% filter(dt$pathology == 1)
str(dt$gender)
dt$gender <- factor(dt$gender)
colnames(dt)
summary(dt)
dt <- subset(dt, select = -(weight))

# Remove rows with ages>=65:
length(which(dt$age >= 60 & dt$pathology == 1)) # 8
length(which(dt$age >= 45 & dt$pathology == 1)) # 37
length(which(dt$age >= 50 & dt$pathology == 1)) # 23
length(which(dt$age >= 55 & dt$pathology == 1)) # 13
length(which(dt$age >= 45)) # 44
length(which(dt$age >= 50)) # 27
length(which(dt$age >= 55)) # 16


# getting rid of columns with 0 variance i.e columns with identical values
zv <- apply(dt, 2, function(x) length(unique(x)) == 1)
dtr <- dt[, !zv]
str(dtr)
colnames(dtr)
n=length(dtr)
# always keep the following feature(s)
cols_fixed <- c("gender")
# apply the correlation feature selection on the following features
cols <- setdiff(names(dtr), cols_fixed)
correlationMatrix <- cor(dtr[cols], use = "pairwise.complete.obs")

# checking for NaNs:
sapply(correlationMatrix, function(x)all(any(is.na(x))))
sum(is.na(correlationMatrix))
print(correlationMatrix)

# Find highly correlated attributes (ideally>0.75):
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff = 0.90)
print(highlyCorrelated)
length(highlyCorrelated)

# Delete the highly correlated feature from dataset:
cols_to_remove <- cols[highlyCorrelated]
# works only with dplyr >= 1.0
dtr <- dtr %>% select(-all_of(cols_to_remove))
# works also with dplyr < 1.0
# dtr <- dtr %>% select(-cols_to_remove)
length(dtr)
colnames(dtr)

# Make Classifier, resampling param, control param and Learner:
dt_task <- makeClassifTask(data = dtr, target = "gender")
rdesc = makeResampleDesc("CV", iters = 10)
train_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))

cols <- data.frame(dt[highlyCorrelated])
which(colnames(cols)=="meanDiameter")
