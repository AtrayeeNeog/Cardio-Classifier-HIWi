library(mlr)
library(caret)
library(here)

# Loading and Preprocessing the Data:
dt <- readr::read_csv(here::here("Data", "Data.csv"))
head(dt)

dt$pathology <- ifelse(dt$pathology == 1, 1, 0)
dt$pathology <- factor(dt$pathology)
str(dt$pathology)
unique(dt$pathology)
colnames(dt)
summary(dt)
#dt$gender <- ifelse(dt$gender == 'f', 1, 0)

# getting rid of columns with 0 variance i.e columns with identical values
zv <- apply(dt, 2, function(x) length(unique(x)) == 1)
dtr <- dt[, !zv]
str(dtr)


# Make Classifier, resampling param, control param and Learner:
dt_task <- makeClassifTask(data = dtr, target = "pathology")
rdesc = makeResampleDesc("CV", iters = 10)
train_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))




