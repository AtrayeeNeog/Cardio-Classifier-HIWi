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


set.seed(123)
# Loading and Preprocessing the Data:
dt <- readr::read_csv(here::here("Data", "Data.csv"))
head(dt)
ncol(dt)
nrow(dt)
which(dt$pathology == 3)
dt <- dt[-(which(dt$pathology == 3)),]
nrow(dt)
unique(dt$pathology)
which(dt$pathology == "3")
length(colnames(dt))
summary(dt)
#dt$gender <- ifelse(dt$gender == 'f', 1, 0)
str(dt$pathology)



# getting rid of columns with 0 variance i.e columns with identical values
zv <- apply(dt, 2, function(x) length(unique(x)) == 1)
dtr <- dt[, !zv]
str(dtr)
length(colnames(dtr))

#getting rid of all the columns with near zero variance:
to_remove <- nearZeroVar(round(dtr,2))
colnames(dtr[,c(6,150,157)])
dtr <- dtr[, -to_remove]
length(colnames(dtr))

# always keep the following feature(s)
cols_fixed <- c("pathology")
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

dtr$pathology <- factor(dtr$pathology)

intrain <- createDataPartition(y = dtr$pathology, p= 0.7, list = FALSE)
training <- dtr[intrain,]
testing <- dtr[-intrain,]


# Hyperparameters for various Classifier Models:
train_task <- makeClassifTask(data = training, target = "pathology")
test_task <- makeClassifTask(data = testing, target = "pathology")