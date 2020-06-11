library(mlr)
library(caret)
library(here)
set.seed(123)
# Loading and Preprocessing the Data:
dt <- readr::read_csv(here::here("Data", "Data.csv"))
head(dt)
ncol(dt)
nrow(dt)
dt$pathology <- factor(dt$pathology)
str(dt$pathology)
which(dt$pathology == "3")
dt <- dt[-(which(dt$pathology == "3")),]
nrow(dt)
unique(dt$pathology)
which(dt$pathology == "3")
colnames(dt)
summary(dt)
#dt$gender <- ifelse(dt$gender == 'f', 1, 0)
str(dt$pathology)

# Remove rows with ages>=65:
length(which(dt$age >= 60 & dt$pathology == "1")) # 8
length(which(dt$age >= 45 & dt$pathology == "1")) # 37
length(which(dt$age >= 50 & dt$pathology == "1")) # 23
length(which(dt$age >= 55 & dt$pathology == "1")) # 13
length(which(dt$age >= 45)) # 44
length(which(dt$age >= 50)) # 27
length(which(dt$age >= 55)) # 16
length(which(dt$age >= 60))

length(which(dt$age >= 45 & dt$pathology == "1")) #37

dt_old <- dt[(which(dt$age >= 45 & dt$pathology == "1")),]
dt_young <- dt[-(which(dt$age >= 45 & dt$pathology == "1")),]
nrow(dt_old) #37
nrow(dt_young) #75


# getting rid of columns with 0 variance i.e columns with identical values
zv <- apply(dt, 2, function(x) length(unique(x)) == 1)
dtr <- dt[, !zv]
str(dtr)

n=length(dtr)
correlationMatrix <- cor(dtr[,1:n-1], use="pairwise.complete.obs")

# checking for NaNs:
sapply(correlationMatrix, function(x)all(any(is.na(x))))
sum(is.na(correlationMatrix))
print(correlationMatrix)

# Find highly correlated attributes (ideally>0.75):
highlyCorrelated <- caret::findCorrelation(correlationMatrix, cutoff = 0.90)
print(highlyCorrelated)
length(highlyCorrelated)

# Delete the highly correlated feature from dataset:
dtr <- dtr[, -c(highlyCorrelated)]
length(dtr)
colnames(dtr)



# Make Classifier, resampling param, control param and Learner:
dt_task <- makeClassifTask(data = dtr, target = "pathology")
rdesc = makeResampleDesc("CV", iters = 10)
train_ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
lrn <- makeLearner("classif.randomForest", predict.type = "prob")
ps = makeParamSet(makeNumericParam("fw.perc", lower = 0, upper = 1))



