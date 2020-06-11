library(mlr)
library(caret)
library(here)
library(ggplot2)
library(GGally)
library(tidyverse)

# age	systolicMaxVortexVolume	diastolicMaxLeftRotationVolumeRel	diastolicMedianRightRotationVolumeRel


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
dt_old<- dt[(which(dt$age > 47 & dt$pathology == "1")),]
dt_bav <- dt[(which(dt$pathology == "2")),]
dt <- rbind(dt_old, dt_bav)
nrow(dt) #52


best_features<- subset(dt, select = c(age,systolicMaxVortexVolume,diastolicMaxLeftRotationVolumeRel,diastolicMedianRightRotationVolumeRel,pathology))
best_features$pathology <- ifelse(best_features$pathology == 1, "Healthy", "BAV")



# Histograms:
ggplot(best_features, aes(x=systolicMaxVortexVolume, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="systolicMaxVortexVolume v/s Number of Cases",
                                                                    x="systolicMaxVortexVolume", y = "Count")

ggplot(best_features, aes(x=diastolicMaxLeftRotationVolumeRel, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="diastolicMaxLeftRotationVolumeRel v/s Number of Cases",
                                                                    x="diastolicMaxLeftRotationVolumeRel", y = "Count")

ggplot(best_features, aes(x=diastolicMedianRightRotationVolumeRel, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="diastolicMedianRightRotationVolumeRel v/s Number of Cases",
                                                                    x="diastolicMedianRightRotationVolumeRel", y = "Count")
ggplot(best_features, aes(x=age, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="age v/s Number of Cases",
                                                                    x="age", y = "Count")

# Scatterplot:
ggplot(best_features, aes(x=systolicMaxVortexVolume, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="systolicMaxVortexVolume v/s Pathology",
                                    x="systolicMaxVortexVolume", y = "Pathology")

ggplot(best_features, aes(x=diastolicMaxLeftRotationVolumeRel, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="diastolicMaxLeftRotationVolumeRel v/s Pathology",
                                    x="diastolicMaxLeftRotationVolumeRel", y = "Pathology")

ggplot(best_features, aes(x=diastolicMedianRightRotationVolumeRel, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="diastolicMedianRightRotationVolumeRel v/s Pathology",
                                    x="diastolicMedianRightRotationVolumeRel", y = "Pathology")
ggplot(best_features, aes(x=age, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="age v/s Pathology",
                                    x="age", y = "Pathology")

# Scatterplot Matrix:
colnames(best_features)
ggpairs(best_features, columns=1:4, aes(color=pathology)) + 
  ggtitle("ScatterPlot Matrix for Four Best Features for Healthy Old Patients")





