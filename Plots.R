library(mlr)
library(caret)
library(here)
library(ggplot2)
library(GGally)
library(tidyverse)

# maxVortexVolumeRel	diastolicMedianMeanAxialVelocity	systolicMeanLeftRotationVolume	diastolicMaxLeftRotationVolumeRel	pathology
 

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


best_features <- subset(dt, select = c(maxVortexVolumeRel,diastolicMedianMeanAxialVelocity,systolicMeanLeftRotationVolume,diastolicMaxLeftRotationVolumeRel,pathology))
best_features$pathology <- ifelse(best_features$pathology == 1, "Healthy", "BAV")
nrow(best_features)
ncol(best_features)
healthy <- best_features %>% filter(best_features$pathology == "Healthy")
nrow(healthy)


# Histograms:
ggplot(best_features, aes(x=maxVortexVolumeRel, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="Max Vortex Volume Rel v/s Number of Cases",
                                                                    x="Max Vortex Volume Rel", y = "Count")
ggplot(best_features, aes(x=diastolicMedianMeanAxialVelocity, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="Diastolic Median Mean Axial Velocity v/s Number of Cases",
                                                                    x="Diastolic Median Mean Axial Velocity", y = "Count")
ggplot(best_features, aes(x=systolicMeanLeftRotationVolume, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="Systolic Mean Left Rotation Volume v/s Number of Cases",
                                                                    x="Systolic Mean Left Rotation Volume", y = "Count")
ggplot(best_features, aes(x=diastolicMaxLeftRotationVolumeRel, color = pathology)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="Diastolic Max Left Rotation Volume Rel v/s Number of Cases",
                                                                    x="Diastolic Max Left Rotation Volume Rel", y = "Count")

# Scatterplot:
ggplot(best_features, aes(x=maxVortexVolumeRel, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="Max Vortex Volume Rel v/s Pathology",
                                    x="Max Vortex Volume Rel", y = "Pathology")
ggplot(best_features, aes(x=diastolicMedianMeanAxialVelocity, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="Diastolic Median Mean Axial Velocity v/s Pathology",
                                     x="Diastolic Median Mean Axial Velocity", y = "Pathology")
ggplot(best_features, aes(x=systolicMeanLeftRotationVolume, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="Systolic Mean Left Rotation Volume v/s Pathology",
                                    x="Systolic Mean Left Rotation Volume", y = "Pathology")
ggplot(best_features, aes(x=diastolicMaxLeftRotationVolumeRel, y=pathology, color = pathology)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="Diastolic Max Left Rotation Volume Rel v/s Pathology",
                                    x="Diastolic Max Left Rotation Volume Rel", y = "Pathology")

sum(best_features$pathology == "Healthy") #90
sum(best_features$pathology == "BAV") #22

# Scatterplot Matrix:
ncol(best_features)
ggpairs(best_features, columns=1:(length(best_features)-1), aes(color=pathology)) + 
  ggtitle("ScatterPlot Matrix for Four Best Features")
