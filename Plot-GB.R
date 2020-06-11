# meanDiameter	medianCrossSectionalArea	maxMeanVelocity	maxOverallVelocityQ99TimeS	systolicMaxMeancircumferentialVelocity
 

library(mlr)
library(caret)
library(here)
library(ggplot2)
library(GGally)

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
dt <- dt[(which(dt$pathology == "1")),]
dt$gender <- factor(dt$gender)
str(dt$gender)
colnames(dt)
summary(dt)
str(dt$gender)
male <- dt %>% filter(dt$gender == 2)
nrow(male)
female <- dt %>% filter(dt$gender == 1)
nrow(female)
str(male$pathology)
healthy_m <- male %>% filter(male$pathology == "1")
nrow(healthy_m)
 

best_features <- subset(dt, select = c(meanDiameter,medianCrossSectionalArea,maxMeanVelocity,
                                       maxOverallVelocityQ99TimeS,systolicMaxMeancircumferentialVelocity, gender))
best_features$gender <- ifelse(best_features$gender == "1", "Female", "Male")
nrow(best_features)
ncol(best_features)

healthy_m <- best_features %>% filter(best_features$gender == "Male")
nrow(healthy_m)

# Features from Forward Selection:

# Histograms:
ggplot(best_features, aes(x=meanDiameter, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="meanDiameter v/s Healthy Genders",
                                                                    x="meanDiameter", y = "Gender Count")
ggplot(best_features, aes(x=medianCrossSectionalArea, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="medianCrossSectionalArea v/s Healthy Genders",
                                                                    x="medianCrossSectionalArea", y = "Gender Count")
ggplot(best_features, aes(x=maxMeanVelocity, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="maxMeanVelocity v/s Healthy Genders",
                                                                    x="maxMeanVelocity", y = "Gender Count")
ggplot(best_features, aes(x=maxOverallVelocityQ99TimeS, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="maxOverallVelocityQ99TimeS v/s Healthy Genders",
                                                                    x="maxOverallVelocityQ99TimeS", y = "Gender Count")
ggplot(best_features, aes(x=systolicMaxMeancircumferentialVelocity, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="systolicMaxMeancircumferentialVelocity v/s Healthy Genders",
                                                                    x="systolicMaxMeancircumferentialVelocity", y = "Gender Count")
# Scatterplot:
ggplot(best_features, aes(x=meanDiameter, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="meanDiameter v/s Healthy Genders",
                                    x="meanDiameter", y = "Gender")
ggplot(best_features, aes(x=medianCrossSectionalArea, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="medianCrossSectionalArea v/s Healthy Genders",
                                    x="medianCrossSectionalArea", y = "Gender")
ggplot(best_features, aes(x=maxMeanVelocity, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="maxMeanVelocity v/s Healthy Genders",
                                    x="maxMeanVelocity", y = "Gender")
ggplot(best_features, aes(x=maxOverallVelocityQ99TimeS, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="maxOverallVelocityQ99TimeS v/s Healthy Genders",
                                    x="maxOverallVelocityQ99TimeS", y = "Gender")
ggplot(best_features, aes(x=systolicMaxMeancircumferentialVelocity, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="systolicMaxMeancircumferentialVelocity v/s Healthy Genders",
                                    x="systolicMaxMeancircumferentialVelocity", y = "Gender")

sum(best_features$gender == "Male") #45
sum(best_features$gender == "Female") #45

# Scatterplot Matrix:
ncol(best_features)
colnames(best_features)
ggpairs(best_features, columns=1:(length(best_features)-1), aes(color=gender)) + 
  ggtitle("ScatterPlot Matrix for Four Best Features of Healthy Genders")


# For 9 features extracted from Chi-Square and Info Gain:

# meanMeanVelocity	diastolicMaxOverallVelocityQ99Time	minFlowJetHighVelocityAreaPercentVelocityWeighted


best_features1 <- subset(dt, select = c(meanMeanVelocity,diastolicMaxOverallVelocityQ99Time,minFlowJetHighVelocityAreaPercentVelocityWeighted,gender))
best_features1$gender <- ifelse(best_features1$gender == "1", "Female", "Male")
nrow(best_features1)
ncol(best_features1)

# Scatterplot Matrix:
ncol(best_features1)
colnames(best_features1)
ggpairs(best_features1, columns=1:(length(best_features1)-1), aes(color=gender)) + 
  ggtitle("ScatterPlot Matrix for Twelve Best Features of Healthy Genders")

# Histograms:
ggplot(best_features1, aes(x=meanMeanVelocity, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="meanMeanVelocity v/s Healthy Genders",
                                                                    x="meanMeanVelocity", y = "Gender Count")
ggplot(best_features1, aes(x=diastolicMaxOverallVelocityQ99Time, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="diastolicMaxOverallVelocityQ99Time v/s Healthy Genders",
                                                                    x="diastolicMaxOverallVelocityQ99Time", y = "Gender Count")
ggplot(best_features1, aes(x=minFlowJetHighVelocityAreaPercentVelocityWeighted, color = gender)) +
  geom_histogram(fill="white", alpha=0.5, position="identity")+labs(title="minFlowJetHighVelocityAreaPercentVelocityWeighted v/s Healthy Genders",
                                                                    x="minFlowJetHighVelocityAreaPercentVelocityWeighted", y = "Gender Count")
# Scatterplot:
ggplot(best_features1, aes(x=meanMeanVelocity, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="meanMeanVelocity v/s Healthy Genders",
                                    x="meanMeanVelocity", y = "Gender")
ggplot(best_features1, aes(x=diastolicMaxOverallVelocityQ99Time, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="diastolicMaxOverallVelocityQ99Time v/s Healthy Genders",
                                    x="diastolicMaxOverallVelocityQ99Time", y = "Gender")
ggplot(best_features1, aes(x=minFlowJetHighVelocityAreaPercentVelocityWeighted, y=gender, color = gender)) + 
  geom_point(shape=23, size = 5)+
  geom_smooth(method=lm, se=FALSE, linetype="dashed",
              color="darkred")+labs(title="minFlowJetHighVelocityAreaPercentVelocityWeighted v/s Healthy Genders",
                                    x="minFlowJetHighVelocityAreaPercentVelocityWeighted", y = "Gender")


