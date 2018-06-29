source("BuildClassifier.R")
source("CentroidCalculation.R")

#library(readxl)
installlibrary(randomForest)
library(randomForest)
library(caret)

Centroid<-function(Touples){
  Cen <- c()
  CenAMU <- mean(Touples$AMU, na.rm = TRUE)
  CenAVMU <- mean(Touples$AVMU, na.rm = TRUE)
  CenADU <- mean(Touples$ADU, na.rm = TRUE)
  CenCLS <- mean(Touples$CLS, na.rm = TRUE)
  CenTC <- mean(Touples$TC, na.rm = TRUE)
  CenRL <- mean(Touples$RL, na.rm = TRUE)
  CenGL <- mean(Touples$GL, na.rm = TRUE)
  Cen <- c(CenAMU,CenAVMU,CenADU,CenCLS,CenTC,CenRL,CenGL)
  return(Cen);
}

# Build_ClassifierModel_RandomForest<- function(TrainingDataset,NumTree=500){
#   TrainingDataset$Class=factor(TrainingDataset$Class)
#   M <- randomForest(TrainingDataset$Class ~ TrainingDataset$AMU
#                     +TrainingDataset$ADU+TrainingDataset$AVMU
#                     +TrainingDataset$RL+TrainingDataset$GL
#                     +TrainingDataset$CLS+TrainingDataset$TC, 
#                     data = TrainingDataset,ntree=NumTree)
#   return(M)
# }


#Import Dataset
management_dataset <- read_excel("H:/management dataset.xlsx", 
                                 col_types = c("text", "numeric", "numeric", 
                                               "numeric", "numeric", "numeric", 
                                               "numeric", "numeric", "text"))
mgmnt=management.dataset
TrainingMalicious<- mgmnt[mgmnt$Class == "Malicious",2:9]
TrainingNotMalicious<- mgmnt[mgmnt$Class == "Not Malicious",2:9]
TrainingPartiallyMalicious<- mgmnt[mgmnt$Class == "Partially Malicious",2:9]

TestingMalicious=mgmnt[mgmnt$Class == "Malicious",2:8]

CentroidMalicious <- Centroid(TrainingMalicious[,1:7])
CentroidNotMalicious <- Centroid(TrainingNotMalicious[,1:7])
CentroidPartialMalicious <- Centroid(TrainingPartiallyMalicious[,1:7])

#Build_ClassifierModel_RandomForest(management_dataset,600)

mgmnt$Class=factor(mgmnt$Class)
M <- randomForest(mgmnt$Class ~ mgmnt$AMU
                  +mgmnt$ADU+mgmnt$AVMU
                  +mgmnt$RL+mgmnt$GL
                  +mgmnt$CLS+mgmnt$TC, 
                  data = mgmnt,ntree=500)

M <- randomForest(Class ~ AMU
                  +ADU+AVMU
                  +RL+GL
                  +CLS+TC, 
                  data = InitialTraining,ntree=500)

predict(M,Testing1)
