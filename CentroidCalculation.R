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