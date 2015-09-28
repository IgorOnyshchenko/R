
IV_all <- function(Data,target) {
  i <- 0
  j <- 0
  n <- 0

  IV_vector <- as.vector(1)
  name <- names(Data)

  for (n in 1:length(name)) {
    
    PredictorRange <- paste(name[n], "Range", sep="")

    if (class(Data[, name[n]]) == "numeric") {
      Data[, PredictorRange] <- cut2(Data[, name[n]], g=12)
      }
    
    else if (class(Data[, name[n]]) == "integer") {
      Data[, PredictorRange] <- cut2(Data[, name[n]], g=12)
      }
    
    else {
      Data[, PredictorRange] <- Data[, name[n]]
      }
    
    Var1 <- table(Data[, PredictorRange], Data[, target], useNA = "ifany")
    
    for (i in 1:length(Var1[, 1])) {
      if (Var1[i, 1] == 0) {Var1[i, 1] <- 0.5} 
      }
    
    for (j in 1:length(Var1[, 2])) {
      if (Var1[j, 2] == 0) {Var1[j, 2] <- 0.5} 
      }
    
    IV_all <- sum((Var1[, 1]/sum(Var1[, 1]) - Var1[, 2]/sum(Var1[, 2]))*
                    log((Var1[, 1]/sum(Var1[, 1]))/(Var1[, 2]/sum(Var1[, 2]))))
    IV_vector[n] <- IV_all
  }
  
  names(IV_vector) <- name
  IV_vector <<- as.matrix(IV_vector)
  #write.csv(IV_vector,'d:/Model Frozen RUS/model0_126/IV.csv')
  print(IV_vector)

}