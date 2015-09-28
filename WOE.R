
WOE<-function(data,predictor,target) {
  i<-0
  j<-0
  n<-0
  
  if (class(data[,predictor])!="factor") {
    
    PredictorRange<-paste(predictor,"Range",sep="")
    data[,PredictorRange]<-cut2(data[,predictor],g=12)
    Var1<-table(data[,PredictorRange],data[,target],useNA="ifany")  

    }
  
  else {
    
    Var1<-table(data[,predictor],data[,target],useNA="ifany")
  
    }
  
  #if value equals to 0 replace it with 0.5
  for (i in 1:length(Var1[,1])) {
    
    if (Var1[i,1]==0) {Var1[i,1]<-0.5}
    
    }
  
  for (j in 1:length(Var1[,2])) {
    
    if (Var1[j,2]==0) {Var1[j,2]<-0.5} 
  
    }
  
  n<-length(Var1[,1])
  
  Var2<-table(1:n,1:n)
  
  rownames(Var2)<-rownames(Var1)
  
  Var2[,1]<-(Var1[,1]+Var1[,2])/sum(Var1[,1]+Var1[,2])
  Var2[,2]<-log((Var1[,2]/sum(Var1[,2]))/(Var1[,1]/sum(Var1[,1])))
  
  WOE<-Var2[,1:2]
  colnames(WOE)<-c("POP","WOE")
  WOE<-WOE[order(WOE[,"WOE"]),]
  barplot(WOE[,2])
  WOE

}
