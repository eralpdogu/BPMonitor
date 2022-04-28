########Control limits#############################
library(caret)
library(boot)
library(dplyr) 
library(ggplot2)
library(MASS) #mvnrnd
library(pdp)

RTC.chart.limit<-function(Train, Train2, Nw){
S0<-sample_n(Train, length(Train[,1])) #bilinen ve normal(hipertansiyon olmayan) veriler
RESPONSE<-"NORMAL"
S0<-cbind(S0,RESPONSE)
N0<-length(S0[,1])
p0<-c()
p1<-c()
SW<-c() #pencere icinde kalan veriler
Data.model<-c()
Predict<-c()
nTrees<-c()
for (i in Nw:length(Train2[,1])) {
  SW<-Train2[(i-Nw+1):i,]
  RESPONSE<-"HIPERTANSIF"
  SW<-cbind(SW,RESPONSE)
  Data.model<-rbind(S0,SW)
  rownames(Data.model)<-1:(N0+Nw)
  #Data.model<-Data.model[,-1]
  tgrid <- expand.grid(
    .mtry = 2:3,
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  fit <- train(as.factor(RESPONSE) ~ ., 
               data = Data.model, 
               method="ranger",
               num.trees = 100,
               tuneGrid=  tgrid,
               #preProcess=c("center", "scale"),
               trControl = trainControl(method="cv", number = 10, verboseIter = T, classProbs = T))
  
  Predict<-predict(fit, type='prob', OOB=TRUE)
  p1[i]<-sum(Predict[(N0+1):(N0+Nw),2])/Nw
}

n = length(Predict[,2])/2
B = 10000
result = rep(NA, B)
for (i in 1:B) {
  boot.sample = sample(n, replace = TRUE)
  result[i] = quantile(p1[boot.sample],0.9973,na.rm = T)
}
return(result)
}
