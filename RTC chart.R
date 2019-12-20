library(caret)
library(boot)
library(dplyr) 
library(ggplot2)
library(MASS) #mvnrnd
library(pdp)

RTC.chart<-function(Train, Nw, Test, CL){
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
Var.Imp<-matrix(0,length(Test[,1]),2)
for (i in Nw:length(Test[,1])) {
  SW<-Test[(i-Nw+1):i,]
  RESPONSE<-"HIPERTANSIF"
  SW<-cbind(SW,RESPONSE)
  Data.model<-rbind(S0,SW)
  rownames(Data.model)<-1:(N0+Nw)
  #Data.model<-Data.model[,-1]
  fitControl <- trainControl(classProbs = T, method = "oob")
  tgrid <- expand.grid(
    .mtry = 2:3,
    .splitrule = "gini",
    .min.node.size = c(10, 20)
  )
  fit <- train(as.factor(RESPONSE) ~ ., 
               data = Data.model, 
               method="ranger",
               num.trees = 2000,
               tuneGrid=  tgrid,
               #preProcess=c("center", "scale"),
               trControl = trainControl(method="cv", number = 10, verboseIter = T, classProbs = T),
               importance = "permutation")
               
  Var.Imp[1:Nw,1:2]<-NA
  Var.Imp[i,1:2]<-varImp(fit)[[1]]$Overall[-1]
  colnames(Var.Imp)<-c("DBP","SBP")
  Predict<-predict(fit, type='prob', OOB=TRUE)
  p1[i]<-sum(Predict[(N0+1):(N0+Nw),2])/Nw
  #if(p1[i]<CL){
  #Var.Imp[i,1]=NA
  #Var.Imp[i,2]=NA}
}

sonuclar<-data.frame(p1, CL, Var.Imp/1000)

gg<-ggplot(sonuclar, aes(x=1:length(sonuclar[,1])))+
  geom_point(aes(y=p1, colour="Tahmin"), size=1)+
  geom_line(aes(y=p1, colour="Tahmin"),  size=1)+
  geom_smooth(aes(y=p1))+
  geom_line(aes(y=DBP, colour="DBP"), size=0.8)+
  geom_line(aes(y=SBP, colour="SBP"), size=0.8)+
  geom_line(aes(y=CL, colour="Kontrol.limiti"),  size=1)+
  labs(x = "Zaman",
       y = "Hipertansif olma olasiligi",
       color = "Legend")+
  scale_color_manual(name="",
                         values=c(Tahmin="black", DBP="blue", SBP="red", Kontrol.limiti="darkgrey"))
gg
}


