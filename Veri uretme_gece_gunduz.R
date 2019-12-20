library(ggplot2)
library(reshape)

Train.uret<-function(mu.SBP.gunduz, 
                     sigma.SBP, 
                     mu.DBP.gunduz,
                     sigma.DBP,
                     mu.SBP.gece, 
                     mu.DBP.gece){

SBP<-c(scale(rnorm(44, mu.SBP.gunduz,sigma.SBP), center = mu.SBP.gunduz), 
       scale(rnorm(24, mu.SBP.gece,sigma.SBP), center = mu.SBP.gece), 
       scale(rnorm(12, mu.SBP.gunduz,sigma.SBP), center = mu.SBP.gunduz))
DBP<-c(scale(rnorm(44, mu.DBP.gunduz,sigma.DBP), center = mu.DBP.gunduz),
       scale(rnorm(24, mu.DBP.gece,sigma.DBP), center = mu.DBP.gece), 
       scale(rnorm(12, mu.DBP.gunduz,sigma.DBP), center = mu.DBP.gunduz))
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
         rep(c("gece"), 24),
         rep(c("gunduz"), 12)))

Train<-data.frame(SBP,DBP, Zaman)
return(Train)
}

Train<-Train.uret(120, 5, 80, 5, 110, 70)

# beyaz onluk -------------------------------------------------------------

vesperal=NULL
mantinal=NULL
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
m=8
delta=20
SBP<-c(scale(rnorm(44, mean=mu.SBP, sigma.SBP), center=mu.SBP),  #gunduz
       scale(rnorm(24, mean=100,5), center = 100),            #gece
       scale(rnorm(12, mean=mu.SBP+delta, sigma.SBP),center = mu.SBP)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP,sigma.DBP), center = mu.DBP),            #gunduz
       scale(rnorm(24, mean=70,5), center=70),                   #gece
       scale(rnorm(12, mean=mu.DBP+delta, sigma.DBP),center = mu.DBP)) #gunduz           #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())

  

# her ikisi de artm?? -----------------------------------------------------

mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
m=8
delta1=60
delta2=40

SBP<-c(scale(rnorm(44, mean=mu.SBP+delta, sigma.SBP), center=mu.SBP),  #gunduz
       scale(rnorm(24, mean=100+delta,5), center = 100),            #gece
       scale(rnorm(12, mean=mu.SBP+delta, sigma.SBP),center = mu.SBP)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP+delta,sigma.DBP), center = mu.DBP),            #gunduz
       scale(rnorm(24, mean=70+delta,5), center=70),                   #gece
       scale(rnorm(12, mu.DBP+delta,sigma.DBP), center = mu.DBP))            #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())


# izole sistolik ----------------------------------------------------------
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
delta=30
Train<-Train.uret(120, 5, 80, 5, 100, 70)

SBP<-c(scale(rnorm(44, mean=mu.SBP+delta, sigma.SBP), center=mu.SBP),  #gunduz
       scale(rnorm(24, mean=100+delta,5), center = 100),            #gece
       scale(rnorm(12, mean=mu.SBP+delta, sigma.SBP),center = mu.SBP)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP,sigma.DBP), center = mu.DBP),            #gunduz
       scale(rnorm(24, mean=70,5), center=70),                   #gece
       scale(rnorm(12, mu.DBP,sigma.DBP), center = mu.DBP))            #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())

# izole diastolik ----------------------------------------------------------
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
delta=30
Train<-Train.uret(120, 5, 80, 5, 100, 70)

SBP<-c(scale(rnorm(44, mean=mu.SBP, sigma.SBP), center=mu.SBP),  #gunduz
       scale(rnorm(24, mean=100,5), center = 100),            #gece
       scale(rnorm(12, mean=mu.SBP, sigma.SBP),center = mu.SBP)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP+delta,sigma.DBP), center = mu.DBP),            #gunduz
       scale(rnorm(24, mean=70+delta,5), center=70),                   #gece
       scale(rnorm(12, mu.DBP+delta,sigma.DBP), center = mu.DBP))            #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())

# izole diastolik ----------------------------------------------------------
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
delta=30
Train<-Train.uret(120, 5, 80, 5, 100, 70)

SBP<-c(scale(rnorm(44, mean=mu.SBP, sigma.SBP), center=mu.SBP),  #gunduz
       scale(rnorm(24, mean=100,5), center = 100),            #gece
       scale(rnorm(12, mean=mu.SBP, sigma.SBP),center = mu.SBP)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP+delta,sigma.DBP), center = mu.DBP),            #gunduz
       scale(rnorm(24, mean=70+delta,5), center=70),                   #gece
       scale(rnorm(12, mu.DBP+delta,sigma.DBP), center = mu.DBP))            #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())

#Varyasyon artisi##############################################
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
delta=6
Train<-Train.uret(120, 5, 80, 5, 100, 70)

SBP<-c(scale(rnorm(44, mean=mu.SBP, sigma.SBP*delta), center=mu.SBP, scale = 5),  #gunduz
       scale(rnorm(24, mean=100, 5*delta), center = 100, scale=5),            #gece
       scale(rnorm(12, mean=mu.SBP, sigma.SBP*delta), center = mu.SBP, scale=5)) #gunduz
DBP<-c(scale(rnorm(44, mu.DBP, sigma.DBP*delta), center = mu.DBP, scale=5),            #gunduz
       scale(rnorm(24, mean=70, 5*delta), center=70, scale=5),                   #gece
       scale(rnorm(12, mu.DBP, sigma.DBP*delta), center = mu.DBP, scale=5))            #gunduz
Zaman<-as.factor(c(rep(c("gunduz"), 44), 
                   rep(c("gece"), 24),
                   rep(c("gunduz"), 12)))

Test<-data.frame(SBP,DBP, Zaman)

Test.grafik<-data.frame(Test, rep(c("Test"), length(Test[,1])), 1:length(Test[,1]))
names(Test.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
Train.grafik<-data.frame(Train, rep(c("E?itim"), length(Train[,1])), 1:length(Train[,1]))
names(Train.grafik)<-c("SBP", "DBP", "Gece/Gunduz", "Tip", "Zaman")
data<-rbind(Train.grafik,Test.grafik)
data<-data[,c(1,2,4,5)]
data_long <- melt(data, id=c("Zaman", "Tip"))
ggplot(data_long, aes(x=Zaman, y = value, color=variable))+
  geom_line()+
  ylab("Kan Bas?nc?")+
  facet_wrap(~Tip)+
  theme(legend.title = element_blank())

