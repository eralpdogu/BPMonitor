library(ggplot2)

vesperal=NULL
mantinal=NULL
mu.SBP=120
sigma.SBP=5
mu.DBP=80
sigma.DBP=5
m=8
for(k in 1:m){vesperal[k]=rnorm(1,mu.SBP,sigma.SBP)-((k-m)/m)*sigma.SBP}
for(k in 1:m){mantinal[k]=rnorm(1,100,5)+((k-m)/m)*5}
SBP<-c(rnorm(56, mean=mu.SBP,sigma.SBP), 
       vesperal, 
       rnorm(12, mean=100,5), 
       mantinal, 
       rnorm(24, mean=mu.SBP,sigma.SBP))

for(k in 1:m){vesperal[k]=rnorm(1,mu.DBP,sigma.DBP)-((k-m)/m)*sigma.DBP}
for(k in 1:m){mantinal[k]=rnorm(1,70,5)+((k-m)/m)*5}
DBP<-c(rnorm(24, mu.DBP,sigma.DBP), 
       vesperal, 
       rnorm(12, mean=70,5), 
       mantinal, 
       rnorm(24, mu.DBP,sigma.DBP))

Zaman<-as.factor(c(rep(c("gunduz"), 24), 
         rep(c("gece"), 12), 
         rep(c("gece"), 12),
         rep(c("gece"), 12),
         rep(c("gunduz"), 24)))

Data<-data.frame(SBP,DBP, Zaman)

ggplot(Data, aes(y=SBP, x=1:length(Data[,1])))+
  geom_line()+
  geom_smooth()

ggplot(Data, aes(y=DBP, x=1:length(Data[,1])))+
  geom_point()+
  geom_smooth()


