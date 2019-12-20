#Cok degiskenli kontrol kartlari
mult.chart(Train[1:40,1:2], type = "t2")
mult.chart(Train[1:40,1:2], type = "mewma")
mult.chart(Train[1:40,1:2], type = "mcusum")

#RTC kontrol limitleri
Train<-rbind(Train.uret(120, 5, 80, 5, 100, 70))
Train2<-rbind(Train.uret(120, 5, 80, 5, 100, 70))

Nw<-10
result<-RTC.chart.limit(Train, Train2, Nw)
result<-data.frame(result)
ggplot(result, aes(x=result)) + geom_density(alpha=.3)
CL<-median(result$result)

#RTC test
Train<-rbind(Train.uret(120, 5, 80, 5, 100, 70), 
             Train.uret(120, 5, 80, 5, 100, 70),
             Train.uret(120, 5, 80, 5, 100, 70))
Test1<-rbind(Train.uret(120, 5, 80, 5, 100, 70), Test, Train.uret(120, 5, 80, 5, 100, 70))
Test.grafik<-data.frame(Test1, rep(c("Test"), length(Test1[,1])), 1:length(Test1[,1]))
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

RTC.chart(Train,Nw,Test1, CL)


########Gercek veriler###############################################################
library(readxl)
Gerçek_Veriler <- read_excel("Dropbox/YL OGRENCILERI/2. HILMI FIDAN UZMANLIK ALAN/1. Tez/Gerçek Veriler.xlsx")
Train<-rbind(Train.uret(120, 5, 80, 5, 110, 70))
Train2<-rbind(Train.uret(120, 5, 80, 5, 100, 70))
Nw<-10
result<-RTC.chart.limit(Train, Train2, Nw)
result<-data.frame(result)
ggplot(result, aes(x=result)) + geom_density(alpha=1.0)
CL<-median(result$result)

SBP<-c(scale(Gerçek_Veriler[1:17,1], center = 120),
       scale(Gerçek_Veriler[18:31,1], center = 110),
       scale(Gerçek_Veriler[32:44,1], center = 120))
DBP<-c(scale(Gerçek_Veriler[1:17,2], center = 80),
       scale(Gerçek_Veriler[18:31,2], center = 70),
       scale(Gerçek_Veriler[32:44,2], center = 80))
Zaman<-Gerçek_Veriler[,3]
Test1<-data.frame(SBP,DBP, Zaman)

RTC.chart(Train,Nw,Test1, CL)

