library("readxl")
library("tidyverse")

comp<-data.frame()

yrs<-c(2014,2015,2016,2019)
for (i in 1:4)
{x<-read_excel("StAlbansHMData.xlsx",sheet=i)
x$yr=yrs[i]
comp<-rbind(comp,x[,c(1:6,which(colnames(x)=="yr"))])
}


names(comp)<-c("split","time","time2","distance","elev_gain","elev_loss","yr")

comp<-mutate(comp,time.m=time*(24*60),speed=distance/time.m,
             elev_gain=ifelse(elev_gain=="--",0,as.numeric(elev_gain)),
             elev_loss=ifelse(elev_loss=="--",0,as.numeric(elev_loss)),
             elev_net=elev_gain-elev_loss)

plot(comp$elev_net,comp$speed)
abline(lm(comp$speed~comp$elev_net))

lm1<-lm(comp$speed~comp$elev_net)

comp<-mutate(comp,speed.pred=predict(lm1,comp),AvE=speed/speed.pred)



ggplot(data=comp, aes(x=split,y=speed,col=as.factor(yr)))+
  geom_point()+geom_line()+ylab("Speed (km / min)")+
  ggtitle("Raw speed by Split")

ggplot(data=comp%>%filter(split!=22), aes(x=split,y=AvE,col=as.factor(yr)))+
  geom_point()+geom_line()+ylab("Actual vs Expected Speed by split")+xlab("Km")+
  ggtitle("Actual vs expected speed by split")
