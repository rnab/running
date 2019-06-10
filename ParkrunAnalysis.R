#Read in the results of one Burgess parkrun
library(ggplot2)
pr<-read.csv("prres.csv")

#rn best time 22.12
rnpb=22.12*60

ggplot(data=pr[pr$sex!='0'&pr$sex!='',],aes(x=s))+
  geom_histogram()+
  facet_grid(sex~.)+
  ggtitle("Distribution of male vs. female times in Burgess parkrun")+
  geom_vline(xintercept=rnpb)+
  xlab("Time (s)")+ylab("Number of runners")+theme_bw()

