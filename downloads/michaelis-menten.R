##################
#Michaelis-Menten Kinetics Analysis and Graph
fpath="file:///C:/Users/Michal/Desktop/testdata.csv" #path of csv file with data
##################

library(ggplot2)
library(stringr)
library(reshape2)

rdata <- data.frame(read.csv(file=fpath))
mdata <- melt(rdata,id.vars="time",variable.name="CS")

levels(mdata$CS) <- str_sub(levels(mdata$CS),2)


vec_v <- numeric(length(colnames(rdata))-1)
vec_cs <- numeric(length(colnames(rdata))-1)

for(i in 2:length(colnames(rdata))) {
  lfit <- lm(formula=rdata[,colnames(rdata)[i]]~rdata$time) 
  vec_cs[i] <- str_sub(colnames(rdata)[i],start=2)
  vec_v[i] <- coef(lfit)[2]
}
data <- data.frame(CS=as.numeric(vec_cs),V=vec_v)

model <- nls(V ~ Vmax * CS/(Km + CS),data=data,start=c(Vmax=max(data$V),Km=max(data$CS)/2))

#Reaction Plots
ggplot(data=mdata,aes(x=time,y=value))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_smooth(method="lm",formula=y ~ x,se=F,linetype="dashed",size=0.7,aes(color=CS))+
  geom_point(aes(color=CS))+
  xlim(0,NA)+
  ylim(0,NA)+
  labs(title="Reaction Progress Plot",x="time (min)",y="[Product] (mM)",colour="[Substrate] (mM)")

#Michaelis-Menten Plot
ggplot(data=data,aes(x=CS,y=V))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  geom_smooth(method="lm",formula=y ~ I(coef(model)[1] * x / (coef(model)[2] + x)),se=F,linetype="dashed",size=1.1,color="grey45")+
  geom_point()+
  xlim(0,NA)+
  ylim(0,NA)+
  labs(title="Michaelis-Menten Kinetics Plot",x="[Substrate] (mM)",y="Initial Velocity")

summary(model)


