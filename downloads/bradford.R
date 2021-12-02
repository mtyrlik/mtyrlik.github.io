std_dil <- c(1500,1000,750,500,250,125,25,0) #standard dilutions (concentration ug/ml)
sam_dil <- c(0.5,0.2,0.1,0.05) #sample dilutions (fraction of original sample)
fpath <- "file:///C:/Users/Michal/Desktop/testdata-bradford.csv" #path to the source .csv file
####################
library(stringr)
library(ggplot2)


rdata <- data.frame(read.csv(file=fpath))

blank <- rdata$standard[length(rdata$standard)]

rdata$standard <- rdata$standard - blank
rdata$sample <- rdata$sample-blank



ldata <- data.frame(Conc=std_dil,Abs=rdata$standard)
ldata$absdiff <- abs(rdata$standard-rdata$sample[1])
ldata <- ldata[order(ldata$absdiff),]

lfit <- lm(formula=Conc~Abs,data=ldata[1:2,])

pconc <- numeric(length(sam_dil))
sdata <- data.frame(Abs=rdata$sample)
sdata$pred <- predict(lfit,sdata)
sdata$orig <- sdata$pred/sam_dil
sdata$dil <- sam_dil
sdata <- head(sdata,length(sam_dil))

cfit <- lm(data=sdata,formula=pred~dil)

ggplot(data=ldata,aes(x=Conc,y=Abs))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_smooth(method="lm",formula=y ~ x,se=F,linetype="dashed",size=1,aes(),color="grey45")+
  geom_point(aes())+
  xlim(0,NA)+
  ylim(0,NA)+
  labs(title="Bradford Assay Standard Curve",x="[Protein] (ug/ml)",y="Absorbance")

ggplot(data=sdata,aes(x=dil,y=pred))+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(legend.position="bottom")+
  geom_smooth(method="lm",formula=y ~ x,se=F,linetype="dashed",size=1,aes(),color="grey45")+
  geom_point(aes())+
  xlim(0,NA)+
  ylim(0,NA)+
  labs(title="Bradford Assay Calculation Curve",x="Dilution",y="Predicted Sample Conc.")

summary(lfit)
head(sdata,length(sam_dil))
summary(cfit)
