install.packages("tidyverse")
library("tidyverse")

DataC1 <- read.csv("DataCap1.csv", sep=",")
head(DataC1)
class(DataC1)
dim(DataC1)
DataC1 <- DataC1[,-c(1:3)]
DataC1 <- DataC1[-1,]
head(DataC1)
dim(DataC1)
apply(DataC1,2,class)
?apply
attach(DataC1)
#plot(PIB_cp,TD_cp)
ggplot(data=DataC1)+
  geom_point(mapping=aes(x=PIB_cp,y=TD_cp), color="blue")+
  xlab("Cambio % trimestral del PIB")+
  ylab("Cambio % trimestral de la TD")+
  geom_smooth(mapping=aes(x=PIB_cp,y=TD_cp),color="blue",method='lm',se=FALSE)
R1<-lm(formula=TD_cp~PIB_cp,data=DataC1)
R1a <- lm( TD_cp ~ PIB_cp, DataC1)
formula1 <- TD_cp ~ PIB_cp
R2 <- lm( formula1 , DataC1)
R3 <- lm( TD_cp ~ ., DataC1)
detach(DataC1)
summary(R1)
summary(R1a)
summary(R2)
summary(R3)

install.packages("estout")
library("estout")
estclear()
eststo(R1)
esttab(t.value=TRUE,sig.levels = c(0.1,0.05,0.01),
       sig.sym=c("*","**","***"),
       caption="Modelo estimado por MCO",
       caption.top=TRUE,label="res1",
       var.rename=c("(Intercept)","intercepto"))
vcov(R1)
