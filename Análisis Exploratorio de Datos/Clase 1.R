data<-read.table(file = file.choose(), sep = '\t', header = TRUE)
head(data)
plot(data)
summary(data)
library(dplyr)
data2<-filter(data,data$health.ins==F)
summary(data2)
library(ggplot2)
ggplot(data, aes(x=" ",y=data$income))+
  geom_boxplot()+
  geom_jitter(width=0.2)+ylab("yearly income in thousands")
+xlab("# of customers")

ggplot(data,aes(x=data$age))+
  geom_histogram()+xlab("Customer age")+
  geom_density()+
  geom_vline(aes(xintercept=mean(age)),color="blue",linetype="dashed",size=1)
?geom_density
