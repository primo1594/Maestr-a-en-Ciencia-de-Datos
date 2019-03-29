data<-read.table("custdata.tsv", sep = '\t', header = TRUE)
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
intervalosIncome = c(100,1000,10000,100000)
sapply(intervalosIncome,log10)
library(scales)
ggplot(data)+geom_density(aes(x<-income))+scale_x_log10(breaks=c(100,1000,10000,100000),labels=dollar)+annotation_logticks(sides="bt")
ggplot(data)+geom_density(aes(x<-log10(income)))

tabla<-table(data$sex)
prop.table(tabla)

getmode <- function(v){
  uniqv<-unique(v)
  uniqv[which.max(tabulate(match(v,uniqv)))]
}
getmode(data$state.of.res)
ggplot(data)+geom_bar(aes(x=data$marital.stat),fill="gray")
