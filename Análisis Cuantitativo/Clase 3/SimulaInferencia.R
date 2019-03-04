###############################
#Normal
set.seed(123)
X2 <- runif(100,-2,2)
error <- rnorm(100,0,3)
Y <- 1+5*X2 + error
X3 <- runif(100,3,6)
data <- data.frame(Y,X2,X3)
head(data)
library(ggplot2)
ggplot(data, aes(x=X2,y=Y))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)
res2 <- lm(Y~X2+X3,data=data)
summary(res2)
res <- matrix(,nrow=10000,ncol=3)
for(N in 1:10000){
  error <- rnorm(100,0,3)
  Y <- 1+5*X2+error
  data <- data.frame(Y,X2,X3)
  res3 <- lm(Y~X2+X3,data=data)
  res[N,] <- coef(res3)
}
round(apply(res,2,mean),2)
hist(res[,1],50,main="Histograma del intercepto estimados")
hist(res[,2],50,main="Histograma de las pendientes con respecto a X2 estimadas")
hist(res[,3],50,main="Histograma de las pendientes con respecto a X3 estimadas")
install.packages("jtools")
library(jtools)
install.packages("ggstance")
library(ggstance)
plot_summs(res2,scale=TRUE)
plot_summs(res2,scale=TRUE,inner_ci_level=.9)
plot_summs(res2,scale=TRUE,inner_ci_level=.9,plot.distributions = TRUE)

########################################################
#Chi cuadrado

set.seed(123)
X2 <- runif(100,-2,2)
error <- rchisq(100,5)
Y <- 1+5*X2 + error
X3 <- runif(100,3,6)
data <- data.frame(Y,X2,X3)
head(data)
library(ggplot2)
ggplot(data, aes(x=X2,y=Y))+
  geom_point()+
  stat_smooth(method="lm",se=FALSE)
res2 <- lm(Y~X2+X3,data=data)
summary(res2)
res <- matrix(,nrow=10000,ncol=3)
for(N in 1:10000){
  error <- rnorm(100,0,3)
  Y <- 1+5*X2+error
  data <- data.frame(Y,X2,X3)
  res3 <- lm(Y~X2+X3,data=data)
  res[N,] <- coef(res3)
}
round(apply(res,2,mean),2)
hist(res[,1],50,main="Histograma del intercepto estimados")
hist(res[,2],50,main="Histograma de las pendientes con respecto a X2 estimadas")
hist(res[,3],50,main="Histograma de las pendientes con respecto a X3 estimadas")
install.packages("jtools")
library(jtools)
install.packages("ggstance")
library(ggstance)
plot_summs(res2,scale=TRUE)
plot_summs(res2,scale=TRUE,inner_ci_level=.9)
plot_summs(res2,scale=TRUE,inner_ci_level=.9,plot.distributions = TRUE)
