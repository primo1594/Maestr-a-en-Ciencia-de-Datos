data <- read.table(file.choose(),header=TRUE)
head(data)
str(data)
class(data)
sapply(data,class)
data2 <- data[,c(1:10,26)]
head(data2,2)
model <- lm(y~.,data=data2)
summary(model)
install.packages("olsrr")
library(olsrr)
models <- ols_step_all_possible(model)
str(models)
library(ggplot2)
plot(models)
#R2
models$mindex[which.max(models$predrsq)]
models$n[which.max(models$predrsq)]
models$predictors[which.max(models$predrsq)]
#AIC
models$mindex[which.min(models$aic)]
models$n[which.min(models$aic)]
models$predictors[which.min(models$aic)]
#BIC
models$mindex[which.min(models$sbc)]
models$n[which.min(models$sbc)]
models$predictors[which.min(models$sbc)]

modelo1 <- lm(y~x1+x2+x3+x4+x5+x9,data=data2)
modelo2 <- lm(y~x2+x3+x4+x5,data=data2)
anova(modelo1,modelo2)
#Rechazo que modelo 1 sea mejor por lo tanto modelo 2 es mejor
install.packages("leaps")
library(leaps)
fwd.model <- regsubsets(x=data[,1:25],y=data[,26],nvmax=1000,method="forward")
attributes(fwd.model)
plot(fwd.model, scale = "adjr2", main = "R^2 ajustado")
modelo3 <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x17+x19+x20+x21,data=data)
summary(modelo3)

max.model <- lm(y~.,data=data)
fwd.model.2 <- ols_step_forward_p(max.model)

modelo4 <- lm(y~x1+x2+x3+x4+x5+x9+x10+x17+x20,data=data)
fwd.model.3 <- ols_step_forward_aic(max.model)

vars.modelo5 <- fwd.model.3$predictors
formula.modelo5 <- as.formula(paste("y~",paste(vars.modelo5,collapse='+'),sep=''))
modelo5 <- lm(formula.modelo5,data=data)

back.model <- regsubsets(x=data[,1:25],y=data[,26],nvmax=1000,method="backward")
plot(back.model,scale="adjr2",main="R^2 ajustado")
back.model.2 <- ols_step_backward_p(max.model)
modelo6 <- lm(y~x1+x2+x3+x4+x5+x8+x9+x10+x17+x19+x20+x21,data=data)

back.model.3 <- ols_step_backward_aic(max.model)
