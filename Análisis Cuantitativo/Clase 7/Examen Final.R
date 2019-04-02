#Paquetes necesarios.
library(car)#Multicolinealidad
library(xts)#Series de tiempo
library(olsrr)#Normalidad de los errores
library(lmtest)#Verificar Heteroscedasticidad
library(sandwich)#Corregir Heteroscedasticidad
library(AER)#WaldTest
library(tseries)#Verificar Autocorrelación
library(lm.beta)#Estandarizar Coeficientes
library(ggplot2)#Para graficar
#Leer Datos.
data<-read.csv("day.csv")
head(data)
tail(data)
str(data)
#Poner la fecha en formato fecha
data$dteday<-as.Date(data$dteday)
#Borrar la columna del consecutivo
data<-data[,-1]
str(data)
attach(data)
#Hacer la regresión
model<-lm(cnt~.,data)
summary(model)
#essentially perfect fit: summary may be unreliable
#Verificar Multicolinealidad
vif(model)
#dteday, season, yr, mnth, temp, atemp, registered son variables que tienen VIF mayor a 4.
data2<-data[,-which(colnames(data) %in% c("yr","mnth","atemp","registered"))]
#Quito las variables yr y mnth porque la misma información me la da dteday y con el día incluido.
#Quito la variable atemp porque la misma información me la da temp.
#Quito la variable registered porque la misma información se puede obtener con casual. cnt-casual=registered
str(data2)
detach(data)
#Nueva regresión
attach(data2)
model2<-lm(cnt~.,data2)
summary(model2)
#essentially perfect fit: summary may be unreliable
#Verifico VIF de nuevo
vif(model2)
#Ya no hay VIF mayor a 4
#Verifico Heteroscedasticidad
#Verifico supuesto de normalidad de los errores para saber si studentizo o no.
ols_test_normality(model2)
#En todos los casos podemos rechazar que los errores sigan una distribución normal.
#Usaremos Studentized de Koenker
bptest(model2,studentize=T)
#Rechazo Homoscedasticidad.
coeftest(model2, vcov = (vcovHC(model2)))
#Con la correción podemos ver que holiday y weekday no son variables significativas.
#Hacemos una regresión sin estas variables.
model3<-lm(cnt~.-holiday-weekday,data2)
summary(model3)
#Comparamos los modelos
waldtest(model3,model2,vcov=(vcovHC(model2)))
#No puedo rechazar que el modelo 3 sea mejor que el 2. Me quedo con el 3.
detach(data2)
#Verifico Autocorrelación
data3<-data2[,-which(colnames(data2) %in% c("holiday","weekday"))]
str(data3)
head(data3)
model3<-lm(cnt~.,data3)
summary(model3)
#Obtengo los residuales
e<-residuals(model3)
#Miro los residuales
ts.plot(e, main= "Errores estimados", xlab= "tiempo",ylab= "errores")
#Cambian de signo
signo.error <- factor(e>0)
head(signo.error, 3)
runs.test(signo.error)
#Rechazo no Autocorrelación.
runs.test(signo.error,alternative="less")
#Rechazo la autocorrelación negativa.
runs.test(signo.error,alternative="greater")
#No rechazo la autocorrelación positiva.
dwtest(model3, alternative = "two.sided")
#Rechazo
dwtest(model3, alternative = "less")
#No rechazo
dwtest(model3, alternative = "greater")
#Rechazo. Entonces hay autocorrelación positiva
coeftest(model3, vcov = NeweyWest(model3))
coeftest(model3, vcov = kernHAC(model3))
coeftest(model3, vcov = weave(model3))
#Los tests nos indican que la variable season pasa a ser no significativa y la variable hum baja la significancia.
#Estimo modelo sin estas variables
attach(data3)
model4<-lm(cnt~.-season-hum,data3)
summary(model4)
waldtest(model4,model3,vcov=NeweyWest(model3))
waldtest(model4,model3,vcov=kernHAC(model3))
waldtest(model4,model3,vcov=weave(model3))
#No puedo rechazar que model 4 sea mejor que model 3. Me quedo con model 4
#Estandarizo los coeficientes
standar.model <- lm.beta(model4)
summary(standar.model)
#Obtengo el máximo coeficiente
coef(standar.model)[which.max(abs(coef(standar.model)))]
#La variable que más afecta es casual
#Por último grafico
coef.est <- matrix(0,10,2)
var.names  <- c(names(standar.model$standardized.coefficients)[-1])
coef.est <- matrix(standar.model$standardized.coefficients[-1])
coef.estand <- as.data.frame(var.names)
coef.estand$coef.est <- coef.est 

p <- ggplot(data=coef.estand, aes(x=var.names, y=coef.est)) +
  geom_bar(stat="identity", fill="steelblue") + 
  ggtitle("Coeficientes estandarizados")
p + coord_flip()
