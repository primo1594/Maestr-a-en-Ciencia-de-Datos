data <- read.table("DatosParcial_1.txt",header=TRUE)
head(data,3)
str(data)
#Si se estima un modelo de regresión múltiple de y en función de x1, x2, x3, x4 y x5
#(llamemos a este el modelo1), se puede concluir sobre el intercepto que:
modelo1<-lm(y~x1+x2+x3+x4+x5,data=data)
summary(modelo1)
#Si se estima un segundo modelo (llamemos a este el modelo2) en el que y es 
#explicado por x1, x3,  y x5, con un 99% de confianza, ¿cuál de los dos modelos 
#es mejor?
modelo2<-lm(y~x1+x3+x5,data=data)
anova(modelo2,modelo1)
#Rechazo con 99% que 2 sea mejor que 1. Por lo tanto 1 es mejor.

#Independientemente de su respuesta en la pregunta anterior y empleando el 
#modelo2, ¿qué se puede concluir sobre el efecto de la variable x3 sobre y?
summary(modelo2)
#y=13.1583+1.2524x1+1.5916x3+1.5080x5+e

#Independientemente de su respuesta en las preguntas anteriores y empleando 
#el modelo1, El R-cuadrado del modelo implica que ...
summary(modelo1)

#5. Ahora estime un tercer modelo (modelo3) en el que Y es explicado por x3, 
#x5, x10 y x15. En este nuevo modelo se puede concluir que:
modelo3<-lm(y~x3+x5+x10+x15,data=data)
summary(modelo3)

#Con un 99% de confianza y todas las pruebas estudiadas, ¿cuál modelo es 
#mejor, el model2 o el model3?
library(AER)
jtest(modelo2,modelo3)

#Cree una variable dummy que tome el valor de uno sí la variable x2 es mayor
#que 5 y cero en caso contrario. Llame a esta variable dummy D. 
#Ahora construya un modelo en el que y depende únicamente de x1, 
#pero se permite un cambio en el intercepto y la pendiente cuando X2 
#es mayor que 5. (Llame a este modelo modelo4)
#Con dicho modelo (modelo4) se puede concluir que:
D<-data$x2>5
data$D<-as.numeric(D)
colnames(data)
modelo4<-lm(y~D+x1+D*x1,data=data)
summary(modelo4)
