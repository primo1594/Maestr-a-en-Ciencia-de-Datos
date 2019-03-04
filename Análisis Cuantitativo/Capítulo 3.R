load("Data/RetornosDiarios.RData")
install.packages("xts")
library(xts)
head(retornos.diarios)
class(retornos.diarios)
str(retornos.diarios)
sapply(retornos.diarios,class)
summary(retornos.diarios)
plot(retornos.diarios)
cor(retornos.diarios)
abs(cor(retornos.diarios)) > 0.65
res1 <- lm(GRUPOSURA ~ .,retornos.diarios)
summary(res1)
anova(res1)
install.packages("AER")
library(AER)
?linear.hypothesis
#linear.hypothesis is replaced by the linearHypothesis function.Deprecated in 2009. Now defunct
linearHypothesis(res1,c("CONCONCRET=0","OCCIDENTE=0"))
linearHypothesis(res1,c("CONCONCRET=0","OCCIDENTE=0"),test="Chisq")
linearHypothesis(res1,c("ECOPETROL=0.12","EXITO=0.12"))
linearHypothesis(res1,c("ECOPETROL=0.12","EXITO=0.12"),test="Chisq")

#3.7 - 1
#Solo puedo comparar 3.23, 3.24, 3.26, 3.27 porque tienen el mismo número de variables 
#explicativas (3)
library(readxl)
my_data <- read_excel("regmult.xls")
head(my_data)
str(my_data)
class(my_data)
colnames(my_data)
colnames(my_data)<-c("Año","CE","CD","I","Ldies","LEl","V")
head(my_data)
abs(cor(my_data))>0.65
res1<-lm(I~CE+CD,my_data)
summary(res1)#0.998
res2<-lm(I~Ldies+LEl,my_data)
summary(res2)#0.9947
res3<-lm(I~CE+LEl,my_data)
summary(res3)#0.9989
res4<-lm(I~CD+Ldies,my_data)
summary(res4)#0.9937
#Con base en el R2 podríamos pensar que el mejor modelo es el 3.26 porque tiene el máximo R2
#Con base en el F y su respectivo pvalor podríamos rechazar la hipótesis nula de que ninguna 
#variable es importante
#Pruebas individuales sobre los parámetros
anova(res3)
#Pruebas conjuntas sobre los parámetros
#linearHypothesis(res3,c(""))

#Capítulo 4
data<-read.table("selModel.txt",header=TRUE,sep=",")
head(data)
str(data)
class(data)
data<-data[,-1]
head(data,3)
res1<-lm(y~x1+x2+x3+x6+x7,data)
res2<-lm(y~x1+x2+x3,data)
res3<-lm(y~x4+x5+x8+x9+x10,data)
#R1
R1<-summary(res1)
R1$adj.r.squared#0.6388607
AIC(res1)#733.9571
BIC(res1)#755.0315
#R2
R2<-summary(res2)
R2$adj.r.squared#0.6357257
AIC(res2)#733.3226
BIC(res2)#748.3758
#R3
R3<-summary(res3)
R3$adj.r.squared#0.5507108
AIC(res3)#766.7175
BIC(res3)#787.792
#Con R Ajustado concluimos que el modelo 1 es el mejor
#Con AIC y BIC concluimos que el modelo 2 es el mejor

#4.4.2 Pruebas estadísticas
#Anidados
anova(res2,res1)
#Con base en el valorp 0.1988 no se puede rechazar la hipótesis nula de que el modelo restringido
#es mejor que el modelo sin restringir, por lo tanto sigue siendo mejor el 2
#No Anidados
library(AER)
J.res1.3<-jtest(res1,res3)
J.res1.3
#Con base en el tvalue y su pvalor 0, se rechaza la hipótesis nula de que el modelo 1 es mejor 
#que el 3
#Con base en el tvalue y su pvalor 0, se rechaza la hipótesis nula de que el modelo 3 es mejor
#que el 1
J.res2.3<-jtest(res2,res3)
J.res2.3
#Mismas conclusiones de arriba
Cox.res1.3<-coxtest(res1,res3)
Cox.res1.3
Cox.res2.3<-coxtest(res2,res3)
Cox.res2.3
#Mismas conclusiones de arriba
#No se puede demostrar que el modelo 3 sea mejor que el 1 o el 2, por lo tanto el 2 sigue siendo 
#el mejor

#3.7 1 con 4

library(readxl)
my_data <- read_excel("regmult.xls")
head(my_data)
str(my_data)
class(my_data)
colnames(my_data)
colnames(my_data)<-c("Año","CE","CD","I","Ldies","LEl","V")
head(my_data)
res1<-lm(I~CE+CD+Ldies+LEl+V,my_data)
R1<-summary(res1)
R1$adj.r.squared#0.999164
AIC(res1)#430.163
BIC(res1)#438.6952
res2<-lm(I~CE+CD,my_data)
R2<-summary(res2)
R2$adj.r.squared#0.9978244
AIC(res2)#451.7391
BIC(res2)#456.6146
res3<-lm(I~Ldies+LEl,my_data)
R3<-summary(res3)
R3$adj.r.squared#0.9941711
AIC(res3)#476.3767
BIC(res3)#481.2522
res4<-lm(I~V,my_data)
R4<-summary(res4)
R4$adj.r.squared#0.9915901
AIC(res4)#484.6526
BIC(res4)#488.3092
res5<-lm(I~CE+LEl,my_data)
R5<-summary(res5)
R5$adj.r.squared#0.9988223
AIC(res5)#436.3941
BIC(res5)#441.2696
res6<-lm(I~CD+Ldies,my_data)
R6<-summary(res6)
R6$adj.r.squared#0.9931598
AIC(res6)#480.3763
BIC(res6)#485.2518
res7<-lm(I~CE+Ldies+LEl+V,my_data)
R7<-summary(res7)
R7$adj.r.squared#0.9991945
AIC(res7)#428.5169
BIC(res7)#435.8302
res8<-lm(I~CE+CD+V,my_data)
R8<-summary(res8)
R8$adj.r.squared#0.9979049
AIC(res8)#451.6329
BIC(res8)#457.7273
#Con base en R2 Ajustado podemos pensar que el modelo 7 es el mejor un poco por encima del
#modelo 1
#Con base en AIC podemos pensar que el modelo 7 es el mejor un poco por debajo del 1
#Con base en BIC podemos pensar lo mismo de AIC
#Anidados
anova(res7,res1)
#Con base en el valorp 0.6088 no se puede rechazar la hipótesis nula de que el modelo restringido
#es mejor que el modelo sin restringir, por lo tanto sigue siendo mejor el 7
J.res7.2<-jtest(res7,res2)
J.res7.2
#Con base en el valorp 0.6088 no se puede rechazar la hipótesis nula de que 7 es mejor que 2, 
#por lo tanto 7 es mejor que 2. Al revés el pvalor es 0 y se rechaza que 2 sea mejor que 7.
Cox.res7.2<-coxtest(res7,res2)
Cox.res7.2
#Misma conclusión de arriba. Descarto el modelo 2
anova(res3,res7)
#Con base en el valorp 0 se rechaza la hipótesis nula de que el modelo 3 es mejor que el 7,
#por lo tanto 7 sigue siendo el mejor
anova(res4,res7)
#Misma conclusión de arriba
anova(res5,res7)
#Misma conclusión de arriba
jtest(res7,res6)
#(0.6088) No se puede rechazar que 7 sea mejor que 6, por lo tanto 7 es mejor.
#(0) Se puede rechazar que 6 es mejor que 7, por lo tanto 7 es mejor.
coxtest(res7,res6)
#Misma conclusión de arriba
jtest(res7,res8)
#Misma conclusión de arriba entre 7 y 8. Por lo tanto 7 es mejor
coxtest(res7,res8)
#Misma conclusión de arriba
#Con base en todo esto concluyo que el mejor modelo es el 7.

#3.7 2 
#a
summary(res7)
#Con base en el pvalor de cada coeficiente podemos concluir que:
#CE y LEl son variables significativas para explicar I
#Ldies es variables significativa con 99% de confianza para explicar I
#V no es una variable significativa para explicar I
#R2 indica que este modelo explica el 99.93% de la variación de I
#F y su pvalor indican que se rechaza la nula de que ninguna es importante, por lo tanto
#alguna es importante.

anova(res7)
#Mismas concluciones de arriba

#b
linearHypothesis(res7,c("V=0"))
linearHypothesis(res7,c("V=0"),test="Chisq")
#No se puede rechazar que no tenga efecto en I, por lo tanto V no tiene efecto en I

#c
#Yo diría que el CE es el que más afecta al ingreso. Porque tiene el número Estimado más grande

#d
#Que aumente el CE o que disminuya el Ldies y el LEl
