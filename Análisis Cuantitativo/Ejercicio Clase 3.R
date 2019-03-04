#Taller en clase: Inferencia y comparación de modelos.
data<-read.csv("DatosTallerInf.csv")
head(data,3)
data<-data[,c(-1,-2)]
head(data,3)
str(data)
#Al interior del equipo de data analytics de la compañía hay tres posiciones. 
#Un grupo sugiere que el mejor modelo es empleando todas las variables. 
#El segundo grupo cree que solo se deben incluir las variables que van 
#de la 2 a la 12. El otro grupo cree que deberían emplearse solo las variables 
#de la 13 a la 30.
head(data[1:12],3)
colnames(data[1:12])
fmla <- as.formula(paste("shares ~ ", paste(colnames(data[1:12]), collapse= "+")))
fmla2 <- as.formula(paste("shares~",paste(colnames(data[13:30]),collapse="+")))
res1<-lm(shares~.,data)
res2<-lm(fmla,data)
res3<-lm(fmla2,data)
anova(res2,res1)
#Con base en el pvalor podemos rechazar la hipótesis nula de que 2 es mejor que 1,
#por lo tanto 1 es mejor que 2
anova(res3,res1)
#Misma conclusión de arriba.
# Por lo tanto el modelo 1 aparentemente es el mejor de los 3 propuestos.
summary(res1)
res4<-lm(shares~.-n_unique_tokens-n_non_stop_words-n_non_stop_unique_tokens
         -num_imgs-num_videos-num_keywords-kw_min_min-kw_max_max-kw_avg_max,data)
summary(res4)
anova(res4,res1)
#Con base en el pvalor 0.4033 no podemos rechazar la hipótesis nula,
#por lo tanto el modelo 4 es mejor
