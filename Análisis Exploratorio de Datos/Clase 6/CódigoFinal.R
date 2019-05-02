library(ggplot2)

DataFinal <- read.csv("SABERTYT20162.csv",sep=";") 
head(DataFinal)
str(DataFinal)
summary(DataFinal$ESTU_INST_MUNICIPIO)

class(DataFinal$ESTU_INST_MUNICIPIO)
#El tipo de dato coincide
summary(DataFinal)
#Si solo tengo en cuenta esta columna, por supuesto habrán duplicados
duplicated(DataFinal$ESTU_INST_MUNICIPIO)
table(is.na(DataFinal$ESTU_INST_MUNICIPIO))
#No hay NA

#Prueba<-DataFinal[which(DataFinal$ESTU_ESTADO=="PUBLICAR" && DataFinal$ESTU_NIVEL_PRGM_ACADEMICO=="TÉCNICO PROFESIONAL"),]
Prueba<-DataFinal[which(DataFinal$ESTU_ESTADO=="PUBLICAR"),]
Prueba <- Prueba[which(Prueba$ESTU_NIVEL_PRGM_ACADEMICO=="TÉCNICO PROFESIONAL"),]
table(Prueba$ESTU_ESTADO)
subset_sabertyt<-subset(Prueba,select = c("MOD_RAZONA_CUANTITAT_PUNT", "MOD_LECTURA_CRITICA_PUNT", "MOD_COMUNI_ESCRITA_PUNT", "ESTU_INST_MUNICIPIO", "ESTU_GENERO", "ESTU_PRGM_ACADEMICO", "INST_NOMBRE_INSTITUCION"))
View(subset_sabertyt)
nrow(subset_sabertyt)


summary(subset_sabertyt)
sd(subset_sabertyt$MOD_LECTURA_CRITICA_PUNT)

summary(subset_sabertyt$MOD_RAZONA_CUANTITAT_PUNT)

summary(subset_sabertyt)
Prueba2<- subset_sabertyt[which(subset_sabertyt$ESTU_INST_MUNICIPIO =="BOGOTÁ D.C."),]
table(Prueba2$ESTU_INST_MUNICIPIO)
Prueba3<-Prueba2
##Sumar puntajes por colegios y sacar el promedio
Prueba3<-table(Prueba3$INST_NOMBRE_INSTITUCION)
View(Prueba3)
Prueba4<-aggregate(subset_sabertyt$MOD_RAZONA_CUANTITAT_PUNT, by=list(INST_NOMBRE_INSTITUCION=subset_sabertyt$INST_NOMBRE_INSTITUCION), FUN=sum)
View(Prueba4)
dataMerged = merge(Prueba3,Prueba4, by.x=c("Var1"),
                    by.y=c("INST_NOMBRE_INSTITUCION"))
View(dataMerged)
dataMerged$PROM<-dataMerged$x/dataMerged$Freq
Results<-dataMerged[c(38,106,73),]
Results$Var1
