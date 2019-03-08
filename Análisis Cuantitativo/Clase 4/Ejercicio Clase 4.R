library(readxl)
data <- read_excel(file.choose())
head(data)
str(data)
attach(data)
D <- Sexo=='F'
head(D)
View(data)
data$D <- as.numeric(D)
head(data)
plot(Salario,D)
res1 <- lm(Salario ~ Educación+Experiencia+data$`Años Compañía`,data=data)
summary(res1)
res2 <- lm(Salario ~ D+Educación+D*Educación+Experiencia+D*Experiencia+data$`Años Compañía`+
             D*data$`Años Compañía`,data=data)
summary(res2)
anova(res1,res2)
##Podemos rechazar con 90% de confianza que el 1 sea mejor que el 2. Es decir, el género
##de alguna manera sí afecta al Salario.