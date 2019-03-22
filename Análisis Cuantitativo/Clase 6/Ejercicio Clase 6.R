data<-read.csv("DatosTallerSelAuto.csv",sep=",")
data<-data[,-c(1:2)]
colnames(data)
model <- lm(shares ~ ., data = data)
summary(model)
library(olsrr)
library(leaps)
?regsubsets
fwd.model2.sbc<- regsubsets(x = data[,1:58], y = data[,59],
                            nvmax=10000, method = "forward")
summary(fwd.model2.sbc)
plot(fwd.model2.sbc, scale ="bic", main ="Empleando el criterio de BIC forward")
bwd.model2.sbc<- regsubsets(x = data[,1:58], y = data[,59],
                            nvmax=10000, method = "backward")
plot(bwd.model2.sbc, scale ="bic", main ="Empleando el criterio de BIC backward")
bwdmodel <- lm(shares ~ num_hrefs + average_token_length + data_channel_is_entertainment + kw_min_min + 
                 kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares  +
                 max_negative_polarity, data = data)
summary(bwdmodel)
fwdmodel <- lm(shares ~ num_hrefs + data_channel_is_entertainment + kw_max_max + kw_min_avg + kw_max_avg + 
                 kw_avg_avg + self_reference_min_shares +avg_negative_polarity, data = data)
summary(fwdmodel)
library(AER)
Ja <- jtest(bwdmodel, fwdmodel)
Ja
#num_hrefs, average_token_length, data_channel_is_entertainment, kw_min_min + kw_min_avg, 
#kw_max_avg, kw_avg_avg, self_reference_min_shares y max_negative_polarity.
#install.packages(car)
library(car)
bwdmodel <- lm(shares ~ num_hrefs + average_token_length + data_channel_is_entertainment + kw_min_min + 
                 kw_min_avg + kw_max_avg + kw_avg_avg + self_reference_min_shares  +
                 max_negative_polarity, data = data)
vif(bwdmodel)
#Esto nos dice que hay multicolinealidad en kw_max_avg, kw_avg_avg
XTX <- model.matrix(bwdmodel)
e <- eigen(t(XTX) %*% XTX)
e$val
lambda.1 <- max(e$val)
lambda.k <- min(e$val)
kappa <- sqrt(lambda.1/lambda.k)
kappa
#219921.9 Este estadístico es muy grande por lo tanto hay multicolinealidad

remueve.VIF.grande <- function(modelo, u){
  require(car)
  # extrae el dataframe
  data <- modelo$model
  # Calcula todos los VIF
  all_vifs <- car::vif(modelo)
  # extraer el nombre de todas las variables X
  names_all <- names(all_vifs)
  # extraer el nombre de la variables y
  dep_var <- all.vars(formula(modelo))[1]
  # Remover lsa variables con VIF > u
  # y reestimar el modelo con las otras variables
  while(any(all_vifs > u)){
    # elimina variable con max vif
    var_max_vif <- names(which(all_vifs == max(all_vifs)))
    # remueve la variable
    names_all <- names_all[!(names_all) %in% var_max_vif]
    # nueva formula
    myForm <- as.formula(paste(paste(dep_var, "~ "),
                               paste (names_all, collapse=" + "), sep=""))
    # re-build model with new formula
    modelo.prueba <- lm(myForm, data= data)
    all_vifs <- car::vif(modelo.prueba)
  }
  modelo.limpio <- modelo.prueba
  return(modelo.limpio)
}
modeloGenial<-remueve.VIF.grande(bwdmodel,4)
summary(modeloGenial)
