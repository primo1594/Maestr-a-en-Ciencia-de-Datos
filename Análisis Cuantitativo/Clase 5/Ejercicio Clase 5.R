data<-read.csv("DatosTallerSelAuto.csv")
head(data,3)
str(data)
data<-data[,2:61]
colnames(data)
class(data)
data<-data[,-c(37,38)]
colnames(data)
model<-lm(data$shares~.,data=data)
summary(model)
fwd.model<-ols_step_forward_p(model)
fwd.model$predictors
vars.model <- fwd.model$predictors
formula.model <- as.formula(paste("shares~",paste(vars.model,collapse='+'),sep=''))
lm.model <- lm(formula.model,data=data)
summary(lm.model)
install.packages("MASS")
library(MASS)
install.packages("RcmdrMisc")
library(RcmdrMisc)
fwd.model.sbc<-stepwise(model,direction = "forward",criterion="BIC")
fwd.model.sbc
lm.model2 <- lm(shares ~ kw_avg_avg + self_reference_min_shares + kw_max_avg + 
                  kw_min_avg + timedelta + num_hrefs + n_tokens_title + data_channel_is_entertainment + 
                   LDA_03 + avg_negative_polarity + average_token_length,data=data)
summary(lm.model2)
anova(lm.model2,lm.model)
