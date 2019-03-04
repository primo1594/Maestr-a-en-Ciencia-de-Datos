
##leer el csv
##
DataC1 <- read.csv("DataCap1.csv", sep=",")

head(DataC1)
##datos esperados
## X TD PIB TD_cp PIB_cp
## 1 2001-03-01 16.70 77308 NA NA
## 2 2001-06-01 14.70 81150 -11.98 4.97
## 3 2001-09-01 14.73 83299 0.23 2.65
## 4 2001-12-01 13.80 83757 -6.33 0.55
## 5 2002-03-01 16.43 84655 19.08 1.07
## 6 2002-06-01 15.80 84575 -3.85 -0.09
class(DataC1)
## [1] "data.frame"

dim(DataC1)
## [1] 71 5
DataC1 <- DataC1[,-c(1:3)]
DataC1 <- DataC1[-1,]
head(DataC1)
## TD_cp PIB_cp
## 2 -11.98 4.97
## 3 0.23 2.65
## 4 -6.33 0.55
## 5 19.08 1.07
## 6 -3.85 -0.09
## 7 -3.16 6.32
dim(DataC1)
## [1] 70 2
apply(DataC1, 2, class)
## TD_cp PIB_cp
## "numeric" "numeric"

plot(DataC1$PIB_cp, DataC1$TD_cp)
##presenta problemas
R1 <- lm(formula = TD_cp ~ PIB_cp, data = DataC1)
summary(R1)
##
## Call:
## lm(formula = TD_cp ~ PIB_cp, data = DataC1)
##
## Residuals:
## Min 1Q Median 3Q Max
## -12.5319 -5.6147 0.8211 3.0796 18.6964
##
## Coefficients:
## Estimate Std. Error t value Pr(>|t|)
## (Intercept) 3.0024 0.8393 3.577 0.000645 ***
## PIB_cp -1.8120 0.1302 -13.922 < 2e-16 ***
## ---
## Signif. codes: 0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
##
## Residual standard error: 6.785 on 68 degrees of freedom
## Multiple R-squared: 0.7403,Adjusted R-squared: 0.7365
## F-statistic: 193.8 on 1 and 68 DF, p-value: < 2.2e-16

R1a <- lm( TD_cp ~  PIB_cp, DataC1)
formula1 <- TD_cp ~ PIB_cp
R2 <- lm( formula1 , DataC1)
R3 <- lm( TD_cp ~ ., DataC1)
library(estout)
estclear()
eststo(R1)
esttab(t.value = TRUE, sig.levels = c(0.1, 0.05, 0.01),
       sig.sym=c("*","**","***"),
       caption="Modelo estimado por MCO",
       caption.top=TRUE, label="res1",
       var.rename=c("(Intercept)","intercepto") )

vcov(R1)


