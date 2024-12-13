setwd("C:/Users/Usuario/Downloads")
load("antropometria.Rdata")
attach(antropometria)
datos <-antropometria[(anyomes==200504 | anyomes==200501 |anyomes==200608),]
attach(datos)
datos <- na.omit(datos)
attach(datos)

class(datos$smoker)

datos$smoker <- as.factor(datos$smoker)
datos$hombre <- as.factor(datos$hombre)

class(datos$hombre)

mod.grande <- lm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
summary(mod.grande)


shapiro.test(mod.grande$residuals)
install.packages("faraway")

library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)
library(leaps)
library(faraway)

library(MASS)

library(car)
library(olsrr)
library(qpcR)


ncvTest(mod.grande)
shapiro.test(mod.grande$residuals)

AIC(mod.grande)
PRESS(mod.grande, verbose=FALSE)$P.square


(press.grande=ols_press(mod.grande))
press.


bcobjeto=boxcox(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)

cbind(bcobjeto$x,bcobjeto$y)

bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

espirom_lambda <- I(espirom^(0.55))

mod.grande2 <- lm(espirom_lambda ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
summary(mod.grande2)

shapiro.test(mod.grande2$residuals)
ncvTest(mod.grande2)
AIC(mod.grande2)
PRESS(mod.grande2, verbose=FALSE)$P.square




spreadLevelPlot(mod.grande)
espirom_lambda2 <- I(espirom^(0.83))
mod.grande3 <- lm(espirom_lambda2 ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
summary(mod.grande3)


shapiro.test(mod.grande3$residuals)
ncvTest(mod.grande3)
AIC(mod.grande3)
PRESS(mod.grande3, verbose=FALSE)$P.square


mod.heter=dglm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre, 
               dformula~age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre, family=gaussian)
summary(mod.heter)

shapiro.test(mod.heter$residuals)
bptest(mod.heter)
AIC(mod.heter)
(AIC.mod.heter <- 2319.81 +2*26)
PRESS(mod.heter, verbose=FALSE)$P.square



mod.huber=rlm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
summary(mod.huber, cor=FALSE)
###El tcalculado se compara con el t tabular
qt(0.975,187)
qt(0.025,187)

shapiro.test(mod.huber$residuals)
ncvTest(mod.huber)
AIC(mod.huber)
PRESS(mod.huber, verbose=FALSE)$P.square



abs.res1=abs(residuals(mod.grande))
mod.ponde1=lm(abs.res1~age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
ponde1=(1/abs(fitted(mod.ponde1)))
             
           mod.grande4=lm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre, weights=I(ponde1))
             summary(mod.grande4)
             round(cbind(mod.grande$coef,mod.grande4$coef),4)


             abs.res2=abs(residuals(mod.grande4))
             mod.ponde2=lm(abs.res2~age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
             ponde2=1/abs(fitted(mod.ponde2))
             
             mod.grande5=lm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre, weights=I(ponde2))

             round(cbind(mod.grande$coef,mod.grande4$coef,mod.grande5$coef),4)

             summary(mod.grande5)           

shapiro.test(mod.grande5$residuals)             
ncvTest(mod.grande5)
AIC(mod.grande5)
PRESS(mod.grande5, verbose=FALSE)$P.square

mod.grande <- lm(espirom ~ age+cinturar+caderar+grip+pantorrilla+brazo+tricipital+subscap+pesokg+tallacm+smoker+hombre)
summary(mod.grande)


modelo.reducido= step(mod.grande)
summary(modelo.reducido)
shapiro.test(modelo.reducido$residuals)
ncvTest(modelo.reducido)
AIC(modelo.reducido)
PRESS(modelo.reducido, verbose=FALSE)$P.square


bcobjeto=boxcox(espirom ~ age+grip+tricipital+tallacm)
cbind(bcobjeto$x,bcobjeto$y)

bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

espirom_lambda <- I(espirom^(0.55))

mod.reducido21 <- step(mod.grande2)
mod.reducido2 <- lm(espirom_lambda ~ age+grip+tricipital+tallacm)
summary(mod.reducido2)
shapiro.test(mod.reducido21$residuals)
ncvTest(mod.reducido2)
AIC(mod.reducido2)
PRESS(mod.reducido2, verbose=FALSE)$P.square

mod.reducido3 <- step(mod.grande3)
summary(mod.reducido3)

spreadLevelPlot(modelo.reducido)
spreadLevelPlot(modelo.reducido)
espirom_lambda2 <- I(espirom^(0.87))
mod.reducido3 <- lm(espirom_lambda2 ~ age+grip+tricipital+tallacm)
summary(mod.reducido3)
shapiro.test(mod.reducido3$residuals)
ncvTest(mod.reducido3)
AIC(mod.reducido3)
PRESS(mod.reducido3, verbose=FALSE)$P.square




mod.red.heter=dglm(espirom ~ age+grip+tricipital+tallacm, 
               dformula~age+grip+tricipital+tallacm, family=gaussian)




summary(mod.red.heter)
shapiro.test(mod.red.heter$residuals)
bptest(mod.red.heter)

(AIC.mod.red.heter <- 2335.521 +2*10)
PRESS(mod.red.heter, verbose=FALSE)$P.square





mod.red.huber=rlm(espirom ~ age+grip+tricipital+tallacm)
summary(mod.red.huber, cor=FALSE)
###El tcalculado se compara con el t tabular
qt(0.975,195)
qt(0.025,195)

shapiro.test(mod.red.huber$residuals)
ncvTest(mod.red.huber)
AIC(mod.red.huber)
PRESS(mod.red.huber, verbose=FALSE)$P.square




abs.res1=abs(residuals(modelo.reducido))
mod.ponde1=lm(abs.res1~age+grip+tricipital+tallacm)
ponde1=(1/abs(fitted(mod.ponde1)))

mod.red.3=lm(espirom ~ age+grip+tricipital+tallacm, weights=I(ponde1))
summary(mod.red.3)
round(cbind(modelo.reducido$coef,mod.red.3$coef),4)




shapiro.test(mod.red.3$residuals)
ncvTest(mod.red.3)
AIC(mod.red.3)
PRESS(mod.red.3, verbose=FALSE)$P.square

vif(mod.reducido2)
plot(mod.reducido2$residuals, mod.reducido2$fitted.values)


par(mfrow=c(2,2))
plot(espirom ~ age)
abline(lm(espirom~age),col="red")
plot(espirom ~ grip)
abline(lm(espirom~grip),col="red")
plot(espirom ~ tricipital)
abline(lm(espirom~tricipital),col="red")
plot(espirom ~ tallacm)
abline(lm(espirom~tallacm),col="red")

