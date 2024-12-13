 setwd("C:/Users/Gilbert BC/Documents/Teletrabajo/regresion 2022/Labs/Lab 9/")



load("gastoocio3.Rdata")
attach(gastoocio3)
names(gastoocio3)
library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)


library(MASS)
##library(faraway)


###Sabemos que tenemos varios modelos con transformaciones distintas
###Sabemos que tenemos violaciones a la normalidad y homoscedasticidad con el modelo original
###Y que tenemos una transformación que nos corrige la heteroscedasticidad y no normalidad.
###Además, la regresión robusta nos indica que tenemos valores extremos que nos están afectando

###Bootstrapearemos los siguientes modelos reducidos:
###a) El original
###b) El transformado con raíz octava
###c) Un modelo robusto reducido


mod.reducido=lm(gasto_ocio~edadjefe+miembrospercep+ingresototal)
summary(mod.reducido)

gasto.lambda2=I(gasto_ocio^(1/8))

mod.reducido6=lm(gasto.lambda2~edadjefe+escoljefe+miembrospercep+ingresototal+propalim+pobmenores)
summary(mod.reducido6)



mod.huber.reducido=rlm(gasto_ocio~edadjefe+miembrospercep+ingresototal)
summary(mod.huber.reducido, cor=FALSE)



###Forma semiparamétrica de resolver problemas de falta de normalidad, 
###heteroscedasticidad, y existencia de valores influyentes

###Bootstrap

###Bootstrapear y comparar intervalos de confianza


summary(mod.reducido)
mod.reducido$coef

Lim.inf=mod.reducido$coef-qt(0.975,length(gasto_ocio)-length(mod.reducido$coef))*summary(mod.reducido)$sigma*(diag(summary(mod.reducido)$cov.unscaled))^0.5
Lim.sup=mod.reducido$coef+qt(0.975,length(gasto_ocio)-length(mod.reducido$coef))*summary(mod.reducido)$sigma*(diag(summary(mod.reducido)$cov.unscaled))^0.5

round(confint(mod.reducido),4)

round(cbind(Lim.inf,mod.reducido$coef,Lim.sup),4)

library(boot)

coefic=function(y,x,d) {
  lm(y[d]~x[d,])$coef
}

coef=boot(gasto_ocio,coefic,R=1000,x=cbind(edadjefe,miembrospercep,ingresototal))

boot.ci(coef,index=1, type="perc")
boot.ci(coef,index=2, type="perc")
boot.ci(coef,index=3, type="perc")
boot.ci(coef,index=4, type="perc")


round(rbind(boot.ci(coef,index=1, type="perc")$percent[,c(4,5)],boot.ci(coef,index=2, type="perc")$percent[,c(4,5)],boot.ci(coef,index=3, type="perc")$percent[,c(4,5)],boot.ci(coef,index=4, type="perc")$percent[,c(4,5)]),4)
round(confint(mod.reducido),4)



###Ahora bootstrap con tranformación

coef2=boot(gasto.lambda2,coefic,R=1000,x=cbind(edadjefe,escoljefe,miembrospercep,ingresototal,propalim,pobmenores))



round(rbind(boot.ci(coef2,index=1, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=2, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=3, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=4, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=5, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=6, type="perc")$percent[,c(4,5)],boot.ci(coef2,index=7, type="perc")$percent[,c(4,5)]),6)

round(confint(mod.reducido6),6)

###Sí se llegan a las mismas conclusiones al comparar los intervalos de confianza 
###teóricos y bootstrapeados

###Si uno quiere bootstrapear Huber entonces tiene que plantear una nueva función.

coefic.huber=function(y,x,d) {
  rlm(y[d]~x[d,])$coef
}

coef3=boot(gasto_ocio,coefic.huber,R=1000,x=cbind(edadjefe,miembrospercep,ingresototal))

boot.ci(coef3,index=1, type="perc")
boot.ci(coef3,index=2, type="perc")
boot.ci(coef3,index=3, type="perc")
boot.ci(coef3,index=4, type="perc")

Lim.inf.huber=mod.huber.reducido$coef-qt(0.975,length(gasto_ocio)-length(mod.huber.reducido$coef))*summary(mod.huber.reducido)$coefficients[,2]
Lim.sup.huber=mod.huber.reducido$coef+qt(0.975,length(gasto_ocio)-length(mod.huber.reducido$coef))*summary(mod.huber.reducido)$coefficients[,2]




round(rbind(boot.ci(coef3,index=1, type="perc")$percent[,c(4,5)],boot.ci(coef3,index=2, type="perc")$percent[,c(4,5)],boot.ci(coef3,index=3, type="perc")$percent[,c(4,5)],boot.ci(coef,index=4, type="perc")$percent[,c(4,5)]),4)
round(cbind(Lim.inf.huber,Lim.sup.huber),4)





