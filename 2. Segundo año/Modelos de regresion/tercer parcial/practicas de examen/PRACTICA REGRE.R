attach(aves)
names(aves)

library(car)
library(lmtest)
library(tseries)
library(e1071)
library(dglm)
library(MASS)
library(boot)
library(foreign)

#########Pequeña practica examen 3 2020 ################################################################3

#pregunta a
mode1= lm(pajaros~ tamanyo+ancho+estacion)
summary(mode1)

#pregunta b
shapiro.test(mode1$residuals)
qqPlot(mode1$residuals)

#pregunta c
boxcox(pajaros~ tamanyo+ancho+estacion)
bcobjeto=boxcox(pajaros~ tamanyo+ancho+estacion)
cbind(bcobjeto$x,bcobjeto$y)
bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]
pajaros1=I(1/(pajaros^(0.1)))
mode2=lm(pajaros1~ tamanyo+ancho+estacion)
summary(mode2)
shapiro.test(mode2$residuals)
qqPlot(mode2$residuals)

#preguta d
plot(mode2$fitted.values, mode2$residuals)
abline(coef(lm(mode2$residuals~mode2$fitted.values )))

#pregunta e
summary(mode1)
mode1$coef

Lim.inf=mode1$coef-qt(0.975,length(pajaros)-length(mode1$coef))*summary(mode1)$sigma*(diag(summary(mode1)$cov.unscaled))^0.5
Lim.sup=mode1$coef+qt(0.975,length(pajaros)-length(mode1$coef))*summary(mode1)$sigma*(diag(summary(mode1)$cov.unscaled))^0.5
round(cbind(Lim.inf,mode1$coef,Lim.sup),4)
# otra manera de calcularlos
round(confint(mode1),4)

# pregunta f
coefic=function(y,x,d) {
  lm(y[d]~x[d,])$coef
}
coef=boot(pajaros,coefic,R=1000,x=cbind(tamanyo,ancho,estacion))
boot.ci(coef,index=1, type="perc")
boot.ci(coef,index=2, type="perc")
boot.ci(coef,index=3, type="perc")
boot.ci(coef,index=4, type="perc")
round(rbind(boot.ci(coef,index=1, type="perc")$percent[,c(4,5)],boot.ci(coef,index=2, type="perc")$percent[,c(4,5)],boot.ci(coef,index=3, type="perc")$percent[,c(4,5)],boot.ci(coef,index=4, type="perc")$percent[,c(4,5)]),4)



######### Practica examen 3 2020 ############################################################################################

attach(cigarros)
names(cigarros)

#pregunta 1

par(mfrow=c(2,2))
plot(cigarros$cigarros~cigarros$deprescale)
abline(lm(cigarros$cigarros~cigarros$deprescale))
plot(cigarros$cigarros~cigarros$hatcalor)
abline(lm(cigarros$cigarros~cigarros$hatcalor))
plot(cigarros$cigarros~cigarros$hatalco)
abline(lm(cigarros$cigarros~cigarros$hatalco))

#pregunta 3
mode3 = lm(cigarros$cigarros~ deprescale+hatcalor+hatalco )
summary(mode3)

#pregunta 5
estandariz = rstandard(mode3)
leverage = hatvalues(mode3)
plot(estandarizados, leverage)
abline(h=0, col=6)
text(estandariz[leverage>0.15],leverage[leverage>0.15],cigarros$cigarros[leverage>0.15])
text(estandariz[estandariz>2],leverage[estandariz>2],cigarros$cigarros[estandariz>2])

#pregunta 6

#pregunta 7
bptest(mode3)

#pregunta 8
shapiro.test(mode3$residuals)

#pregunta 9 
boxcox(cigarros$cigarros~ deprescale+hatcalor+hatalco)
bcobjeto=boxcox(cigarros$cigarros~ deprescale+hatcalor+hatalco)
cbind(bcobjeto$x,bcobjeto$y)
bcobjeto$x[bcobjeto$y==max(bcobjeto$y)]

#pregunta 10
cigarros1=I(cigarros$cigarros^(1/5))
mode4=lm(cigarros1~ deprescale+hatcalor+hatalco)
summary(mode4)
summary(mode3)

#pregunta 11
plot(mode4$residuals, mode4$fitted.values)

#pregunta 12
bptest(mode4)


######### Practica 2 de regresion solo logistica ###############################################

attach(crime1)
names(crime1)

#pregunta 1
lognarr = log(narr86+1)
lognarr

#pregunta 2
mode5= lm(lognarr~ pcnv+ptime86+inc86+black+hispan)
summary(mode5)

#pregunta3
AIC(mode5)

#pregunta 4

len = nrow(crime1)
crime1$arrestados = 0
for (i in (1:len)){
  if (crime1[i,1]== 0){
    crime1[i,17] =0
  } else {
    crime1[i,17]=1
  }
}
mode8 = glm(crime1$arrestados~ pcnv+ptime86+inc86+black+hispan, family = binomial(link = logit))
summary(mode8)
AIC(mode8)
length(crime1$arrestados)
length(pcnv)

#pregunta 5
exp(mode5$coefficients)
exp(mode8$coefficients)


#pregunta 8
library(ResourceSelection)

anova(mode8,test="LRT")
deviance(mode8)
####H0: El modelo tiene un buen ajuste a los datos
hlt=hoslem.test(crime1$arrestados,mode8$fitted,g=10)
hlt








######### Manual de practica III parcial preguntas ####################################################################