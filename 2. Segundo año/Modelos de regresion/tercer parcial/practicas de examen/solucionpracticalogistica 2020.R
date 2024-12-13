###Pr?ctica



load("E:/regresion 2017/Practicas/Practica Poisson y logistica de regresi?n\/crime1.Rdata")

attach(crime1)

##1.	 Cree una variable que se llame lognarr que sea igual al logaritmo natural de (narr86+1).
hist(narr86)
lognarr=log(narr86+1)
hist(lognarr)


###2.	Estime un modelo gaussiano en el que lognarr est? en funci?n de pcnv, ptime86, inc86, black y hispan

mod.gauss=lm(lognarr~ pcnv+ ptime86+ inc86+ black+ hispan)

summary(mod.gauss)

###3.	Con la funci?n aic(), c?lculele el AIC

AIC(mod.gauss)



###11.	Recodifique la variable narr86 en 0=0 y 1= Todo lo dem?s, y dicho resultado p?ngalo en una variable llamada arrestado

library(car)
arrestado=recode(narr86,"0=0;1:hi=1")
arrestado <- ifelse(narr86==0,0,1)
arrestado <- 1*(narr86>0)

table(arrestado)

##4.	Estime un modelo log?stico en el que la variable arrestado sea predicha por pcnv, ptime86, inc86, black y hispan.

mod.logist=glm(arrestado~ pcnv+ ptime86+ inc86+ black+ hispan, 
               family=binomial(link=logit))
summary(mod.logist)
AIC(mod.logist)


###13.	Llene la siguiente tabla

cuadro1 <- round(cbind(exp(mod.gauss$coeff),summary(mod.gauss)$coeff[,4],exp(mod.logist$coeff),summary(mod.logist)$coeff[,4]),3)

colnames(cuadro1) <- c("Gauss exp(beta)","Gauss pvalue","Logist OR","Logist pvalue")

cuadro1
###No es necesaria en el script

###6.	Compare ambos modelos.  Llega a conclusiones distintas con la regresi?n gaussiana y con la regresi?n log?stica?


##Igual

###7.	Interprete los Odds Ratio para inc86 e hispan.

exp(mod.logist$coeff)

###Igual

###16.	Pruebe la bondad de ajuste del modelo con una prueba de Hosmer y Lemeshow, al 5%.


library(ResourceSelection)
###H0: El modelo tiene un buen ajuste a los datos
###H1: El modelo tiene un mal ajuste a los datos


hlt=hoslem.test(arrestado,mod.logist$fitted,g=10)
hlt



####9.	Analice la capacidad predictiva del modelo con una tabla de clasificaci?n.

clasif=recode(mod.logist$fitted,"0:0.5=0;0.5:1=1")
table(clasif,arrestado)/length(arrestado)
0.687+0.033

mean(arrestado)

clasif2=recode(mod.logist$fitted,"0:mean(arrestado)=0;mean(arrestado):1=1")
table(clasif2,arrestado)/length(arrestado)
0.419+0.193

###10.	Haga un gr?fico de residuos de deviancia vs. leverage, identificados por n?mero de arrestos.  ?Tiene los mismos valores extremos e influyentes que en la regresi?n de Poisson?

dev.logist=residuals(mod.logist)
lev.logist=hatvalues(mod.logist)

plot(dev.logist,lev.logist)
identify(dev.logist,lev.logist,narr86)


##11.	Haga un listado de las caracter?sticas de los casos que tengan un leverage extremo o un residuo extremo (Ud. Decide el l?mite), y describa a esos casos.

crime1[dev.logist>2.5,c(1,4,7,9,11,12)]
crime1[lev.logist>0.02,c(1,4,7,9,11,12)]
###12.  Comparar con bootstrap




library(boot)

coefic=function(y,x,d) {
  lm(y[d]~x[d,])$coef
}

coef=boot(lognarr,coefic,R=1000,x=cbind(pcnv,ptime86, inc86, black, hispan))


round(rbind(boot.ci(coef,index=1, type="perc")$percent,boot.ci(coef,index=2, type="perc")$percent,boot.ci(coef,index=3, type="perc")$percent,boot.ci(coef,index=4, type="perc")$percent,boot.ci(coef,index=5, type="perc")$percent,boot.ci(coef,index=6, type="perc")$percent),4)
round(confint(mod.gauss),4)
