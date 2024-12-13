mod.grande=lm(sr~pop15+pop75+dpi+ddpi, data=ahorro)
summary(mod.grande)

###Hagan un grafico de dispersion de los coeficientes de determinacion 
###parcial de cada variable vs. coeficientes estandarizados.

mod.sin.pop15=lm(sr~pop75+dpi+ddpi, data=ahorro)

(r2.pop15=(anova(mod.sin.pop15)[4,2]-anova(mod.grande)[5,2])/anova(mod.sin.pop15)[4,2])

(r2.pop15.alt=1-anova(mod.grande)[5,2]/anova(mod.sin.pop15)[4,2])



mod.sin.pop75=lm(sr~pop15+dpi+ddpi, data=ahorro)

(r2.pop75=(anova(mod.sin.pop75)[4,2]-anova(mod.grande)[5,2])/anova(mod.sin.pop75)[4,2])

mod.sin.dpi=lm(sr~pop15+pop75+ddpi, data=ahorro)

(r2.dpi=(anova(mod.sin.dpi)[4,2]-anova(mod.grande)[5,2])/anova(mod.sin.dpi)[4,2])

mod.sin.ddpi=lm(sr~pop15+pop75+dpi, data=ahorro)

(r2.ddpi=(anova(mod.sin.ddpi)[4,2]-anova(mod.grande)[5,2])/anova(mod.sin.ddpi)[4,2])

mod.est=lm(scale(sr)~scale(pop15)+scale(pop75)+scale(dpi)+scale(ddpi), data=ahorro)

det.parcial=c(r2.pop15,r2.pop75,r2.dpi,r2.ddpi)

plot(det.parcial,abs(mod.est$coef[-1]))

det.parcial

