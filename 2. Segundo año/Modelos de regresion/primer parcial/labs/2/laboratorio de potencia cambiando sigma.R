almacen.ee=rep(NA,4)
errores=c(5,10,15,20)
for (i in 1:4) {
  
  almacen.pvalue=rep(NA,500)
  error=5*i
  
  for (j in 1:500) {
    
    x1=rnorm(12,mean=0,sd=1)
    y1=3+4*x1+rnorm(12,mean=0,sd=errores[i])
    
    mod=lm(y1~x1)
    almacen.pvalue[j]=1*(summary(mod)$coef[2,4]<0.05)
    
  }
  
  almacen.ee[i]=mean(almacen.pvalue)
 
}

barplot(almacen.ee,names.arg =c(5,10,15,20), 
        xlab="Errores estandar", ylab="Potencia prueba Beta1=0", 
        ylim=c(0,1.0))



