# Error est??ndar de beta - simulaci??n

sim.beta=function(a,b,sig,n,min=1,max=10){
  x=runif(n,min,max)
  y=a+b*x+rnorm(n,0,sig)  
  return(list("x"=x,"y"=y))
}

a=2; b=3; sig=5; n=10

s1=sim.beta(a,b,sig,n)
s1

summary(lm(s1$y~s1$x))
anova(lm(s1$y~s1$x))


plot(s1$x,s1$y)
mod=lm(s1$y~s1$x)
abline(mod,col=2)
summary(mod)      #observe el error est??ndar de la pendiente

plot(0,0,xlim=c(0,10),ylim=c(0,40),xlab="x",ylab="y",type="n")

nrep=1000
b1=numeric(nrep)

for(i in 1:nrep){
  s1=sim.beta(a,b,sig,n)
  abline(lm(s1$y~s1$x),col="grey")
  b1[i]=lm(s1$y~s1$x)$coef[2]
}
abline(a,b,col=2,lwd=2)

hist(b1)

sd(b1)           #compare este valor con el error est??ndar anterior

hist(b1)

