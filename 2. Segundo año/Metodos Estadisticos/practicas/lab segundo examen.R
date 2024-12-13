attach(restaurant)
plot(cambio.emp,calific.serv)
qqplot(cambio.emp,calific.serv)

cor.test(cambio.emp, calific.serv, use= "pairwise.complete.obs", method="pearson")
cor.test(cambio.emp, calific.serv, use= "pairwise.complete.obs", method="kendall")

dif= cambio.emp - calific.serv
shapiro.test(dif)

cor.test(cambio.emp, calific.serv, use= "pairwise.complete.obs", method="spearman")

cor.test(cambio.emp, calific.serv, use= "pairwise.complete.obs", method="kendall")

muestras=c(15,100)
alfa= 0.05
for (i in 1:2 ){
  almacenwilcoxon_rech = rep(NA, 5000)
  for (j in 1:5000){
     var1= rnorm(muestras[i], mean = 5, sd = 1)
    var2= rnorm(muestras[i], mean = 5.5, sd = 2)
    
    prueba= wilcox.test(var1, var2, paired = T, correct= F, alternative="two.sided")
    almacenwilcoxon_rech[j]= 1* (prueba$p.value < alfa)
  }
   print(mean(almacenwilcoxon_rech))
}
