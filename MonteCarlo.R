setwd("C:/Users/patur/OneDrive/Bureau/ENSTA/ENSTA Cours/STA/STA211/MonteCarlo")

library(ggplot2)
#Q1
set.seed(100)
M=10000
L1=rnorm(n=10000,mean=0,sd=sqrt(0.5))
L2=rnorm(n=10000,mean=0,sd=sqrt(0.5))

IM=data.frame(id=1:M)
IM$valeurs=(pi*cos(L1)^2*sin(3*L2)^2)
mean(IM$valeurs)
mean(IM$valeurs[1:1000])
for (i in 1:M)
{ 
  IM$estimateur[i]=mean(IM$valeurs[1:i])
}
ggplot(data = IM)+aes(x = id,y = estimateur)+geom_line()

#Estimation de la variance :
estimation_variance=var(IM$estimateur)
for (i in 1:M)
{ 
  IM$estimation_variance[i]=mean((IM$valeurs[1:i]-IM$estimateur[i])^2)
}

#Intervalle de confiance à 5%
z_975 <- qnorm(0.975, 0, 1) 
for (i in 1:M)
{ 
  IM$IC_95_inf[i]=IM$estimateur[i]-z_975*sqrt(IM$estimation_variance[i]/i)
  IM$IC_95_sup[i]=IM$estimateur[i]+z_975*sqrt(IM$estimation_variance[i]/i)
  
}

ggplot(data=IM,aes(x = id)) + geom_ribbon(aes(ymin = IC_95_inf, ymax = IC_95_sup),fill = "lightblue") + geom_line(aes(y = estimateur))  # Add the estimate line 

###Exo2
library(purrr)
set.seed(100)
X=rdunif(1000,0,1)

M=50000
X=rnorm(M,mean=1,sd=0)
IM=data.frame(Id=1:M)
IM$Valeurs=(sin(10*X)^2*)
(Esperance=mean(IM$Valeurs))
(variance=var(IM$Valeurs))
  
