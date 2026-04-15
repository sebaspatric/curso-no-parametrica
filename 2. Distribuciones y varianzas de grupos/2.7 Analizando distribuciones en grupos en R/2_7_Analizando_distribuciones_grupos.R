library(tidyverse)
library(broom)
theme_set(theme_classic())
library(readr)
iq <- read_csv("2. Distribuciones y varianzas de grupos/2.7 Analizando distribuciones en grupos en R/iq.csv")
View(iq)


grupo1=iq[iq$School==1,]
grupo2=iq[iq$School==2,]
  

res1=grupo1$IQ-mean(grupo1$IQ)
res2=grupo2$IQ-mean(grupo2$IQ)

#Shapiro-Wilks
x.test1 <- shapiro.test(res1)
x.test1
x.test2 <- shapiro.test(res2)
x.test2
p_value_thresh=0.05


#Definiendo la funci?n que me devuelve si se cumple o no la hipotesis
sw_test_results <- function(x.test,p_value_thresh) {
  if(x.test$p.value > p_value_thresh){
    print('Assumption satisfied')
  } else {
    print('Assumption not satisfied')
    print('Confidence intervals will likely be affected')
    print('Try performing nonlinear transformations on variables')
  }
  
}

#Definiendo la funci?n del grafico de distribucion Normal
plotn <- function(x,main="Histograma de frecuencias \ny distribuci?n normal",
                  xlab="X",ylab="Densidad") {
  min <- min(x)
  max <- max(x)
  media <- mean(x)
  dt <- sd(x)
  hist(x,freq=F,main=main,xlab=xlab,ylab=ylab)
  curve(dnorm(x,media,dt), min, max,add = T,col="blue")
}

#Probando la funcion sobre res1 
sw_test_results(x.test1,p_value_thresh)
plotn(res1,main="Distribuci?n normal")
#Probando la funcion sobre res2 
sw_test_results(x.test2,p_value_thresh)
plotn(res2,main="Distribuci?n normal")

#########################################
#Otro tipo de estimaci?n de densidad
#Res1
df <- data.frame(
  res1
)
# Histograma con density plot
ggplot(df, aes(x=res1)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

#Res2
df <- data.frame(
  res2
)
# Histograma con density plot
ggplot(df, aes(x=res2)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

