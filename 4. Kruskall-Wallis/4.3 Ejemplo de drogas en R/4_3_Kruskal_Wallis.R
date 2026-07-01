library(readr)
drugstudy <- read_csv("4. Kruskall-Wallis/4.3 Ejemplo de drogas en R/drugstudy.csv")
View(drugstudy)

attach(drugstudy)
dose=as.factor(dose)

####### Kruskal-Wallis
k=kruskal.test(rating ~ dose, data = drugstudy)

p_value_thresh=0.05

k$statistic
if(k$p.value > p_value_thresh){
  cat('pvalor =', k$p.value, "\n")
  cat('No rechazamos H0: No hay diferencias significativas entre los grupos')
} else {
  cat('pvalor =', k$p.value, "\n")
  cat('Rechazamos H0: Hay diferencias significativas entre los grupos')
}

####### Comparaci?n de pares de grupos con Mann-Whitney
# Bonferroni: 6 comparaciones
alpha_new=0.05/6
alpha_new

pairwise.wilcox.test(rating, dose, p.adj = "bonf")

#Conclusi?n:
#Seg?n la prueba de Kruskal-Wallis de comparaci?n de grupos independientes 
#existen diferencias significativas entre los valores reportados de dolor 
#de acuerdo a la dosis recibida del medicamento.

#Seg?n las pruebas Mann-Whitney de pares de grupos hay diferencias significativas
#entre los grupos:
  
#1 y 3 (100mg vs 500mg)
#1 y 4 (100mg vs 1000mg)

#El dolor reportado por los pacientes no difiere significativamente 
#entre los grupos que tomaron dosis de 250mg, 500mg y 1000mg.



