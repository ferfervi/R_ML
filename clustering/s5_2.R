############################################################
#
# Sesion 5 - MBD
#
############################################################

############################################################
#
# Lectura de datos
#
############################################################
##-- 1. Borra todo lo que hay en memoria y lee los datos de entrenamiento
rm(list=ls())
setwd('/Users/Fer/Development/MASTERBD/ESTADISTICA/E_CLUSTERING/practica/')     
datos2 <- read.csv('p3_train.csv',sep=";")

############################################################
#
# Inspeccion de los datos
#
############################################################
##-- 2. Mira los 100 primeros registros, haz la descriptiva de todas las variables y elimina las variables "id" y "target"
View(head(datos2,100))
summary(datos2)

myIdsList <-datos2$id
datos2$id <- NULL
datos2$target <- NULL

############################################################
#
# Algoritmo de K-means
#
############################################################
##-- 3. Pon una semilla (un n?mero cualquiera) para siempre obtener los mismos resultados a partir de aqui
set.seed(12345)

##-- 4. Usa el algoritmo de k-means para hacer 2 grupos con 10 puntos de inicio
##-- Si tarda mucho. Haz alguna de estas opciones: (1) reduce los puntos de inicio; (2) escoge una submuestra de datos2; o (3) usa el paquete bigkmeans
km2 <- kmeans(datos2,centers=2,nstart=10)

#evaluamos la calidad del clustering con 2 grupos o clusters
inercia_km2 <- km2$tot.withinss  # Variabilidad intra-grupo (Inercia)
inercia_km2 #->21384032. a menor variabilidad intra-grupo más omogenos son los grupos, mejor agrupación.

#EVALUACION: variabilidad explicada. Variabilidad entre groups / variabilidad total
variabilidad_Explicada_km2 <-100*km2$betweenss/km2$totss
variabilidad_Explicada_km2 # 7.267007

##-- 5. Calcula el % de variabilidad explicada dividiendo la variabilidad entre grupos entre la variabilidad total
(I <-100*km2$betweenss/km2$totss)

##-- 6. Para escoger el numero de grupos, haz un bucle y calcula las variabilidades explicadas 
##-- de 2 a 20 grupos con 5 puntos (o menos si va muy lento) de inicio
set.seed(12345)                                 # Semilla
I <- c()                                      # Vector donde se guardaran los porcentajes de variabilidad
for(g in 2:20){                               # bucle
  km <- kmeans(datos2,centers=g,nstart=5) # Empezar cogiendo nstart bajo por si tarda mucho
  I[g] <-100*km$betweenss/km$totss                   # Guardar en vector el % de Variabilidad explicada
  print(g)                                    # Printar progreso
}

##-- 7. Haz el grafico de las variabilidades explicadas en funcion del numero de grupos y
##-- decide con cuantos grupos te quedas
#plot(...,type="b",pch=19)                     # pch=19 es un punto salido
plot(I,type="b",pch=19,xlab="Numero de grupos",ylab="Variabilidad explicada")


##-- 8. Para el numero de grupos escogido, ejecuta el k-means con 10 puntos de inicio

## Elegimos 11 grupos siguiendo la regla "del codo", en la que parece que a partir
## de 11 grupos la variabilidad explicada aumenta mucho menos.
set.seed(12345)                                 # semilla
km.def <- kmeans(datos2,centers=11,nstart=10)  # aplicar kmeans para el n?mero escogido de grupos


##-- 9. Crea una paleta con suficientes colores para representar tus datos (deja tantos colores como grupos)
colors <- c("black","red","green3","blue","cyan","magenta","darkgray",  
            "darkgoldenrod","darkgreen","violet","turquoise")



palette(c("black","red","green3","blue","cyan","magenta","darkgray",  
          "darkgoldenrod","darkgreen","violet","turquoise","orange","lightpink","lavender",  
          "yellow","lightgreen","lightgray","lightblue","darkkhaki","darkmagenta","darkolivegreen",
          "lightcyan","darkorange","darkorchid","darkred","darksalmon","darkseagreen","darkslateblue",
          "darkslategray","darkslategray","darkturquoise","darkviolet","lightgray","lightsalmon","lightyellow"))

palette(colors)

##-- 10. La instruccion princomp realiza un analisis de componentes principales que reduce las dimensiones del conjunto
##-- de datos original. Aplicala y representa las 2 primeras dimensiones (scores) obtenidas con colores para los clusters
pc <- princomp(datos2)
par(mfrow=c(1,1))
x <- pc$scores[,1]                    # Primera coordenada
y <- pc$scores[,2]                  # Segunda coordenada
colores <- km.def$cluster                     # Haz names(km) y mira que objeto guarda el identificador de los grupos para ponerlo como color
plot(x,y,pch=19,col=km.def$cluster,cex=0.7) # cex=0.7 indica el tamanyo de los puntos

##-- 10. Representa todas las combinaciones de parejas de las 4 primeras dimensiones usando un bucle
##-- (1 vs 2, 1 vs 3, 1 vs 4, 2 vs 3, 2 vs 4, 3 vs 4). Marca los grupos con colores
par(mfrow=c(3,2))                     # 6 ventanas en una
for(i in 1:3){
  for(j in (i+1):4){
    plot(pc$scores[,i],pc$scores[,j],pch=19,col=km.def$cluster,cex=0.7,xlab=paste("dimension ",i), ylab=paste("dimension ",j))
  }
}

## EVALUACION clustering: calculamos la inercia y variabilidad explicada

#print km.def para saber las variables disponibles
#km.def[1]                              # Clusters
#km.def[2]                              # Centos de los clusters                              
#km.def[3]                              # Variabilidad total
#km.def[4]                              # Variabilidad para cada grupo
#km.def[5]                              # Variabilidad intra-grupo (Inercia)
#km.def[6]                              # Variabilidad entre-grupo
#km.def[[6]]/(#km.def[[3]])                 # Variabilidad explicada

inercia_km.def <- km.def$tot.withinss  # Variabilidad intra-grupo (Inercia)
inercia_km.def #->14373067. a menor variabilidad intra-grupo más omogenos son los grupos, mejor agrupación.

#EVALUACION: variabilidad explicada. Variabilidad entre groups / variabilidad total
variabilidad_Explicada_km.def <-100*km.def$betweenss/km.def$totss
variabilidad_Explicada_km.def # 37.67

# Guartar en un fichero -> (id_producto,grupo asignado)
df <- data.frame(product_id = myIdsList , cluster=km.def$cluster)
write.table(df,'resultados_p3_partA.txt',sep=',',dec='.',col.names=c("product_id","cluster"),row.names=TRUE,quote=FALSE,append=FALSE)
quartz()
plot(df)

# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(df$cluster)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
main="Pie Chart of Groups\n (amount of observations per group)")


############################################################
#
# Clusterizacion jerarquica --> No es idonea para grandes conjuntos de datos
#
############################################################
##-- 11. Calcula las distancias con dist para los 100 primeros individuos y haz la jerarquia con hc
##-- Especifica los metodos que quieras mirando ?dist y ?hclust
##-- Recomendados: "euclidean" para dist y "ward.D" para hclust
nmax <- 100
d <- dist(datos2[1:nmax,], method = "euclidean")        # matriz de distancias
hc <- hclust(d,method = "ward.D")                    # jerarquizaci?n

##-- 12. Representa graficamente la jerarquia  
#windows(14,7)

par(mfrow=c(1,1))
plot(hc,cex=0.7)
rect.hclust(hc,k=3,border="red")

#install.packages("NbClust")
library(NbClust)
NbClust(datos2[1:nmax,],method='complete',index="dindex")

##-- 13. Viendo la jerarquizacion, escoge el numero de grupos idoneo
ct <- cutree(hc, k = 3)

##-- 14. Representa la clusterizacion hallada para todos los pares de las 4 primeras dimensiones 
par(mfrow=c(2,3))
for(i in 1:3){
  for(j in (i+1):4){
    #plot(pc$scores[...,...],pc$scores[...,...],pch=19,col=...)
    plot(pc$scores[,i],pc$scores[,j],pch=19,col=ct,cex=0.7,xlab=paste("dimension ",i), ylab=paste("dimension ",j))
  }
}

#SEGUNDA PARTE!!
############################################################
#
# Algoritmos supervisados
#
############################################################
############################################################
#
# Conditional trees
#
############################################################
##-- 15. Se vuelven a leer los datos sin eliminar la respuesta y eliminando la variable id
setwd('/Users/Fer/Development/MASTERBD/ESTADISTICA/E_CLUSTERING/practica/')     
datos2 <- read.table('p3_train.csv',header=TRUE,sep=';')
datos2$id <- NULL

##-- 16. Instala y carga el paquete party para calcular arboles condicionales
#install.packages('party')
library("party")

##-- 17. Calcula el arbol con los parametros dados (pueded tardar)
ct <- ctree(target~.,datos2,control = ctree_control(mincriterion = 0.95,   # pvalue<0.05 para hacer una particion 
                                                         minsplit = 50,    # minimo tama?o de un nodo para hacer una particion
                                                         minbucket = 20,   # minimo tama?o de un nodo (minbucket<minsplit/2)
                                                         maxdepth = 2))    # Profundidad del arbol

##-- 18. Dibuja el arbol
#windows(30,10,rescale="fixed")  # rescale="fixed" no adapta la ventana al tama?o de la pantalla
quartz()
plot(ct)                        # arbol

##-- 19. Para la variable que mas discrima (raiz del arbol), create una variable segun este criterio de discriminacion
##-- Haz un mosaicplot de esta variable y la respuesta. Cual es la categoria de la respuesta que predice esta variable?
graphics.off()
var.discrimina <- datos2$feat_34 <= 5
mosaicplot(table(datos2$target,var.discrimina),col=2:3,las=2)


##-- 20. Lee el fichero que contiene la muestra test
test <- read.table('p3_test (con respuesta).csv',header=TRUE,sep=';')


##-- 21.Mira el % de aciertos que se obtiene con este arbol
pr <- predict(ct,test)          # Predicciones de las clases
(t <- table(pr,test$target))           # Tabla de prediciciones vs valor real
100*sum(diag(t))/sum(t)         # % de aciertos


##-- 22. Crea tu propio arbol modificando los parametros a tu gusto (puedes a?adir otros --> ?ctree_control)
##-- Modificalos poco a poco porque algunos arbole tienen un coste computacional alto
ct1 <- ctree(target~.,datos2,control = ctree_control(mincriterion = 0.95,             
                                                    minsplit = 14,
                                                    minbucket = 7,
                                                    maxdepth = 50))

##-- 23. Realiza la prediccion sobre la muestra test y comprueba si mejoras el nivel de aciertos
pr1 <- predict(ct1,test)          # Predicciones de las clases
(t1 <- table(pr1,test$target))           # Tabla de prediciciones vs valor real
100*sum(diag(t1))/sum(t1)         # % de aciertos

##-- 24. Representa en dos graficos las dos primeras dimensiones del princomp para las 1000 primeras observaciones
##-- En el primero, con colores segun las predicicones y con un simbolo diferente dependiendo de si se acertado o no
##-- En el segundo con los colores segun los valores reales
x <- pc$scores[1:1000,1]                # Primera coordenada
y <- pc$scores[1:1000,2]                   # Segunda coordenada
colores1 <- as.numeric(pr1)               # predicciones pasadas a numeros
colores2 <- as.numeric(test$target)               # Valores reales pasados a numeros
par(mfrow=c(1,2))
plot(x,y,col=colores1,cex=0.7,pch=ifelse(pr1==test$target,19,4),main="Predicciones") # pch=19 (punto); pch=4 (cruz)
plot(x,y,col=colores2,cex=0.7,pch=19,main="Reales")

##-- 25. Calcula las predicciones de las probabilidades para cada categoria y mira las 6 primeras predicciones
pr.prob0 <- predict(ct1,test,type="prob")
head(pr.prob0,6)

##-- 26. Pasa las probabilidades de una lista a una matriz y mira las 6 primeras filas de predicciones
pr.prob <- matrix(unlist(pr.prob0),ncol=9,byrow=TRUE)
head(pr.prob,6)

##-- 27. Escoge una observacion al azar en que no acierte la prediccion
##-- Mira que probabilidad tenia la respuesta real en este caso
fallos <- which(pr1!=test$target)         # fallos (no coincide prediccion con valor real)
set.seed(12345)
sel <- sample(fallos,1)           # un fallo al azar
test$target[sel]                  # valor real del elemento escogido
pr.prob[sel,]                     # probabilidades del elemento escogido

##-- 28. Escribe las probabilidades con el identificador en un fichero separado por comas
df <- data.frame(id=test$id,pr.prob)
write.table(df,'resultados_parteB.txt',sep=',',dec='.',row.names=FALSE,quote=FALSE,append=FALSE)

# CALCULATING LOGLOSS
handle_extreme_values <- function(p){
  
  res <- max(min(p,1-exp(-15)),exp(-15))
  return(res)
}

logloss <- 0
for(i in 1:nrow(df))
  for(j in 1:9)
    if(!is.element(i, fallos)) 
      logloss <- logloss + log(handle_extreme_values(df[i,j]))

logloss <- logloss*(-1/nrow(df)) 
logloss
    

