############################################################
#
# Sesion 4 - MBD
# Author: Fernando Ferri
############################################################

############################################################
# Variables en el conjunto de datos:
#
# Caracteristicas del cliente:
# id: identificador del cliente
# age: edad
# job: tipo de trabajo (admin., blue-collar, entrepreneur, housemaid, management, retired, self-employed, services, student, technician, unemployed, unknown)
# marital: estado civil (divorced, married, single, unknown)
# education: nivel de estudios (basic.4y, basic.6y, basic.9y, high.school, illiterate, professional.course, university.degree, unknown)
# default: ?es moroso? (no,yes,unknown)
# housing: ?tiene hipoteca? (no,yes,unknown)
# loan: tiene un prestamo personal? (no,yes,unknown)
# 
# Caracter?sticas de la llamada:
# contact: tipo de telefono (cellular, telephone)
# month: mes
# day_of_week: dia de la semana (mon, tue, wed, thu, fri)
# 
# Otros atributos:
# campaign: numero de contactos realizados esta campa?a para este cliente (incluyendo el actual)
# pdays: numero de dias que han pasado desde que el cliente fue contactado por ultima vez para una campa?a previa (999 significa que no fue contactado previamente)
# previous: n?mero de llamadas realizadas a este cliente antes de esta campa?a
# poutcome: resultado de la anterior campa?a (failure, nonexistent, success)
#
############################################################

############################################################
#
# Parte 1. Construir modelo
#
############################################################
##-- 1.Borra todos los objetos que tengas en memoria
rm(list=ls())

############################################################
# Lectura de datos y inspeccion
############################################################
##-- 2. Lee los datos p2_train.csv
setwd('/Users/Fer/Development/MASTERBD/ESTADISTICA/D_REGRESIO_LOGISTICA/practica_reg_logistica')                                           # directorio de trabajo
datos2 <- read.table('p2_train.csv',header=TRUE,sep=";",na.strings=c("NA",999))  # lectura de los datos

##-- 3. Visualiza los 100 primeros registros, haz descriptiva con summary y elimina la variable id
View(head(datos2,100))   # Ver los primeros registros
summary(datos2)             # Descriptiva de todas las variables
datos2$id <-NULL         # Eliminar la variable id que no se har? servir


############################################################
# Eliminar variable con muchos missings 
############################################################
##-- 4. Hay una variable que tiene m?s de un 96% de datos ausentes codificados con 999
howmany_na_variables <- apply(apply(datos2,2,is.na),2,sum)
howmanyPdays <-howmany_na_variables["pdays"]/nrow(datos2)
howmanyPdays

##-- Encuentrala y eliminala
datos2$pdays <- NULL


############################################################
# Descriptiva bivariante para variables categoricas
############################################################
##-- 5. Crea un vector (var.cat) que contenga la posicion de todas las variables 
##-- categoricas (factores) excepto la respuesta (y) 
sapply(datos2,class)                                                  # Clase de las variables
var.cat <- which(sapply(datos2,class)=="factor" & names(datos2)!="y") # Se seleccionan los factores y no sea la respuesta 

##-- 6. Haz un mosaic-plot de todas las variables categoricas con la respuesta
##-- usando un for
#quartz()
for(vc in var.cat){
  mosaicplot(datos2[,vc]~datos2$y,main=paste(names(datos2)[vc]," VS campign-success"),col=2:3,las=2,xlab=names(datos2)[vc],ylab="success? (achieved deposit subscription)")
}


############################################################
# Recategorizacion de variables categoricas
############################################################
##-- 7. Creamos una variable job2 con solo 2 categorias
##-- Miramos el mosaicplot de esta variable para decidir las categorias
datos2$job2 <- factor(ifelse(datos2$job %in% c("retired","student"),"not_active","active"))
datos2$job <- NULL

##-- 8. Creamos una variable month2 con solo 2 categorias
##-- Miramos el mosaicplot de esta variable para decidir las categorias
datos2$month2 <- factor(ifelse(datos2$month %in% c("dec","mar","oct","sep"),"higherOffer","lowerOffer"))
datos2$month <- NULL

##-- 9. Miramos el modelo ajustado para la variable education
##-- Juntamos las dos categorias con mas similitud en efecto y semantica y el resto, dejalas igual
mod.glm.edu <- glm(y~education,datos2,family=binomial)
summary(mod.glm.edu)
datos2$education2 <- factor(ifelse(datos2$education %in% c("basic.6y","basic.9y"),"educationbasic",as.character(datos2$education)))
datos2$education <- NULL

##-- 10. La variable default tiene una categoria sin individuos que hayan comprado (y='yes')
##-- Juntamos esta categoria con otra
table(datos2$default,datos2$y)
datos2$default2 <- factor(ifelse(datos2$default=='no','no','other'))
datos2$default <- NULL

#NEW: OTRAS nuevas agrupaciones
##-- Juntamos divorced y married al parecer tener la misma probabilidad de venta
#table(datos2$marital,datos2$y)
datos2$marital2 <- factor(ifelse(datos2$marital %in% c("divorced","married"),'have_had_commitment','other'))
datos2$marital <- NULL


#NEW: eliminamos del modelo variables que parecen no relevantes (misma ventas para todos las categorias)
# (són eliminadas de todas formas por la seleccion automatica: step)
datos2$loan <- NULL
datos2$housing <- NULL


############################################################
# Descriptiva bivariante para variables numericas
############################################################
##-- 11. Crea un vector (var.num) que contenga la posicion de todas las variables 
##-- numericas o enteras
#quartz()
var.num <- which(sapply(datos2,class) %in% c("numeric","integer"))

##-- 12. Usa la funcion cdplot para dibujar las densidades de todas las variables 
##-- numericas con la respuesta
for(vn in var.num){
  cdplot(datos2$y~datos2[,vn],main=paste(names(datos2)[vn]," VS campign-success"),n=16,las=2,xlab=names(datos2)[vn],ylab="success? (achieved deposit subscription)")
}

############################################################
# Estimacion del modelo con todas las variables
############################################################
##-- 13. Estima el modelo con todas las variables
mod.glm0 <- glm(y~., datos2 ,family=binomial)
summary(mod.glm0)

############################################################
# Seleccion automatica
############################################################
##-- 14. Selecciona automaticamente las variables relevantes con step (Be patient!)
# Mirar si hi ha missings
apply(apply(datos2,2,is.na),2,sum)

mod.glm1 <- step(mod.glm0)
summary(mod.glm1)


############################################################
# Validacion 
############################################################
##-- 15. Instala el paquete ResourceSelection y cargalo
#install.packages('ResourceSelection')
library(ResourceSelection)

##-- 16. Realiza el test de Hosmer-Lemeshow
valores.reales <- mod.glm1$y
valores.predichos <- fitted(mod.glm1)
#?hoslem.test
hoslem.test(valores.reales ,valores.predichos)

##-- 17. Divide los valores predichos en deciles
br <- quantile(valores.predichos,seq(0,1,0.1))

##-- 18. Dibuja los efectivos predichos vs los esperados en cada cuantil 
#quartz()
int <- cut(fitted(mod.glm1),br)
#obs: valor observados
obs <- tapply(mod.glm1$y,int,sum)
#exp-> expected, predichos 
exp <- tapply(fitted(mod.glm1),int,sum)
plot(1:10,exp,type='h',xlab="Intervalos",ylab="Frecuencias")
lines(1:10-0.1,obs,type='h',col=2)

# Vemos que tras dividir los datos test en diez grupos, el análisis 
# visual sobre los valores predichos / expected (barras negras) y los
# reales/ observed extraídos de los datos test (barra negra),  que la 
# predicción se acerca mucho al valor real básicamente para todos los diez
# grupos en que se han dividido las observaciones.




############################################################
# Estimacion de un Odds Ratio (OR)
############################################################
##-- 19. Calcula el OR de contactar al telefono (contact) fijo respecto a hacerlo por movil
exp(mod.glm1$coefficients["contacttelephone"])

## PREGUNTAR!
##-- 20. Calcula el OR de una campanya adicional (campaign) y interpretalo
exp(mod.glm1$coefficients["campaign"])

##-- 21. Calcula los intervalos de confianza del 95% para todos los ORs
##-- ?Cu?l es la variable m?s relevante a la hora de tener ?xito en la venta?
IC <- confint(mod.glm1)     # Intervalos de  confianza para los coeficiente. Paciencia!
round(exp(IC),2)           # Intervalos de confianza para los ORs

#la variable más relevante es la con el intervalo de confianza más estrecho, en este caso
# es "n.employed", con una distancia de 0.01.

############################################################
# Estimacion de las probabilidades de adquirir el producto
############################################################
##-- 22. Estima las probabilidades predichas por el modelo para todos los individuos
pr <- predict(mod.glm1,datos2,type="response")
pr

##-- 23. Identifica a los individuos que el modelo predice con mayor y menor probabilidad
##-- de comprar el producto y verifica si realmente lo adquirieron o no
ind.max <- which.max(pr)
datos2$y[ind.max]

ind.min <- which.min(pr)
datos2$y[ind.min]

##-- 24. Escoge un idividuo al azar y mira la probabilidad predicha y si realmente escogio el producto 
#ind <- datos2[1000,]            # Posicion del individuo 
#ind$y

ind <- 1000 # Posicion del individuo
datos2$y[ind]

pr[ind]                 # Probabilidad de escogerlo


############################################################
# Curva ROC y AUC
############################################################
##-- 25. Instala y carga el paquete AUC
#install.packages("AUC")               # Instalar paquete
library("AUC")               # Cargar paquete

##-- 26. La instrucci?n roc prepara los datos para luego hacer el grafico de la curva ROC
##-- y para calcular el AUC
#quartz()
roc.curve <- roc(pr,datos2$y)
plot(roc.curve)

auc(roc.curve)

##-- 27. A partir de ahora la empresa decide unicamente llamar a aquellos que tengan una probabilidad
##-- mayor de 0.2 de adquirir el producto. ?Que porcentaje se espera de las llamadas que adquieran el producto?
##-- ?Que porcentaje de los que no llamamos hubiesen adquirido el producto?
cut.point <- which(roc.curve$cutoffs<0.2)[1]  # Posicion del punto de corte para p=0.2
roc.curve$tpr[cut.point]                      # True Positive Rate (TPR) --> De los que realmente hubieran adquirido el producto, ?a que porcentaje llamaremos?
roc.curve$fpr[cut.point]                            # False Positive Rate (FPR) --> De los que realmente NO hubieran adquirido el producto, ?a que porcentaje llamaremos?

############################################################
#
# Parte 2. Testear resultados
#
############################################################
##-- 28. leemos los datos test
test <- read.table('p2_test (con variable respuesta).csv',header=TRUE,sep=';')

############################################################
# Transformaciones en variables
############################################################
##-- 29. Realizamos las mismas transformaciones que hicimos en el conjunto de entrenamiento
#eliminamos pdays y id del modelo
#test$id <- NULL
test$pdays <- NULL

#agrupamos job en un menor número de categorias 
test$job2 <- factor(ifelse(test$job %in% c("retired","student"),"not_active","active"))
test$job <- NULL

#agrupamos month en un menor número de categorias 
test$month2 <- factor(ifelse(test$month %in% c("dec","mar","oct","sep"),"higherOffer","lowerOffer"))
test$month <- NULL

#agrupamos educacion en un menor número de categorias 
test$education2 <- factor(ifelse(test$education %in% c("basic.6y","basic.9y"),"educationbasic",as.character(test$education)))
test$education <- NULL

#agrupamos default en un menor número de categorias 
test$default2 <- factor(ifelse(test$default=='no','no','other'))
test$default <- NULL

#NEW: OTRAS nuevas agrupaciones
##-- Juntamos divorced y married
#table(datos2$marital,datos2$y)
test$marital2 <- factor(ifelse(test$marital %in% c("divorced","married"),'have_had_commitment','other'))
test$marital <- NULL


#NEW: variables q parecen no relevantes (són eliminadas de todas formas por la seleccion automatica: step)
test$loan <- NULL
test$housing <- NULL

############################################################
# Calcular predicciones y compararlas con valores reales
############################################################
##-- 30. Representa las predicciones en fucion de la respuesta con un boxplot
pr <- predict(mod.glm1,test, type="response")
#quartz()
boxplot(pr~test$y)

##-- 31. Haz la curva ROC y calcula el AUC
roc.curve <- roc(pr,test$y)
#quartz()
plot(roc.curve)
auc(roc.curve)






