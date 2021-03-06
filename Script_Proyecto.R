  #PROYECTO ESTADISTICA PARA CIENCIAS DE LA COMPUTACION

#Importacion del Dataset
library(readr)
dataset <- read_csv("Dataset/Training.csv")
sapply(dataset, function(x) sum(is.na(x))) #numero de nulos y faltantes que existe en la data 

library(readr)
dataset2 <- read_csv("Dataset/Test.csv")
sapply(dataset2, function(x) sum(is.na(x))) #numero de nulos y faltantes que existe en la data 

#Asignacion de las variables
x1=dataset$MYCT
x2=dataset$MMIN
x3=dataset$MMAX
x4=dataset$CACH
x5=dataset$CHMIN
x6=dataset$CHMAX
x7=dataset$PRP
y=dataset$ERP
#Tenemos un total de 209 registros de nuestro Dataset
#70% para training 146
#30% para test 63

##############################################################
#####REGRESION POR LOS METODOS BACKWARD, FORWARD, AND BOTH####
##############################################################
#BACKWARD
rbackward=step(lm(y~x1+x2+x3+x4+x5+x6+x7), direction = "backward") 
summary(rbackward) #p-value=2.2e-16 #rajustado=0.949 #x1,2,3,5,6,7
#FORWARD
aux=lm(y~1)
rforward=step(lm(y~x1+x2+x3+x4+x5+x6+x7), direction = "forward", scope = (~ x1+x2+x3+x4+x5+x6+x7))
summary(rforward) #p-value=2.2e-16 #rajustado=0.9486 #x1,2,3,4,5,6,7

#BOTH
rboth=step(lm(y~x1+x2+x3+x4+x5+x6+x7), direction = "both")
summary(rboth) #p-value=2.2e-16 #rajustado=0949 #x1,2,3,5,6,7

#Conclusion: both y backwatd me dan un mejor rajustado aunque sea casi minimo
#eliminando la variable x4
#en este caso escogeremos el metodo both

#Arnmando mi ecuacion de regresion estimada
b0=rboth$coefficients[1]
b1=rboth$coefficients[2]
b2=rboth$coefficients[3]
b3=rboth$coefficients[4]
b4=rboth$coefficients[5]
b5=rboth$coefficients[6]
b6=rboth$coefficients[7]


#Probando con el %70 de Training
y_estimada=b0+b1*x1+b2*x2+b3*x3+b4*x5+b5*x6+b6*x7

error=y-y_estimada
errorValorAbsoluto=abs(error)
errorPromedio=sum(errorValorAbsoluto)/length(y)
errorPromedio
#Tenemos un error promedio de 17.24797 en nuestro Training

#Probando con el %30 de Test
x1=dataset2$MYCT
x2=dataset2$MMIN
x3=dataset2$MMAX
x4=dataset2$CACH
x5=dataset2$CHMIN
x6=dataset2$CHMAX
x7=dataset2$PRP
y=dataset2$ERP

y_estimada=b0+b1*x1+b2*x2+b3*x3+b4*x5+b5*x6+b6*x7

error=y-y_estimada
errorValorAbsoluto=abs(error)
errorPromedio=sum(errorValorAbsoluto)/length(y)
errorPromedio
#Tenemos un error promedio de 26.55031 en nuestro Test


##############################################################
#######################USANDO PRIMERO PCA#####################
##############################################################

matriz  = cbind(dataset[, 3:9]) 
# Test de Barlett
bt = bartlett.test(dataset[, 3:9])

# Kaiser, Meyer, Olkin - KMO
#install.packages("REdaS")
library("REdaS")
kmo = KMOS(matriz)


#Matriz de Variables Independientes

pca = prcomp(matriz, center = TRUE, scale. = TRUE) #,rank. = 3
print(pca)
plot(pca, type = "l")
summary(pca)
plot(pca)# grafica 


# Componentes Principales
cp = predict(pca, newdata=tail(matriz, 146))
cp = as.data.frame(cp)

y=dataset$ERP

#Regresion con PCA
vende = lm(y ~ cp$PC1 + cp$PC2 + cp$PC3 + cp$PC4 + cp$PC5 + cp$PC6 +cp$PC7 ) #calculo de la regresion lineal (funcion directa)
print(summary(vende)) #r-ajustado 0.9486 

vende5 = lm(y ~ cp$PC1 + cp$PC2 + cp$PC3 + cp$PC4 + cp$PC5) #calculo de la regresion lineal (funcion directa)
print(summary(vende5)) #r-ajustado 0.9437 

vende6 = lm(y ~ cp$PC1 + cp$PC2 + cp$PC3 + cp$PC4) #calculo de la regresion lineal (funcion directa)
print(summary(vende6)) #r-ajustado 0.9377 

vende3 = lm(y ~ cp$PC1 + cp$PC2 + cp$PC3) #calculo de la regresion lineal (funcion directa)
print(summary(vende3)) #r-ajustado 0.9359 


b0=vende3$coefficients[1]
b1=vende3$coefficients[2]
b2=vende3$coefficients[3]
b3=vende3$coefficients[4]


######EN TRAINING
#ecuacion estimada
y_estimada=b0+b1*cp$PC1+b2*cp$PC2+b3*cp$PC3

error=y-y_estimada
errorValorAbsoluto=abs(error)
errorPromedio=sum(errorValorAbsoluto)/length(y)
errorPromedio
#El error promedio es de 18.30 en TRAINING

###EN TEST
y=dataset2$ERP

matriz  = cbind(dataset2[, 3:9]) 
pca = prcomp(matriz, center = TRUE, scale. = TRUE) #,rank. = 3
cp = predict(pca, newdata=tail(matriz, 63))
cp = as.data.frame(cp)

y_estimada=b0+b1*cp$PC1+b2*cp$PC2+b3*cp$PC3

error=y-y_estimada
errorValorAbsoluto=abs(error)
errorPromedio=sum(errorValorAbsoluto)/length(y)
errorPromedio
#El error promedio es de 60.94 en TEST

  
