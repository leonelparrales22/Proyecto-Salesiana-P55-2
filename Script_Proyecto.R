#PROYECTO ESTADISTICA PARA CIENCIAS DE LA COMPUTACION

#Importacion del Dataset
library(readr)
dataset <- read_csv("Documentos/Estadistica_Proyecto/Dataset/dataset.csv")
View(dataset)
#Asignacion de las variables
x1=dataset$MYCT
x2=dataset$MMIN
x3=dataset$MMAX
x4=dataset$CACH
x5=dataset$CHMIN
x6=dataset$CHMAX
x6=dataset$PRP
y=dataset$ERP

#Tenemos un total de 209 registros de nuestro Dataset
#70% para testing 146
#30% para aprendizaje 63


