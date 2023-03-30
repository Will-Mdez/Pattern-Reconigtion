library(ggplot2)
library(dplyr)
library(tidyr)
library(car)

namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//iris.data"
dataIris <- read.table(namefile, header = TRUE, sep = ",")


#NOmbres
namesIris<-c("sepal_l","sepal_w","petal_l","petal_w","clase")
names(dataIris)<-namesIris

#Convertir datos categoricos
dataIris$clase<-factor(dataIris$clase)
#DEcripcion
summary(dataIris)



#BAse
#pch:tipo de marcador
plot(dataIris$sepal_l,dataIris$petal_l,pch=19,col="black")
plot(dataIris$sepal_l,dataIris$petal_l,pch=19,col="red",xlab = "longitud sepalo",ylab = "longitud petalo")
plot(dataIris$sepal_l,dataIris$petal_l,pch=as.numeric(dataIris$clase),col=as.numeric(dataIris$clase),xlab = "longitud sepalo",ylab = "longitud petalo")

legend("bottomright",legend = c("setosa","versicolor","virginica"),
       pch=c(1,2,3),lty=c(1,2,3),col=c(1,2,3),lwd=2)
x11()


#Otra forma
scatterplot(dataIris$sepal_l,dataIris$petal_l,
            col=1,pch=15,boxplots = "",
            regLine=list(col="green",lwd=3),
            smooth=list(col.smooth="red",col.spread="blue"))
          