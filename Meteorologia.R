
#METODO ID3
library(dplyr)

Exterior <- c("Soleado","Soleado","Nublado","Lluvioso","Lluvioso","Lluvioso","Nublado","Soleado","Soleado","Lluvioso","Soleado","Nublado","Nublado","Lluvioso")

Temperatura <- c("Calor","Calor","Calor","Templado","Frio","Frio","Frio","Templado","Frio","Templado","Templado","Templado","Calor","Templado")
Humedad <- c("Alta","Alta","Alta","Alta","Normal","Normal","Normal","Alta","Normal","Normal","Normal","Alta","Normal","Alta")
Viento <- c("No","Sí","No","No","No","Sí","Sí","No","No","No","Sí","Sí","No","Sí")
ClaseM <- c("N","N","P","P","P","N","P","N","P","P","P","P","P","N")
length(Exterior)
length(Temperatura)
length(Humedad)
length(Viento)

dataMeteorologia <- cbind(Exterior,Temperatura,Humedad,Viento,ClaseM)

dataMeteorologia

dataMeteorologia <- as.data.frame(dataMeteorologia)
dataMeteorologia$Temperatura <- factor(dataMeteorologia$Temperatura)
dataMeteorologia$Exterior <- factor(dataMeteorologia$Exterior)
dataMeteorologia$Viento <- factor(dataMeteorologia$Viento)
dataMeteorologia$ClaseM <- factor(dataMeteorologia$ClaseM)
summary(dataMeteorologia)
filter(dataMeteorologia, Exterior=="Soleado" & ClaseM == "N")


#CLASIFICADOR BAYESIANO


