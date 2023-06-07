#Ejemplo 2
#Clasificador Bayes Ingenuo
Cielo <- c("sol","sol","nubes","lluvia","lluvia","lluvia","nubes","sol","sol","lluvia","sol","nubes","nubes","lluvia")
Temperatura <- c("calor","calor","calor","templado","frio","frio","frio","templado","frio","templado","templado","templado","calor","templado")
Humedad <- c("alta","alta","alta","alta","normal","normal","normal","alta","normal","normal","normal","alta","normal","alta")
Viento <- c("suave","fuerte","suave","suave","suave","fuerte","fuerte","suave","suave","suave","fuerte","fuerte","suave","fuerte")
ClaseMeteoro <- c("no","no","si","si","si","no","si","no","si","si","si","si","si","no")
N <- length(ClaseMeteoro)

dataMeteoro <- data.frame(Cielo,Temperatura,Humedad,Viento,ClaseMeteoro)


library(e1071)


# Codificar las variables categóricas como factores
dataMeteoro$Cielo <- factor(dataMeteoro$Cielo)
dataMeteoro$Temperatura <- factor(dataMeteoro$Temperatura)
dataMeteoro$Humedad <- factor(dataMeteoro$Humedad)
dataMeteoro$Viento <- factor(dataMeteoro$Viento)
# Entrenar el modelo de clasificación bayesiana
modelo_bayesiano <- naiveBayes(ClaseMeteoro ~ ., data = dataMeteoro)

# Imprimir el resumen del modelo
print(modelo_bayesiano)

tableCielo <- modelo_bayesiano$tables$Cielo
tableCielo <- matrix(tableCielo,nrow=2)
tableCielo
# Realizar predicciones en nuevos datos
nuevos_datos <- data.frame(Cielo = c("nubes"),
                           Temperatura = c("templado"),
                           Humedad = c("alta"),
                           Viento = c("fuerte"))
predicciones <- predict(modelo_bayesiano, nuevos_datos,type = "raw")


# Imprimir las predicciones
print(predicciones)
.44*.6428*.33*.44*.33
