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
nuevos_datos <- data.frame(Cielo ,
                           Temperatura,
                           Humedad ,
                           Viento)
predicciones <- predict(modelo_bayesiano, nuevos_datos,type = "raw")


# Imprimir las predicciones
print(predicciones)

1/(.44*.6428*.33*.44*.33)

#Beta(alpha1 + alpha2) = 1
#Beta = 1/(alpha1 + alpha2)

Dia <- c("laborable","laborable","laborable","laborable","sabado","laborable","festivo","domingo","laborable","laborable","laborable","sabado","laborable","laborable","sabado","laborable","festivo","laborable","laborable")
#Estacion <- c("primavera","invierno","invierno","invierno","verano",,,,,,,,)
ClassVerdadera <- c("puntual","puntual","puntual","tarde","puntual","retrasado","puntual","puntual","retrasado","puntual","puntual","tarde","puntual","retrasado","puntual","puntual","puntual","puntual","puntual")
classIdenti <- c("puntual","tarde","puntual","retrasado","puntual","retrasado","puntual","puntual","tarde","puntual","puntual","tarde","puntual","retrasado","puntual","tarde","puntual","tarde","retrasado")
ClassVerdadera <- factor(ClassVerdadera)
classIdenti <- factor(classIdenti)

matriz_confusion <-confusionMatrix(classIdenti, ClassVerdadera)
matriz_confusion$byClass
  