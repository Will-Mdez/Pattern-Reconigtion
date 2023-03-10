nameFile <- "C://Users//Alumnos//Downloads//datasets-20230307T150614Z-001//datasets//iris.data";
dataIris <- read.table(nameFile, header = FALSE, sep = ",");
nameFile2 <- "C://Users//Alumnos//Downloads//datasets-20230307T150614Z-001//datasets//wiki4HERed.csv";
dataWiki <- read.table(nameFile2, header = TRUE, sep = ",");


##Nombre a las columnas de datos
names(dataIris) <- c("sepal_length","sepal_width","petal_length","petal_width","class")
#dimensiones
dim(dataIris)
#Acceder a una columna de datos
dataIris$sepal_length
#Acceder al registro 10 de la columna
dataIris$sepal_length[10]
#Descripcion de datos
summary(dataIris)

#Convertir los datos a Categoricos
dataIris$class<-factor(dataIris$class)
#Descripcion de los datos
summary(dataIris)

#Media
meanSL <- mean(dataIris$sepal_length)
#Desviacion Estandar
stdSL <- sd(dataIris$sepal_length)
df_ordenado <-dataIris
#Ordenar los datos de acuerdo a los calos de una columna
df_ordenado <- dataIris[order(dataIris$sepal_length,decreasing = FALSE),]
print(df_ordenado)

#Indices de la ordenacion
dataF1 <- dataIris$sepal_length
ind_ord <- order(dataF1)

#Frecuencias
freqClass <- table(dataIris$class)
#Frecuencia por classes
print(freqClass[1])
print(freqClass[2])
print(freqClass[3])

freqClass_DF <- as.data.frame(freqClass)
names(freqClass_DF)<-c("clase","freq")
print(freqClass_DF)

#gistograma de frecuencias
library(ggplot2)
p<-ggplot(data=freqClass_DF,aes(x=clase),y=freq)


#DISCRETIZACION
dataWineDiscretizacion <- dataWine[order(dataWine$Proline,decreasing = FALSE), ]
dim(dataWine)
rango <- 178/3 
rango1 <- c()
for(i in 1:rango){
  rango1 <- append(rango1,1)
}
for(i in 1:rango){
  rango1 <- append(rango1,2)
}
for(i in 1:rango){
  rango1 <- append(rango1,3)
}
rango1 <- append(rango1,3)
dataWineDiscretizacion <- cbind(dataWineDiscretizacion,Discretizar=rango1)
head()