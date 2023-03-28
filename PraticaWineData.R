nameFile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//wine.data";
dataWine <- read.table(nameFile, header = FALSE, sep = ",");

##Nombre a las columnas de datos
names(dataWine) <- c("class","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                     "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                     "OD280/OD315 of diluted wines","Proline")
#dimensiones
dim(dataWine)
#Descripcion de datos
summary(dataWine)
head(dataWine)
#Convertir los datos a Categoricos
dataWine$class<-factor(dataWine$class)


#Desviacion Estandar
dataWineSE <- dataWine[dataWine$Alcohol<=mean(dataWine$Alcohol)+(3*sd(dataWine$Alcohol)),]
dataWineSE <- dataWine[dataWine$Malic_acid<=mean(dataWine$Malic_acid)+(3*sd(dataWine$Malic_acid)),]
dataWineSE <- dataWine[dataWine$Ash<=mean(dataWine$Ash)+(3*sd(dataWine$Ash)),]
dataWineSE <- dataWine[dataWine$Alcalinity_of_ash<=mean(dataWine$Alcalinity_of_ash)+(3*sd(dataWine$Alcalinity_of_ash)),]
dataWineSE <- dataWine[dataWine$Mag<=mean(dataWine$Mag)+(3*sd(dataWine$Mag)),]
dataWineSE <- dataWine[dataWine$`Total phenols`<=mean(dataWine$`Total phenols`)+(3*sd(dataWine$`Total phenols`)),]
dataWineSE <- dataWine[dataWine$Flavanoids<=mean(dataWine$Flavanoids)+(3*sd(dataWine$Flavanoids)),]
dataWineSE <- dataWine[dataWine$`Nonflavanoid phenols`<=mean(dataWine$`Nonflavanoid phenols`)+(3*sd(dataWine$`Nonflavanoid phenols`)),]
dataWineSE <- dataWine[dataWine$Proanthocyanins<=mean(dataWine$Proanthocyanins)+(3*sd(dataWine$Proanthocyanins)),]
dataWineSE <- dataWine[dataWine$`Color intensity`<=mean(dataWine$`Color intensity`)+(3*sd(dataWine$`Color intensity`)),]
dataWineSE <- dataWine[dataWine$Hue<=mean(dataWine$Hue)+(3*sd(dataWine$Hue)),]
dataWineSE <- dataWine[dataWine$`OD280/OD315 of diluted wines`<=mean(dataWine$`OD280/OD315 of diluted wines`)+(3*sd(dataWine$`OD280/OD315 of diluted wines`)),]
dataWineSE <- dataWine[dataWine$Proline<=mean(dataWine$Proline)+(3*sd(dataWine$Proline)),]

dataWineSE <- dataWine[dataWine$Alcohol>=mean(dataWine$Alcohol)-(3*sd(dataWine$Alcohol)),]
dataWineSE <- dataWine[dataWine$Malic_acid>=mean(dataWine$Malic_acid)-(3*sd(dataWine$Malic_acid)),]
dataWineSE <- dataWine[dataWine$Ash>=mean(dataWine$Ash)-(3*sd(dataWine$Ash)),]
dataWineSE <- dataWine[dataWine$Alcalinity_of_ash>=mean(dataWine$Alcalinity_of_ash)-(3*sd(dataWine$Alcalinity_of_ash)),]
dataWineSE <- dataWine[dataWine$Mag>=mean(dataWine$Mag)-(3*sd(dataWine$Mag)),]
dataWineSE <- dataWine[dataWine$`Total phenols`>=mean(dataWine$`Total phenols`)-(3*sd(dataWine$`Total phenols`)),]
dataWineSE <- dataWine[dataWine$Flavanoids>=mean(dataWine$Flavanoids)-(3*sd(dataWine$Flavanoids)),]
dataWineSE <- dataWine[dataWine$`Nonflavanoid phenols`>=mean(dataWine$`Nonflavanoid phenols`)-(3*sd(dataWine$`Nonflavanoid phenols`)),]
dataWineSE <- dataWine[dataWine$Proanthocyanins>=mean(dataWine$Proanthocyanins)-(3*sd(dataWine$Proanthocyanins)),]
dataWineSE <- dataWine[dataWine$`Color intensity`>=mean(dataWine$`Color intensity`)-(3*sd(dataWine$`Color intensity`)),]
dataWineSE <- dataWine[dataWine$Hue>=mean(dataWine$Hue)-(3*sd(dataWine$Hue)),]
dataWineSE <- dataWine[dataWine$`OD280/OD315 of diluted wines`>=mean(dataWine$`OD280/OD315 of diluted wines`)-(3*sd(dataWine$`OD280/OD315 of diluted wines`)),]
dataWineSE <- dataWine[dataWine$Proline>=mean(dataWine$Proline)-(3*sd(dataWine$Proline)),]



summary(dataWineSE)

mean_features <- sapply(names,function(x) mean(dataWine[[x]]))
sd_features <- sapply(names,function(x) sd(dataWine[[x]]))
print(sd_features)
print(mean_features)


#NOrmalizacion
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN<- dataF
  dataFN<- (dataFN-meanF)/stdF
  return(dataFN)
}
dataWineNorm <- lapply(names,function(x) normalizeDataL(dataWine[[x]],mean_features[x],sd_features[x]))
names(dataWineNorm)<-names
dataWineNorm<-as.data.frame(dataWineNorm)
#head(dataWineNorm)

summary(dataWineNorm)
N<-dim(dataWine)[1]
print(N)




#DATA WINE con NaN
set.seed(11991)
dataWineNA <- dataWine
dataWineNA$Alcohol[rbinom(N,1,0.1)== 1] <- NA
dataWineNA$`Total phenols`[rbinom(N,1,0.1)== 1] <- NA
summary(dataWineNA)


#Eliminando Registros con NA
dataWine1 <- dataWineNA[rowSums(is.na(dataWineNA))==0,]
summary(dataWine1)
print(dim(dataWine1))


#Imputacion con valor de la media 
dataWine2 <- dataWineNA
meanAlch <- mean(dataWine1$Alcohol,na.rm = TRUE)
meanFenol <- mean(dataWine1$`Total phenols`,na.rm=TRUE)
dataWine2$Alcohol[is.na(dataWineNA$Alcohol)]<-meanAlch
dataWine2$`Total phenols`[is.na(dataWineNA$`Total phenols`)]<-meanFenol
summary(dataWine2)

#Imputacion con valor de la media condicionada
dataWine3 <- dataWineNA
indClase1 <- which(dataWine1$class==1)
meanAlchC1 <- mean(dataWine1$Alcohol[dataWine1$class==1])
meanAlchC2 <- mean(dataWine1$Alcohol[dataWine1$class==2])
meanAlchC3 <- mean(dataWine1$Alcohol[dataWine1$class==3])
meanFenolC1 <- mean(dataWine1$`Total phenols`[dataWine1$class==1])
meanFenolC2 <- mean(dataWine1$`Total phenols`[dataWine1$class==2])
meanFenolC3 <- mean(dataWine1$`Total phenols`[dataWine1$class==3])
dataWine3$Alcohol[is.na(dataWineNA$Alcohol)&dataWineNA$class==1]<-meanAlchC1
dataWine3$Alcohol[is.na(dataWineNA$Alcohol)&dataWineNA$class==2]<-meanAlchC2
dataWine3$Alcohol[is.na(dataWineNA$Alcohol)&dataWineNA$class==3]<-meanAlchC3
dataWine3$`Total phenols`[is.na(dataWineNA$`Total phenols`)&dataWineNA$class==1]<-meanFenolC1
dataWine3$`Total phenols`[is.na(dataWineNA$`Total phenols`)&dataWineNA$class==2]<-meanFenolC2
dataWine3$`Total phenols`[is.na(dataWineNA$`Total phenols`)&dataWineNA$class==3]<-meanFenolC3
summary(dataWine3)

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

#Frecuencias
freqClassWine <- table(dataWine$class)
#Frecuencia por classes
print(freqClassWine[1])
print(freqClassWine[2])
print(freqClassWine[3])

freqClass_DFWine <- as.data.frame(freqClassWine)
names(freqClass_DFWine)<-c("clase","freq")
print(freqClass_DFWine)

#gistograma de frecuencias
library(ggplot2)
p<-ggplot(data=freqClass_DFWine,aes(x=clase,y=freq)) + geom_bar(stat = "identity",fill="#696969")
print(p)
