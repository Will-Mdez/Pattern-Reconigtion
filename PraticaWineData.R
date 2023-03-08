nameFile <- "C://Users//Alumnos//Downloads//datasets-20230307T150614Z-001//datasets//wine.data";
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
sigma <-sapply(dataWine[c("Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                          "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                          "OD280/OD315 of diluted wines","Proline")],sd)
media <-sapply(dataWine[c("Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                          "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                          "OD280/OD315 of diluted wines","Proline")],mean)
names <- c("Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                     "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                     "OD280/OD315 of diluted wines","Proline")
print(names)


dataWineSE <-sapply(dataWine[c("Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                          "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                          "OD280/OD315 of diluted wines","Proline")],valor_extremo)
valor_extremo = media[1]-(3*sigma[1])
dataWineSE <- dataWine[dataWine[1:1]<=valor_extremo,]
summary(dataWineSE)
