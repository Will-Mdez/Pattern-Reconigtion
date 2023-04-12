nameFile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//wine.data";
dataWine <- read.table(nameFile, header = FALSE, sep = ",");

##Nombre a las columnas de datos
names(dataWine) <- c("class","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                     "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                     "OD280/OD315 of diluted wines","Proline")

datos<-dataWine[1:5,2:5]
datos
datos2<-dataWine[60:62,2:5]
datos2
datos3<-dataWine[65,2:5]
datos3
datos4<-dataWine[20:22,2:5]
datos4
fulldatos<-rbind(datos,datos2,datos3,datos4)
fulldatos
media1<-mean(datos[,1])+mean(datos4[,1])
media1
media2<-mean(datos[,2])+mean(datos4[,2])
media2
media3<-mean(datos[,3])+mean(datos4[,3])
media3
media4<-mean(datos[,4])+mean(datos4[,4])
media4
