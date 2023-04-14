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
metodo2<-c(1,1,1,1,2,2,2,2,2,2,2,2)
metodo1<-c(1,1,1,1,1,2,2,2,2,1,1,1)
fulldatos<-cbind(fulldatos,metodo1,metodo2)
fulldatos
grupo1<-which(metodo1 %in% 1)
grupo2<-which(metodo1 %in% 2)
grupo1
grupo2
mediaMetodo1_g1<-apply(fulldatos[grupo1,],2, mean)
mediaMetodo1_g1
mediaMetodo1_g2<-apply(fulldatos[grupo2,],2, mean)
mediaMetodo1_g2
grupo1m2<-which(metodo2 %in% 1)
grupo2m2<-which(metodo2 %in% 2)
grupo1m2
grupo2m2
mediaMetodo2_g1<-apply(fulldatos[grupo1m2,],2, mean)
mediaMetodo2_g1
mediaMetodo2_g2<-apply(fulldatos[grupo2m2,],2, mean)
mediaMetodo2_g2

mediaglobalCaractM1<-(mediaMetodo1_g1*8+mediaMetodo1_g2*4)/12
mediaglobalCaractM1
mediaglobalCaractM2<-(mediaMetodo2_g1*4+mediaMetodo2_g2*8)/12
mediaglobalCaractM2

euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))


distancia1m1g1<-sapply(grupo1,function(x) euclidiana(mediaMetodo1_g1,fulldatos[x,1:4]))
distancia1m1g1
distancia1m1g2<-sapply(grupo2,function(x) euclidiana(mediaMetodo1_g2,fulldatos[x,1:4]))
distancia1m1g2

distancia1m2g1<-sapply(grupo1m2,function(x) euclidiana(mediaMetodo2_g1,fulldatos[x,1:4]))
distancia1m2g1
distancia1m2g2<-sapply(grupo2m2,function(x) euclidiana(mediaMetodo2_g2,fulldatos[x,1:4]))
distancia1m2g2

sumad1m1<-sum(distancia1m1g1**2)+sum(distancia1m1g2**2)
sumad1m1
sumad1m2<-sum(distancia1m2g1**2)+sum(distancia1m2g2**2)
sumad1m2

#Metrica tr(SB)
trSBM1_g1<-8*euclidiana(mediaglobalCaractM1[1:4],mediaMetodo1_g1[1:4])
trSBM1_g1
trSBM1_g2<-4*euclidiana(mediaglobalCaractM1[1:4],mediaMetodo1_g2[1:4])
trSBM1_g2
sumtrSBM1<-trSBM1_g2+trSBM1_g1
sumtrSBM1
trSBM2_g1<-4*euclidiana(mediaglobalCaractM1[1:4],mediaMetodo2_g1[1:4])
trSBM2_g1
trSBM2_g2<-8*euclidiana(mediaglobalCaractM1[1:4],mediaMetodo2_g2[1:4])
trSBM2_g2
sumtrSBM2<-trSBM2_g2+trSBM2_g1
sumtrSBM2
