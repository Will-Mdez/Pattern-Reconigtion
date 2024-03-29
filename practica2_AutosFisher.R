#namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//autosFeatureS.csv"
namefile <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//autosFeatureS.csv"
dataAuto <- read.table(namefile, header = TRUE, sep = ",")

#autoFile2 <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//imports-85.data";
#dataAuto2 <- read.table(autoFile2, header = TRUE, sep = ",")
clase<-c(1,1,0,2,1,1,2,2,2,0,1,0)
dataAuto<-cbind(clase,dataAuto)

summary(dataAuto)

grupo0<-which(clase %in% 0)
grupo1<-which(clase %in% 1)
grupo2<-which(clase %in% 2)
dataAuto[6:9] <- as.data.frame(scale(dataAuto[6:9]))
mediaglobalLength<-mean(c(dataAuto[,6]))
mediaglobalLength
mediaglobalWidth<-mean(c(dataAuto[,7]))
mediaglobalWidth
mediaglobalHeight<-mean(c(dataAuto[,8]))
mediaglobalHeight
mediaglobalHighway<-mean(c(dataAuto[,9]))
mediaglobalHighway


MediasLenth<-aggregate(dataAuto$length,list(dataAuto$clase),mean)
MediasLenth
MediasWidth<-aggregate(dataAuto$width,list(dataAuto$clase),mean)
MediasWidth
MediasHeight<-aggregate(dataAuto$height,list(dataAuto$clase),mean)
MediasHeight
MediasHighway<-aggregate(dataAuto$highway.mpg,list(dataAuto$clase),mean)
MediasHighway

StdLenth<-aggregate(dataAuto$length,list(dataAuto$clase),sd)
StdLenth
StdWidth<-aggregate(dataAuto$width,list(dataAuto$clase),sd)
StdWidth
StdHeight<-aggregate(dataAuto$height,list(dataAuto$clase),sd)
StdHeight
StdHighway<-aggregate(dataAuto$highway.mpg,list(dataAuto$clase),sd)
StdHighway

Proporcion<-c(3/12,5/12,4/12)
Proporcion

multL<-Proporcion*StdHeight[2]
multL

FisherLength<-sum(Proporcion*(MediasLenth[2]-mediaglobalLength)**2)/sum(Proporcion*StdHeight[2])
FisherLength
FisherWidth<-sum(Proporcion*(MediasWidth[2]-mediaglobalWidth)**2)/sum(Proporcion*StdWidth[2])
FisherWidth
FisherHeight<-sum(Proporcion*(MediasHeight[2]-mediaglobalHeight)**2)/sum(Proporcion*StdHeight[2])
FisherHeight
FisherHighway<-sum(Proporcion*(MediasHighway[2]-mediaglobalHighway)**2)/sum(Proporcion*StdHighway[2])
FisherHighway


##ENTROPIA
#make,fuel-type,aspiration
entropia <- c(0.7793,1.28,1.04)



#CORRELACION
correlacion_31 <- sum(dataAuto$height*dataAuto$length)/sqrt(sum(dataAuto$height**2)*sum(dataAuto$length**2))
correlacion_31
correlacion_32 <- sum(dataAuto$height*dataAuto$width)/sqrt(sum(dataAuto$height**2)*sum(dataAuto$width**2))
correlacion_32
correlacion_34 <- sum(dataAuto$height*dataAuto$highway.mpg)/sqrt(sum(dataAuto$height**2)*sum(dataAuto$highway.mpg**2))
correlacion_34
alpha1 <- 0.5
Fs1 <- alpha1*FisherLength-alpha1*abs(correlacion_31)
Fs1
Fs2<- alpha1*FisherWidth-alpha1*abs(correlacion_32)
Fs2
Fs3 <- alpha1*FisherHighway-alpha1*abs(correlacion_34)
Fs3
#HighWay es la mejor ahora

#correlacion de 4 con las que quedan 1 y 2
correlacion_41 <- sum(dataAuto$highway.mpg*dataAuto$length)/sqrt(sum(dataAuto$highway.mpg**2)*sum(dataAuto$length**2))
correlacion_41
correlacion_42 <- sum(dataAuto$highway.mpg*dataAuto$width)/sqrt(sum(dataAuto$highway.mpg**2)*sum(dataAuto$width**2))
correlacion_42
alpha1 <- 0.5
FisherLength
Fs21 <- alpha1*FisherLength-(alpha1/2)*(abs(correlacion_41)+abs(correlacion_31))
Fs21
Fs22 <- alpha1*FisherWidth-(alpha1/2)*(abs(correlacion_42)+abs(correlacion_32))
Fs22
#Width es la mejor ahora

#SELECCION DE SUBCONJUNTOS DE CARACTERISTICAS
#make --- fuel_type

table(dataAuto$make,dataAuto$fuel.type)


gas <- c(4,3,0,3)
diesel <- c(0,0,2,0)
observada<- c(gas,diesel)

make_fuel_type <- cbind(make,gas,diesel)
make_fuel_type

gas_frec_esp <- c(3.33,2.5,1.66,2.5)
diesel_frec_esp <- c(0.66,0.5,0.33,0.5)
esperada <- c(gas_frec_esp,diesel_frec_esp)

chi2 <- sum(sum((observada-esperada)**2/esperada))
chi2
correlacion <- sqrt(chi2/(chi2+12))
correlacion

#make-aspiration
observada
esperada2 <- c(c(4,3,0,1),c(0,0,1,2),c(0,0,1,0))
esperada2
observada2 <- c(c(2.66,2,1.33,2),c(1,0.75,0.5,0.75),c(0.33,0.25,0.166,0.25))

chi22 <- sum(sum((observada2-esperada2)**2/esperada2))
chi22
correlacion2 <- sqrt(chi22/(chi22+12))
correlacion2


FSmake <- 0.5*1.28-0.5*0.7122
FSmake
