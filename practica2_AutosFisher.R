namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//autos.csv"
dataAuto <- read.table(namefile, header = TRUE, sep = ",")

autoFile2 <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//imports-85.data";
dataAuto2 <- read.table(autoFile2, header = TRUE, sep = ",")
clase<-c(1,1,0,2,1,1,2,2,2,0,1,0)
dataAuto<-cbind(clase,dataAuto)
dataAuto[9]

grupo0<-which(clase %in% 0)
grupo1<-which(clase %in% 1)
grupo2<-which(clase %in% 2)
dataAuto[9:11] <- as.data.frame(scale(dataAuto[9:11]))
mediaglobalLength<-mean(c(dataAuto[,9]))
mediaglobalLength
mediaglobalWidth<-mean(c(dataAuto[,10]))
mediaglobalWidth
mediaglobalHeight<-mean(c(dataAuto[,11]))
mediaglobalHeight


MediasLenth<-aggregate(dataAuto$length,list(dataAuto$clase),mean)
MediasLenth
MediasWidth<-aggregate(dataAuto$width,list(dataAuto$clase),mean)
MediasWidth
MediasHeight<-aggregate(dataAuto$height,list(dataAuto$clase),mean)
MediasHeight

StdLenth<-aggregate(dataAuto$length,list(dataAuto$clase),sd)
StdLenth
StdWidth<-aggregate(dataAuto$width,list(dataAuto$clase),sd)
StdWidth
StdHeight<-aggregate(dataAuto$height,list(dataAuto$clase),sd)
StdHeight

Proporcion<-c(3/12,5/12,4/12)
Proporcion
mediaglobalLength
restaL<-(MediasLenth[2]-mediaglobalLength)**2
restaL
multL<-Proporcion*StdHeight[2]
multL

FisherLength<-sum((MediasLenth[2]-mediaglobalLength)**2)/sum(Proporcion*StdHeight[2])
FisherLength
FisherWidth<-sum((MediasWidth[2]-mediaglobalWidth)**2)/sum(Proporcion*StdWidth[2])
FisherWidth
FisherHeight<-sum((MediasHeight[2]-mediaglobalHeight)**2)/sum(Proporcion*StdHeight[2])
FisherHeight

