#library(magick)
library(recolorize)
nameFileG <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//pelotas.jpg";

img<-readImage(nameFileG, resize =NULL, rotate=NULL)

#dimensiones de la imagen
dim(img)
x11()
layout(matrix(1:4,nrow=2))
par(mar = c(1,2,2,0))
plotImageArray(img,main="original")
plotImageArray(img[ , , 1],main ="red")
plotImageArray(img[ , , 2],main= "green")
plotImageArray(img[ , , 3],main ="blue")

#Features

F1<-img[,,1]
F2<-img[,,2]
F3<-img[,,3]
dim(F1)
class(F1)
#Features como Vectores
F1_V<-as.vector(F1)
F2_V<-as.vector(F2)
F3_V<-as.vector(F3)
#Features en un dataframe
dataPixels<-data.frame(FR=F1_V,FG=F2_V,FB=F3_V)