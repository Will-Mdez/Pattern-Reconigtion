#library(magick)
library(recolorize)
library(imager)

setwd("C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//")
nameFileG <- ".//pelotas.jpg";
img<-readImage(nameFileG, resize =0.125, rotate=NULL)

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
head(dataPixels)
#Regresar a matrix
pert1<-runif(dim,min = 0,max = 1)
pert2<-runif(dim,min = 0,max = 1)
pert3<-runif(dim,min = 0,max = 1)

b<-1.5

beta<-1/(pert1+pert2+pert3)
PBb1<-(pert1*beta)**1.5
PBb2<-(pert2*beta)**1.5
PBb3<-(pert3*beta)**1.5

VectorPEscalar1<-dataPixels[,]*PBb1
VectorPEscalar2<-dataPixels[,]*PBb2
VectorPEscalar3<-dataPixels[,]*PBb3

sumas1<-c((sum(VectorPEscalar1[,1])),
          (sum(VectorPEscalar1[,2])),
          (sum(VectorPEscalar1[,3])))
sumas2<-c((sum(VectorPEscalar2[,1])),
          (sum(VectorPEscalar2[,2])),
          (sum(VectorPEscalar2[,3])))
sumas3<-c((sum(VectorPEscalar3[,1])),
          (sum(VectorPEscalar3[,2])),
          (sum(VectorPEscalar3[,3])))
medias1<-c((sumas1[1]/sum(PBb1)),
           (sumas1[2]/sum(PBb1)),
           (sumas1[3]/sum(PBb1)))
medias2<-c((sumas2[1]/sum(PBb2)),
           (sumas2[2]/sum(PBb2)),
           (sumas2[3]/sum(PBb2)))
medias3<-c((sumas3[1]/sum(PBb3)),
           (sumas3[2]/sum(PBb3)),
           (sumas3[3]/sum(PBb3)))
dim<-dim(dataPixels[1])
dim
euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))


distancia1<-sapply(seq(1:dim),function(x) euclidiana(medias1,dataPixels[x,]))
distancia1
distancia2<-sapply(seq(1:dim),function(x) euclidiana(medias2,dataPixels[x,]))
distancia2
distancia3<-sapply(seq(1:dim),function(x) euclidiana(medias3,dataPixels[x,]))
distancia3


colum1<-((1/distancia1**2))**(1/(1.5-1))
colum1
colum2<-((1/distancia2**2))**(1/(1.5-1))
colum2
colum3<-((1/distancia3**2))**(1/(1.5-1))
colum3

sumadistancias<-(((1/distancia1**2))**(1/(1.5-1)))+(((1/distancia2**2))**(1/(1.5-1))+((1/distancia3**2))**(1/(1.5-1)))
sumadistancias



pertenencia1 <- (1/distancia1**2)**(1/(1.5-1))/sumadistancias
pertenencia1
pertenencia2 <- (1/distancia2**2)**(1/(1.5-1))/sumadistancias
pertenencia2
pertenencia3 <- (1/distancia3**2)**(1/(1.5-1))/sumadistancias
pertenencia3

medias1
epsilon<-0.1
medias1
medias2
medias3
difmedias<-1
while (difmedias > epsilon) {
  
  #beta<-1/(pertenencia1+pertenencia2+pertenencia3)
  
  #PBb1<-(pertenencia1*beta)**1.5
  #PBb2<-(pertenencia2*beta)**1.5
  #PBb3<-(pertenencia3*beta)**1.5
  PBb1<-(pertenencia1)**1.5
  PBb2<-(pertenencia2)**1.5
  PBb3<-(pertenencia3)**1.5
  
  VectorPEscalar1<-dataPixels[,]*PBb1
  VectorPEscalar2<-dataPixels[,]*PBb2
  VectorPEscalar3<-dataPixels[,]*PBb3
  
  sumas1<-c((sum(VectorPEscalar1[,1])),
            (sum(VectorPEscalar1[,2])),
            (sum(VectorPEscalar1[,3])))
  sumas2<-c((sum(VectorPEscalar2[,1])),
            (sum(VectorPEscalar2[,2])),
            (sum(VectorPEscalar2[,3])))
  sumas3<-c((sum(VectorPEscalar3[,1])),
            (sum(VectorPEscalar3[,2])),
            (sum(VectorPEscalar3[,3])))
  
  mediasact1<-c((sumas1[1]/sum(PBb1)),
             (sumas1[2]/sum(PBb1)),
             (sumas1[3]/sum(PBb1)))
  mediasact2<-c((sumas2[1]/sum(PBb2)),
             (sumas2[2]/sum(PBb2)),
             (sumas2[3]/sum(PBb2)))
  mediasact3<-c((sumas3[1]/sum(PBb3)),
             (sumas3[2]/sum(PBb3)),
             (sumas3[3]/sum(PBb3)))
  
  distancia1<-sapply(seq(1:dim),function(x) euclidiana(mediasact1,dataPixels[x,]))
  distancia2<-sapply(seq(1:dim),function(x) euclidiana(mediasact2,dataPixels[x,]))
  distancia3<-sapply(seq(1:dim),function(x) euclidiana(mediasact3,dataPixels[x,]))
  
  sumadistancias<-(((1/distancia1**2))**(1/(1.5-1)))+(((1/distancia2**2))**(1/(1.5-1))+((1/distancia3**2))**(1/(1.5-1)))
  
  pertenencia1 <- (1/distancia1**2)**(1/(1.5-1))/sumadistancias
  pertenencia2 <- (1/distancia2**2)**(1/(1.5-1))/sumadistancias
  pertenencia3 <- (1/distancia3**2)**(1/(1.5-1))/sumadistancias
  
  dif1<-((sum(medias1-mediasact1)**2))**(1/2)
  dif2<-((sum(medias2-mediasact3)**2))**(1/2)
  dif3<-((sum(medias3-mediasact3)**2))**(1/2)
  medias1<-mediasact1
  medias2<-mediasact2
  medias3<-mediasact3
  difmedias<-(sum(dif1,dif2,dif3))/3
}
difmedias
medias1 
asignaGrupo<-function(a,b) (if (sum(a>b)) 0 else 1 )

asignaGrupoPorColor<-function(a,b) ifelse (a>b , 0, 1 )

grupox<-asignaGrupo(dataPixels[1,],medias1)
grupo <- sapply(seq(1:dim),function(x) asignaGrupo(mediasact1,dataPixels[x,]))
grupoColores <- sapply(seq(1:dim),function(x) asignaGrupoPorColor(mediasact1,dataPixels[x,]))
t(grupoColores)
FRes<-matrix(grupo,ncol = dim(F1)[2])
FRes
img
rojo<-matrix(grupoColores[,1],ncol = dim(F1)[2])
verde<-matrix(grupoColores[,2],ncol = dim(F2)[2])
azul<-matrix(grupoColores[,3],ncol = dim(F3)[2])

grupo<-matrix(grupo)

grupo
dataPixels
dim(img)
x11()
layout(matrix(1:4,nrow=2))
par(mar = c(1,2,2,0))
plotImageArray(img,main="original")
plotImageArray(rojo,main ="red")
plotImageArray(verde,main= "green")
plotImageArray(azul,main ="blue")




