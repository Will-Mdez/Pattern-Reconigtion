#namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//autos.csv"
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//autosFeatureS.csv"
dataAuto <- read.table(namefile, header = TRUE, sep = ",")
dataCarsKMeans<-dataAuto[1:10,7:10]
p1<-c(0.2,0.5,0.25,0.45,0.15,0.66,0.1,0.95,0.15,0.35)
p2<-c(0.98,0.7,0.35,0.65,0.95,0.23,0.87,0.35,0.25,0.35)
b<-1.5
beta<-1/(p1+p2)
PBb1e2<-(p1*beta)**1.5
PBb2e2<-(p2*beta)**1.5
PBb1e2
VectorPEscalar1<-dataCarsKMeans[1:10,1:4]*PBb1e2
VectorPEscalar2<-dataCarsKMeans[1:10,1:4]*PBb2e2
sumas1<-c((sum(VectorPEscalar1[,1])),
  (sum(VectorPEscalar1[,2])),
  (sum(VectorPEscalar1[,3])),
  (sum(VectorPEscalar1[,4])))
sumas2<-c(sum(VectorPEscalar2[,1]),sum(VectorPEscalar2[,2]),sum(VectorPEscalar2[,3]),sum(VectorPEscalar2[,4]))
dim<-c(1,2,3,4,5,6,7,8,9,10)
medias1<-c((sumas1[1]/sum(PBb1e2)),(sumas1[2]/sum(PBb1e2)),(sumas1[3]/sum(PBb1e2)),(sumas1[4]/sum(PBb1e2)))
medias1
medias2<-c((sumas2[1]/sum(PBb2e2)),(sumas2[2]/sum(PBb2e2)),(sumas2[3]/sum(PBb2e2)),(sumas2[4]/sum(PBb2e2)))
medias2

sumasAply<-sapply(dim,function(x) sum(VectorPEscalar1[,x]))

euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))

distancia1<-sapply(seq(1:10),function(x) euclidiana(medias1,dataCarsKMeans[x,]))
distancia1
distancia2<-sapply(seq(1:10),function(x) euclidiana(medias2,dataCarsKMeans[x,]))
distancia2

colum1<-((1/distancia1**2))**(1/(1.5-1))
colum1

colum2<-((1/distancia2**2))**(1/(1.5-1))
colum2

sumadistancias<-(((1/distancia1**2))**(1/(1.5-1)))+(((1/distancia2**2))**(1/(1.5-1)))
sumadistancias



pertenencia1 <- (1/distancia1**2)**(1/(1.5-1))/sumadistancias
pertenencia1
pertenencia2 <- (1/distancia2**2)**(1/(1.5-1))/sumadistancias
pertenencia2

