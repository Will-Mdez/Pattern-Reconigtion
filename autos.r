namefile <- "C://Users//Alumnos//Downloads//datasets-20230315T153433Z-001//datasets//autos.csv"
dataAuto <- read.table(namefile, header = TRUE, sep = ",")

autoFile2 <- "C://Users//Alumnos//Downloads//datasets-20230315T153433Z-001//datasets//imports-85.data";
dataAuto2 <- read.table(autoFile2, header = TRUE, sep = ",");

namesCarsDOrig<-c("symboling","normalized-losses","make","fuel-type","aspiration",
                  "num-of-doors","body-style","drive-wheels","engine-location","wheel-base",
                  "length","width","height","curb-weight","engine-type","num-of-cylinders","engine-size",
                  "fuel-system","bore","stroke","compressio-ratio","horsepower","peak-rom","city-mpg","highway-mpg",
                  "price");
filtroCol<-seq(from=4,to=14)
dataCars<-dataAuto2[,filtroCol]
namesCarsD<-namesCarsDOrig[filtroCol]
names(dataCars)<-namesCarsD

summary(dataCars)
cualitatFeat<-seq(from=1,to=6)
cuantitaFeat<-seq(form=7,to=11)
#convertir datos categoricos
dataCars_Cualit<-lapply(cualitatFeat,function(x) factor(dataCars[,x]))
names(dataCars_Cualit)<-namesCarsD[cualitatFeat]
dataCars_Cualit<-as.data.frame(dataCars_Cualit)
summary(dataCars_Cualit)

#Eliminar NA
indicesNA <- which(rowSums(is.na(dataCars_Cualit))!=0)
print(indicesNA)
dataCars_Cualit<-dataCars_Cualit[rowSums(is.na(dataCars_Cualit))==0,]
summary(dataCars_Cualit)

table(dataCars_Cualit)
obj1 <- 1
obj2 <- 4
indObj <- c(1,4)
dataTwo <- dataCars_Cualit[indObj,]
#Caracteristica 1
tabCoinc1 <- table(dataTwo[,1],dataTwo[,1])
print(tabCoinc1)
#caracteristica 4
tabCoinc4 <- table(dataTwo[,4],dataTwo[,4])
print(tabCoinc4)
sum(tabCoinc1==2)
sum(tabCoinc4==2)

searchCoinc <- function(FC_objetosXY){
  tabCoinc1 <- table(FC_objetosXY,FC_objetosXY)
  #print(tabCoinc1)
  return(sum(tabCoinc1==2))
}

similEmpExt <- function(objetosXY_df){
  namesFC <- names(objetosXY_df) #nombres de las caracteristicas
  K <- length(names(dataTwo)) #numero de carac
  similFeatures <- sapply(namesFC, function(x) searchCoinc(objetosXY_df[,x]))
  #print(similFeatures)
  simil_xy <- sum(similFeatures)/K
  return(simil_xy)
}

similEmpExt(dataTwo)
indObj <- c(1,6)
dataTwo <- dataCars_Cualit[indObj,]
similEmpExt(dataTwo)

#matriz entre todos los objetos
N <- dim(dataCars_Cualit)[1]
indObj <- seq(1:N)
similMat <- sapply(indObj,function(x) sapply(indObj, function(y) similEmpExt(dataCars_Cualit[c(x,y),])))

#######################
##Datos cuantitativos##
#######################
#convertir datos
cuantitaFeat<-seq(from=7,to=11)
dataCars_Cuanti<- dataCars[,cuantitaFeat]
names(dataCars_Cuanti)<-namesCarsD[cuantitaFeat]
dataCars_Cuanti<-as.data.frame(dataCars_Cuanti)
summary(dataCars_Cuanti)

#eliminar NA
indicesNA <- which(rowSums(is.na(dataCars_Cuanti))!=0)
print(indicesNA)
dataCars_Cuanti<-dataCars_Cuanti[rowSums(is.na(dataCars_Cuanti))==0,]
summary(dataCars_Cuanti)

calcularSim <- function(a,b) 1/(1 + sqrt( sum((a-b) ^ 2)))

#1 y 2
indObj <- c(1,1)
dataTwo <- dataCars_Cuanti[indObj,]
print(dataTwo)
calcularSim(dataCars_Cuanti[1,],dataCars_Cuanti[2,])


#matriz entre todos los objetos
N <- dim(dataCars_Cuanti)[1]
indObj <- seq(1:N)
similMatCuant <- sapply(indObj,function(x) sapply(indObj, function(y) calcularSim(dataCars_Cuanti[x,],dataCars_Cuanti[y,])))

calcularDistEuclidiana <- function(a,b) sqrt(sum((a-b) ^ 2))

#wb <- c(99.8,99.4,99.8,105.8,95.3,98.8,107.9)

N <- dim(dataCars_Cuanti)[1]
indObj <- seq(1:N)
similMatCuant <- sapply(indObj,function(x) sapply(indObj, function(y) calcularDistEuclidiana(dataCars_Cuanti[x,],dataCars_Cuanti[y,])))
                    


##K Means w/autos.csv                      

dataCarsKMeans<-dataAuto[,7:10]
indObj <- seq(1:12)
KMeansDE <- sapply(indObj,function(x) sapply(indObj, function(y) calcularDistEuclidiana(dataCarsKMeans[x,],dataCarsKMeans[y,])))
distColum2<-KMeansDE[2,]
distColum9<-KMeansDE[9,]
print(distColum2)
grupo<-(distColum2<distColum9)
grupo[distColum2<distColum9]<-1
grupo[distColum2>=distColum9]<-2
print(grupo)
Medias<-grupo
indx1<-which(grupo %in% c(1))
indx2<-which(grupo %in% c(2))

Medias<-apply(indx1,function(x) dataCarsKMeans[x,], simplify = TRUE)
