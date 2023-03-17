autoFile <- "C://Users//Alumnos//Downloads//datasets-20230315T153433Z-001//datasets//autos.csv";
dataAuto <- read.table(autoFile, header = TRUE, sep = ",");

summary(dataAuto)


autoFile2 <- "C://Users//Alumnos//Downloads//datasets-20230315T153433Z-001//datasets//imports-85.data";
dataAuto2 <- read.table(autoFile2, header = TRUE, sep = ",");

summary(dataAuto2)
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

obj1<-1
obj4<-4
indObj<-c(obj1,obj4)
dataTwo<-dataCars_Cualit[indObj,]
#caracteristica1
tabCoinc1<-table(dataTwo[,1],dataTwo[,1])
print(tabCoinc1)
#caracteristica4
tabCoinc4<-table(dataTwo[,4],dataTwo[,4])
print(tabCoinc4)
sum(tabCoinc1==2)
sum(tabCoinc4==2)

searchCoinc<-function(FC_objetosXY){
  tabCoinc1<-table(FC_objetosXY,FC_objetosXY)
  #print(tabCoinc1)
  return(sum(tabCoinc1==2))
}

similEmpExt<-function(objetosXY_df){
  namesFC<-names(objetosXY_df)
  K<-length(names(dataTwo))
  similFeatures<-sapply(namesFC,function(x) searchCoinc(objetosXY_df[,x]))
 # print(similFeatures)
  simil_xy<-sum(similFeatures)/K
  return(simil_xy)
}

similEmpExt(dataTwo)
obj1<-1
obj2<-6

indObj<-c(obj1,obj2)
dataTwo<-dataCars_Cualit[indObj,]
#MAtriz de similitudes dentre dlos diferentes objetos 
N<-dim(dataCars_Cualit)[1]
indObj<-seq(1:N)
similMat<-sapply(indObj,function(x) sapply(indObj,function(y) similEmpExt(dataCars_Cualit[c(x,y),])))
print(similMat)

#_Similitud Cuantitativas Distancia Euclidena
searchCoinCuant<-function(FC_objetosXY){
  tabCoinc1<-table(FC_objetosXY,FC_objetosXY)
  #print(tabCoinc1)
  return(sum(tabCoinc1==2))
}
similEmpExtCuant<-function(objetosXY_df){
  namesFC<-names(objetosXY_df)
  K<-length(names(dataTwo))
  similFeatures<-sapply(namesFC,function(x) searchCoinc(objetosXY_df[,x]))
  # print(similFeatures)
  simil_xy<-sum(similFeatures)/K
  return(simil_xy)
}