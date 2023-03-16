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
