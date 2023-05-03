
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"

namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
dataTwitter <- read.table(namefile, header = TRUE, sep = ",")

#names Cuantitativos
names<-c("num_caracteres_nombre_usuario","seguidores","perfiles_seguidos","twitts_por_dia","cantidad_twitts","clase")


##Nombre a las columnas de datos
names(dataTwitter)
#dimensiones
dim(dataTwitter)
#Descripcion de datos
summary(dataTwitter)

#Convertir los datos a Categoricos
dataTwitter$clase<-factor(dataTwitter$clase)
#Descripcion de los datos
summary(dataTwitter)


#Eliminando Registros con NA
dataTwitterNA <- dataTwitter
dataTwitter1 <- dataTwitterNA[rowSums(is.na(dataTwitterNA))==0,]
summary(dataTwitter1)
print(dim(dataTwitter1))
print(dim(dataTwitterNA))
print(dim(dataTwitter))


#NOrmalizacion
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN<- dataF
  dataFN<- (dataFN-meanF)/stdF
  return(dataFN)
}


mean_features <- sapply(names,function(x) mean(dataTwitterNA[x]))
sd_features <- sapply(names,function(x) sd(dataTwitterNA[[x]]))

dataTwitterNorm <- lapply(names,function(x) normalizeDataL(dataTwitter[[x]],mean_features[x],sd_features[x]))
names(dataWineNorm)<-names
dataWineNorm<-as.data.frame(dataWineNorm)
#head(dataWineNorm)


















