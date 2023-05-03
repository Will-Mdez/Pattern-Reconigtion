#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
dataTwitterLOU <- read.table(namefile, header = TRUE, sep = ",")


#names Cuantitativos
names<-c("cantidad_caracteres_nombre","cantidad_seguidores","cantidad_seguidos","promedio_publicaciones")

##Nombre a las columnas de datos
names(dataTwitterLOU)
#dimensiones
dim(dataTwitterLOU)
#Descripcion de datos
summary(dataTwitterLOU)


#Convertir los datos a Categoricos
dataTwitterLOU$clase<-factor(dataTwitterLOU$clase)
dataTwitterLOU$perfil_privado<-factor(dataTwitterLOU$perfil_privado)
dataTwitterLOU$tiene_foto_perfil<-factor(dataTwitterLOU$tiene_foto_perfil)
dataTwitterLOU$dia_mas_publica<-factor(dataTwitterLOU$dia_mas_publica)
dataTwitterLOU$comenta_publicaciones<-factor(dataTwitterLOU$comenta_publicaciones)

#Descripcion de los datos CATEGORIZADOS
summary(dataTwitterLOU)


#hacer promedio y desv estandar a todas las caracteristicas
mean_features <- sapply(names, function(x) mean(dataTwitterLOU[[x]]))
sd_features <- sapply(names, function(x) sd(dataTwitterLOU[[x]]))

#Normalizar datos 
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataF - meanF)/stdF
  return(dataFN)
}

dataTwitterLOUNorm <- lapply(names,function(x) normalizeDataL(dataTwitterLOU[[x]],mean_features[x],sd_features[x]))
names(dataTwitterLOUNorm) <- names
dataTwitterLOUNorm <- as.data.frame(dataTwitterLOUNorm)

mean_features_norm <- sapply(names, function(x) mean(dataTwitterLOUNorm[[x]]))
sd_features_norm <- sapply(names, function(x) sd(dataTwitterLOUNorm[[x]]))

