
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
dataTwitter <- read.table(namefile, header = TRUE, sep = ",")

#names Cuantitativos
names<-c("num_caracteres_nombre_usuario","seguidores","perfiles_seguidos","twitts_por_dia","cantidad_twitts")

names

##Nombre a las columnas de datos
names(dataTwitter)
#dimensiones
dim(dataTwitter)
#Descripcion de datos
summary(dataTwitter)

#Convertir los datos a Categoricos
dataTwitter$clase<-factor(dataTwitter$clase)
dataTwitter$perfil_privado<-factor(dataTwitter$perfil_privado)
dataTwitter$foto_de_perfil<-factor(dataTwitter$foto_de_perfil)
dataTwitter$dia_mayor_cantidad_twitts<-factor(dataTwitter$dia_mayor_cantidad_twitts)
dataTwitter$comenta_publicaciones<-factor(dataTwitter$comenta_publicaciones)

#Descripcion de los datos
summary(dataTwitter)

#hacer promedio y desv estandar a todas las caracteristicas
mean_features <- sapply(names, function(x) mean(dataTwitter[[x]]))
sd_features <- sapply(names, function(x) sd(dataTwitter[[x]]))

#Normalizar datos 
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataF - meanF)/stdF
  return(dataFN)
}

dataTwitterNorm <- lapply(names,function(x) normalizeDataL(dataTwitter[[x]],mean_features[x],sd_features[x]))
names(dataTwitterNorm) <- names
dataTwitterNorm <- as.data.frame(dataTwitterNorm)

mean_features_norm <- sapply(names, function(x) mean(dataTwitterNorm[[x]]))
sd_features_norm <- sapply(names, function(x) sd(dataTwitterNorm[[x]]))


#Numero de registros en el conjunto de datos
N <- dim(dataTwitter)[1]
print(N)
dataTwitterNA <- dataTwitter
dataTwitterNA$twitts_por_dia[rbinom(N,1,0.1) == 1] <- NA
dataTwitterNA$cantidad_twitts[rbinom(N,1,0.1) == 1] <- NA
summary(dataTwitterNA)
#Eliminar registros con NA
dataTwitter1 <- dataTwitterNA[rowSums(is.na(dataTwitterNA)) == 0,]
summary(dataTwitter1)
print(dim(dataTwitter1))



#Discretizacion

#ordenar de menor a mayor
df_ordenado <- dataTwitter[order(dataTwitter$seguidores, decreasing = FALSE),]
print(df_ordenado)

k=3
bite_size <- dim(df_ordenado)[1]/k
df_ordenado$seguidores <- c(rep(c(1,2,3),each=trunc(bite_size)),3)
write.csv(as.data.frame(df_ordenado), file = "Twitter_Ordernado.csv", row.names = names(dataTwitter)) # guarda un archivo csv












