
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv";
namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"

dataTwitter <- read.table(namefile, header = TRUE, sep = ",")
dataTwitter2 <- read.table(namefile2, header = TRUE, sep = ",")

##Nombre a las columnas de datos
names(dataTwitter)
#dimensiones
dim(dataTwitter)

columnas1<-c(2,3,4,5,6,7,8,9,11)
columnas2<-c(2,3,7,4,5,6,8,9,10)
dataTwitter<-dataTwitter[columnas1]
nombresdata <- colnames(dataTwitter)
dataTwitter2<-dataTwitter2[columnas2]
names(dataTwitter2)<-nombresdata


#Reasignamos, para empatar Valores
dataTwitter[dataTwitter=="si"]<-"Sí"
dataTwitter[dataTwitter=="no"]<-"No"
dataTwitter[dataTwitter=="real"]<-"Real"
dataTwitter[dataTwitter=="FALSO"]<-"Fake"

dataTwitter[dataTwitter=="lunes"]<-"Lunes"
dataTwitter[dataTwitter=="martes"]<-"Martes"
dataTwitter[dataTwitter=="miercoles"]<-"Miércoles"
dataTwitter[dataTwitter=="jueves"]<-"Jueves"
dataTwitter[dataTwitter=="viernes"]<-"Viernes"
dataTwitter[dataTwitter=="sabado"]<-"Sábado"
dataTwitter[dataTwitter=="domingo"]<-"Domingo"

#UNIMOS DATASES
dataTwitterFinal <- rbind(dataTwitter,dataTwitter2)
#Descripcion de datos
summary(dataTwitterFinal)

#Convertir los datos a Categoricos
dataTwitterFinal$clase<-factor(dataTwitterFinal$clase)
dataTwitterFinal$perfil_privado<-factor(dataTwitterFinal$perfil_privado)
dataTwitterFinal$foto_de_perfil<-factor(dataTwitterFinal$foto_de_perfil)
dataTwitterFinal$dia_mayor_cantidad_twitts<-factor(dataTwitterFinal$dia_mayor_cantidad_twitts)
dataTwitterFinal$comenta_publicaciones<-factor(dataTwitterFinal$comenta_publicaciones)


#Descripcion de los datos
summary(dataTwitterFinal)
dim(dataTwitterFinal)


#IMPUTAR DATOS NA
#Descripcion de los datos
dataTwitterFinal$comenta_publicaciones[is.na(dataTwitterFinal$comenta_publicaciones)]<-"Sí"
dataTwitterFinal$dia_mayor_cantidad_twitts[is.na(dataTwitterFinal$dia_mayor_cantidad_twitts)]<-"Viernes"

summary(dataTwitterFinal)
#sum(is.na(dataTwitterFinal))
#dim(dataTwitterFinal)
#dataTwitterFinal <- na.omit(dataTwitterFinal)
#summary(dataTwitterFinal)
dim(dataTwitterFinal)

#names Cuantitativos
namesCuali<-c("perfil_privado","seguidores","perfil_privado","dia_mayor_cantidad_twitts","comenta_publicaciones","clase")
namesCuali

dataTwitterCuanti<-dataTwitterFinal[c(1,4,5,6)]

#hacer promedio y desv estandar a todas las caracteristicas

meanDia1 <- mean(dataTwitterCuanti$twitts_por_dia[dataTwitterFinal$clase == "Real"],na.rm = TRUE)
meanDia2 <- mean(dataTwitterCuanti$twitts_por_dia[dataTwitterFinal$clase == "Fake"],na.rm = TRUE)
meanDia1
meanDia2
dataTwitterCuanti$twitts_por_dia[is.na(dataTwitterCuanti$twitts_por_dia)&dataTwitterFinal$clase == "Real"] <- meanDia1
dataTwitterCuanti$twitts_por_dia[is.na(dataTwitterCuanti$twitts_por_dia)&dataTwitterFinal$clase == "Fake"] <- meanDia2


#Normalización de datos
FeatNames<-colnames(dataTwitterCuanti)
mean_features <- sapply(FeatNames, function(x) mean(dataTwitterCuanti[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(dataTwitterCuanti[[x]]))
mean_features
sd_features


#Normalizar datos 
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataF - meanF)/stdF
  return(dataFN)
}

dataTwitterNorm <- lapply(FeatNames, function (x) normalizeDataL(dataTwitterCuanti[[x]], mean_features[x], sd_features[x]))

names(dataTwitterNorm)<- FeatNames  
dataTwitterNorm <- as.data.frame(dataTwitterNorm)

Clase<-dataTwitterFinal$clase
dataTwitterNorm<-cbind(dataTwitterNorm,Clase)
summary(dataTwitterNorm)

summary(dataTwitterCuanti)
mean_features

#Identificación de valores extremos
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$num_caracteres_nombre_usuario<(mean_features[1]+3*sd_features[1]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$seguidores<(mean_features[2]+3*sd_features[2]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$perfiles_seguidos<(mean_features[3]+3*sd_features[3]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$twitts_por_dia<(mean_features[4]+3*sd_features[4]))

dataTwitterEx







