#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"

#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv";
namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv"

dataFacebook <- read.table(namefile, header = TRUE, sep = ",")

dataFacebook2 <- read.table(namefile2, header = TRUE, sep = ",")

namesF <- c("Nro Caracteres del nombre",
            "Foto de Perfil",
            "Tiempo en FB al día",
            "Publicaciones al día",
            "Amigos",
            "Páginas que siguen",
            "comenta_publicaciones"
            ,"Día de la semana con más actividad","clase")
namesF2<-c("instancia","caracteres_nombre",
"foto_perfil","perfil_privado","num_amigos","seguidores",
"publicaciones_dia","dias_masPublicaciones","paginas_sigueOgustan","amigos_comun","publicaciones_mes","tipo_perfil")

##Nombre a las columnas de datos
names(dataFacebook) <- namesF
names(dataFacebook)
names(dataFacebook2)<-namesF2
names(dataFacebook2)

summary(dataFacebook)
summary(dataFacebook2)
#Reasignamos, para empatar Valores
dataFacebook<-dataFacebook[c(1,2,5,6,8,9)]
nombresTabla <- colnames(dataFacebook)
dataFacebook2<-dataFacebook2[c(2,3,5,9,8,12)]


names(dataFacebook2)<-nombresTabla

dataFacebook2[dataFacebook2=="SI"]<-"Sí"
dataFacebook2[dataFacebook2=="NO"]<-"No"
dataFacebook2[dataFacebook2==TRUE]<-0
dataFacebook2[dataFacebook2==FALSE]<-1

dataFacebook2[dataFacebook2=="Miercoles"]<-"Miércoles"
dataFacebook2[dataFacebook2=="Sabado"]<-"Sábado"
summary(dataFacebook)
summary(dataFacebook2)
#UNIMOS DATASETS
dataFacebookFinal <- rbind(dataFacebook,dataFacebook2)

#Descripción de los datos

summary(dataFacebookFinal)

#Convertir los datos categóricos

cualitativos <- c(2,5)
cuantitativos<- c(1,3,4,6)

#Separamos los datos categóricos
dataFacebook_cualit <- lapply(cualitativos,
                         function(x) factor(dataFacebookFinal[,x]))

names(dataFacebook_cualit) <- nombresTabla[cualitativos]
dataFacebook_cualit <- as.data.frame(dataFacebook_cualit)

summary(dataFacebook_cualit)

#Datos cuantitativos
fb_cuanti<-dataFacebookFinal[cuantitativos]
summary(fb_cuanti)

#Imputación de datos
#Imputación con valor de la media condicionada
dataFace <- fb_cuanti

meannumAmig1 <- mean(dataFace$Numero_Amigos[dataFacebookFinal$Clase == 0],na.rm = TRUE)
meannumAmig2 <- mean(dataFace$Numero_Amigos[dataFacebookFinal$Clase == 1],na.rm = TRUE)

fb_cuanti$Numero_Amigos[is.na(fb_cuanti$Numero_Amigos)&dataFacebookFinal$Clase == 0] <- meannumAmig1
fb_cuanti$Numero_Amigos[is.na(fb_cuanti$Numero_Amigos)&dataFacebookFinal$Clase == 1] <- meannumAmig2

meanpagsig1 <- mean(dataFace$Numero_paginas_seguidas[dataFacebookFinal$Clase == 0],na.rm = TRUE)
meanpagsig2 <- mean(dataFace$Numero_paginas_seguidas[dataFacebookFinal$Clase == 1],na.rm = TRUE)

fb_cuanti$Numero_paginas_seguidas[is.na(fb_cuanti$Numero_paginas_seguidas)&dataFacebookFinal$Clase == 0] <- meanpagsig1
fb_cuanti$Numero_paginas_seguidas[is.na(fb_cuanti$Numero_paginas_seguidas)&dataFacebookFinal$Clase == 1] <- meanpagsig2

#Normalización de datos
FeatNames<-nombresTabla[cuantitativos]
mean_features <- sapply(FeatNames, function(x) mean(fb_cuanti[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(fb_cuanti[[x]]))

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}

dataFBNorm <- lapply(FeatNames, function (x) normalizeaDataL(fb_cuanti[[x]], mean_features[x], sd_features[x]))

names(dataFBNorm)<- FeatNames  
dataFBNorm <- as.data.frame(dataFBNorm)


#Identificación de valores extremos
dataSnEx<-filter(fb_cuanti,Numero_Caracteres<(mean_features[1]+3*sd_features[1]))
dataSnEx<-filter(dataSnEx,Numero_Amigos<(mean_features[2]+3*sd_features[2]))
dataSnEx<-filter(dataSnEx,Numero_paginas_seguidas<(mean_features[3]+3*sd_features[3]))

