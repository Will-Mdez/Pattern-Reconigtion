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
dataFacebook2[dataFacebook2=="si"]<-"Sí"
dataFacebook[dataFacebook=="SÍ"]<-"Sí"
dataFacebook2[dataFacebook2=="NO"]<-"No"
dataFacebook2[dataFacebook2==TRUE]<-0
dataFacebook2[dataFacebook2==FALSE]<-1
dataFacebook2[dataFacebook2=="Miercoles"]<-"Miércoles"
dataFacebook2[dataFacebook2=="Sabado"]<-"Sábado"
dataFacebook2[dataFacebook2=="jueves"]<-"Jueves"
dataFacebook2[dataFacebook2=="viernes"]<-"Viernes"
summary(dataFacebook)
summary(dataFacebook2)
#UNIMOS DATASETS
dataFacebookFinal <- rbind(dataFacebook,dataFacebook2)

#Descripción de los datos

summary(dataFacebookFinal)
dataFacebookFinal <- as.data.frame(dataFacebookFinal)
dataFacebookFinal[dataFacebookFinal=="NaN"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Lunes"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Martes"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Miércoles"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Jueves"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Viernes"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Sábado"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Domingo"]<-"Publica"
dataFacebookFinal[dataFacebookFinal=="Ninguno"]<-"No Publica"
summary(dataFacebookFinal)

dim(dataFacebookFinal)
dataFacebook2[dataFacebook2=="viernes"]<-"Viernes"

#Convertir los datos a Categoricos
dataFacebookFinal$clase<-factor(dataFacebookFinal$clase)
dataFacebookFinal$`Día de la semana con más actividad`<-factor(dataFacebookFinal$`Día de la semana con más actividad`)
dataFacebookFinal$`Foto de Perfil`<-factor(dataFacebookFinal$`Foto de Perfil`)
dataFacebookFinal$`Páginas que siguen` <- as.numeric(dataFacebookFinal$`Páginas que siguen`)


#Descripción de los datos

summary(dataFacebookFinal)
dim(dataFacebookFinal)

#Convertir los datos categóricos

#Columnas con datos Cualitativos
cualitativos <- c(2,5)

#Columnas con datos cuantitativos
cuantitativos<- c(1,3,4,6)

#Separamos los datos categóricos
dataFacebook_cualit <- lapply(cualitativos,function(x) factor(dataFacebookFinal[,x]))

names(dataFacebook_cualit) <- nombresTabla[cualitativos]
dataFacebook_cualit <- as.data.frame(dataFacebook_cualit)

summary(dataFacebook_cualit)

#Datos cuantitativos
dataFB_cuanti<-dataFacebookFinal[cuantitativos]
summary(dataFB_cuanti)

#Imputación de datos
dataFacebookFinal$`Día de la semana con más actividad`[is.na(
  dataFacebookFinal$`Día de la semana con más actividad`
)] <- "No Publica"
summary(dataFacebookFinal)
dim(dataFacebookFinal)

#Imputación con valor de la media condicionada
dataFace <- dataFB_cuanti

meanAmig1 <- mean(dataFace$Amigos[dataFacebookFinal$clase == 0],na.rm = TRUE)
meanAmig2 <- mean(dataFace$Amigos[dataFacebookFinal$clase == 1],na.rm = TRUE)
dataFB_cuanti$Amigos[is.na(dataFB_cuanti$Amigos)&dataFacebookFinal$clase == 0] <- meanAmig1
dataFB_cuanti$Amigos[is.na(dataFB_cuanti$Amigos)&dataFacebookFinal$clase == 1] <- meanAmig2

meanPagSig1 <- mean(dataFace$`Páginas que siguen`[dataFacebookFinal$clase == 0],na.rm = TRUE)
meanPagSig2 <- mean(dataFace$`Páginas que siguen`[dataFacebookFinal$clase == 1],na.rm = TRUE)

dataFB_cuanti$`Páginas que siguen`[is.na(dataFB_cuanti$`Páginas que siguen`)&dataFacebookFinal$clase == 0] <- meanPagSig1
dataFB_cuanti$`Páginas que siguen`[is.na(dataFB_cuanti$`Páginas que siguen`)&dataFacebookFinal$clase == 1] <- meanPagSig2
summary(dataFB_cuanti)
dim(dataFB_cuanti)

#Normalización de datos
FeatNames<-names(dataFB_cuanti)
FeatNames
dataFB_cuanti$`Páginas que siguen` <- as.numeric(dataFB_cuanti$`Páginas que siguen`)
dataFB_cuanti$`Nro Caracteres del nombre` <- as.numeric(dataFB_cuanti$`Nro Caracteres del nombre`)
dataFB_cuanti$Amigos <- as.numeric(dataFB_cuanti$Amigos)
dataFB_cuanti$clase <- as.numeric(dataFB_cuanti$clase)
mean_features <- sapply(FeatNames, function(x) mean(dataFB_cuanti[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(dataFB_cuanti[[x]]))

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}

dataFacebookNorm <- lapply(FeatNames, function (x) normalizeaDataL(dataFB_cuanti[[x]], mean_features[x], sd_features[x]))

names(dataFacebookNorm)<- FeatNames  
dataFacebookNorm <- as.data.frame(dataFacebookNorm)
summary(dataFacebookNorm)

#Identificación de valores extremos

#dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$`Nro Caracteres del nombre`<(mean_features[1]+3*sd_features[1]))
#dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$Amigos<(mean_features[2]+3*sd_features[2]))
#dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$`Páginas que siguen`<(mean_features[3]+3*sd_features[3]))
#names(dataFBSE)<-names(dataFB_cuanti)
#summary(dataFBSE)


#DISCRETIZACION
dataFB_ordenado <- dataFB_cuanti[order(dataFB_cuanti$`Nro Caracteres del nombre`, decreasing = FALSE),]
dim(dataFB_ordenado)

N <- dim(dataFB_ordenado)[1]
division <- N/3
print (division)


#Discretizacion Numero de Caracteres
dfFB_caracteres <- dataFB_ordenado$`Nro Caracteres del nombre`
dfFB_caracteres[1:125] <- "Poco"
dfFB_caracteres[126:253]<- "Normal"
dfFB_caracteres[254:377]<- "Muchos"
dfFB_caracteres

#Discretizacion Amigos
dataFB_ordenado <- dataFB_cuanti[order(dataFB_cuanti$Amigos, decreasing = FALSE),]

dfFB_Amigos <- dataFB_ordenado$Amigos
dfFB_Amigos[dataFB_ordenado$Amigos<400] <- "Pocos"
dfFB_Amigos[dataFB_ordenado$Amigos>=400 & dataFB_ordenado$Amigos<=1000]<- "Normal"
dfFB_Amigos[dataFB_ordenado$Amigos>1000]<- "Muchos"
dfFB_Amigos

#Discretizacion Seguidos
dataFB_ordenado <- dataFB_cuanti[order(dataFB_cuanti$`Páginas que siguen`, decreasing = FALSE),]
dfFB_Seguidos <- dataFB_ordenado$`Páginas que siguen`
dfFB_Seguidos[dataFB_ordenado$`Páginas que siguen`<100] <- "Pocos"
dfFB_Seguidos[dataFB_ordenado$`Páginas que siguen`>=100 & dataFB_ordenado$`Páginas que siguen`<=300]<- "Normal"
dfFB_Seguidos[dataFB_ordenado$`Páginas que siguen`>300]<- "Muchos"
dfFB_Seguidos


#Asignamos valores
dataFB_ordenado$`Páginas que siguen`<-dfFB_Seguidos
dataFB_ordenado$`Nro Caracteres del nombre` <- dfFB_caracteres
dataFB_ordenado$Amigos <- dfFB_Amigos
summary(dataFB_ordenado)

dataFB_ordenado$`Páginas que siguen`<-factor(dataFB_ordenado$`Páginas que siguen`)
dataFB_ordenado$`Nro Caracteres del nombre`<-factor(dataFB_ordenado$`Nro Caracteres del nombre`)
dataFB_ordenado$Amigos<-factor(dataFB_ordenado$Amigos)
summary(dataFB_ordenado)
#Discretizacion Datos Cualitativos
dataCopy <- dataFacebookFinal[c(2,5)]

summary(dataCopy)




valores <- c('No', 'Sí')
dataCopy$`Foto de Perfil` <- match(dataCopy$`Foto de Perfil`, valores)
valores <- c('Publica','No Publica')
dataCopy$`Día de la semana con más actividad` <- match(dataCopy$`Día de la semana con más actividad`, valores)

summary(dataCopy)
dataFaceDiscretizado<-cbind(dataFB_ordenado[,1:3],dataFacebookFinal[,c(2,5)],dataFB_ordenado[4])
summary(dataFaceDiscretizado)
dim(dataFaceDiscretizado)
#Frecuencias
#histograma de frecuencias

freqClass <- table(dataFB_ordenado$Amigos)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("Amigos","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=Amigos, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p

#histograma de frecuencias

freqClass <- table(dataCopy$`Foto de Perfil`)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("Foto de Perfil","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=`Foto de Perfil`, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p



#Selección de características

#Rankeo
#Entropia
N<-364

P_00<-table(FBTableFinal$Foto_Perfil[FBTableFinal$Clase==0])
P_01<-table(FBTableFinal$Foto_Perfil[FBTableFinal$Clase==1])

P_10<-table(FBTableFinal$Dia_mas_actividad[FBTableFinal$Clase==0])
P_11<-table(FBTableFinal$Dia_mas_actividad[FBTableFinal$Clase==1])

P_20<-table(FBTableFinal$Numero_Caracteres[FBTableFinal$Clase==0])
P_21<-table(FBTableFinal$Numero_Caracteres[FBTableFinal$Clase==1])

P_30<-table(FBTableFinal$Numero_Amigos[FBTableFinal$Clase==0])
P_31<-table(FBTableFinal$Numero_Amigos[FBTableFinal$Clase==1])

P_40<-table(FBTableFinal$Numero_paginas_seguidas[FBTableFinal$Clase==0])
P_41<-table(FBTableFinal$Numero_paginas_seguidas[FBTableFinal$Clase==1])

#Entropia Foto Perfil
EV_00<--(P_00[1]/N*log2(P_00[1]/N)+P_01[1]/N*log2(P_01[1]/N))
EV_01<--(P_00[2]/N*log2(P_00[2]/N)+P_01[2]/N*log2(P_01[2]/N))
E0<-((P_00[1]+P_01[1])*EV_00+(P_00[2]+P_01[2])*EV_01)/N

EV_10<--(P_10[1]/N*log2(P_10[1]/N)+P_11[1]/N*log2(P_11[1]/N))
EV_11<--(P_10[2]/N*log2(P_10[2]/N)+P_11[2]/N*log2(P_11[2]/N))
EV_12<--(P_10[3]/N*log2(P_10[3]/N)+P_11[3]/N*log2(P_11[3]/N))

E1<-((P_10[1]+P_11[1])*EV_10+(P_10[2]+P_11[2])*EV_11+(P_10[3]+P_11[3])*EV_12)/N

EV_20<--(P_20[1]/N*log2(P_20[1]/N)+P_21[1]/N*log2(P_21[1]/N))
EV_21<--(P_20[2]/N*log2(P_20[2]/N)+P_21[2]/N*log2(P_21[2]/N))
EV_22<--(P_20[3]/N*log2(P_20[3]/N)+P_21[3]/N*log2(P_21[3]/N))

E2<-((P_20[1]+P_21[1])*EV_20+(P_20[2]+P_21[2])*EV_21+(P_20[3]+P_21[3])*EV_22)/N

EV_30<--(P_30[1]/N*log2(P_30[1]/N)+P_31[1]/N*log2(P_31[1]/N))
EV_31<--(P_30[2]/N*log2(P_30[2]/N)+P_31[2]/N*log2(P_31[2]/N))
EV_32<--(P_30[3]/N*log2(P_30[3]/N)+P_31[3]/N*log2(P_31[3]/N))

E3<-((P_30[1]+P_31[1])*EV_30+(P_30[2]+P_31[2])*EV_31+(P_30[3]+P_31[3])*EV_32)/N

EV_40<--(P_40[1]/N*log2(P_40[1]/N)+P_41[1]/N*log2(P_41[1]/N))
EV_41<--(P_40[2]/N*log2(P_40[2]/N)+P_41[2]/N*log2(P_41[2]/N))
EV_42<--(P_40[3]/N*log2(P_40[3]/N)+P_41[3]/N*log2(P_41[3]/N))

E4<-((P_40[1]+P_41[1])*EV_40+(P_40[2]+P_41[2])*EV_41+(P_40[3]+P_41[3])*EV_42)/N

#La menor entropía es la de E1 que corresponde a Día Más Actividad


