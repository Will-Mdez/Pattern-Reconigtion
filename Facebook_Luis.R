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
)] <- "Ninguno"
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

dataFBNorm <- lapply(FeatNames, function (x) normalizeaDataL(dataFB_cuanti[[x]], mean_features[x], sd_features[x]))

names(dataFBNorm)<- FeatNames  
dataFBNorm <- as.data.frame(dataFBNorm)
summary(dataFBNorm)

#Identificación de valores extremos

dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$`Nro Caracteres del nombre`<(mean_features[1]+3*sd_features[1]))
dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$Amigos<(mean_features[2]+3*sd_features[2]))
dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$`Páginas que siguen`<(mean_features[3]+3*sd_features[3]))
names(dataFBSE)<-names(dataFB_cuanti)
summary(dataFBSE)


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
valores <- c('Lunes', 'Martes','Miércoles','Jueves','Viernes','Sábado','Domingo','Ninguno')
dataCopy$`Día de la semana con más actividad` <- match(dataCopy$`Día de la semana con más actividad`, valores)

summary(dataCopy)


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

#Entropia
N<-377
P_00<-table(dataFacebook_cualit$Foto.de.Perfil[dataFacebookFinal$clase==0])
P_00
#P_00<-c(P_00[['No']],P_00[['Sí']])
P_01<-table(dataFacebook_cualit$Foto.de.Perfil[dataFacebookFinal$clase==1])
P_01
#P_01<-c(P_01[['No']],P_01[['Sí']])

P_10<-table(dataFacebook_cualit$Día.de.la.semana.con.más.actividad[dataFacebookFinal$clase==0])
P_10
#P_10<-c(P_10[['Domingo']],P_10[['Jueves']],P_10[['Lunes']],P_10[['Martes']],P_10[['Miércoles']],P_10[['Ninguno']],P_10[['Sábado']],P_10[['Viernes']])
P_11<-table(dataFacebook_cualit$Día.de.la.semana.con.más.actividad[dataFacebookFinal$clase==1])
P_11
#P_11<-c(P_11[['Domingo']],P_11[['Jueves']],P_11[['Lunes']],P_11[['Martes']],P_11[['Miércoles']],P_11[['Ninguno']],P_11[['Sábado']],P_11[['Viernes']])

#Entropia Foto Perfil
EV_00<--(P_00[1]/377*log2(P_00[1]/377)+P_01[1]/377*log2(P_01[1]/377))
EV_01<--(P_00[2]/377*log2(P_00[2]/377)+P_01[2]/377*log2(P_01[2]/377))
Entropia0<-((P_00[1]+P_01[1])*EV_00+(P_00[2]+P_01[2])*EV_01)/377

EV_10<--(P_10[1]/377*log2(P_10[1]/377)+P_11[1]/377*log2(P_11[1]/377))
EV_11<--(P_10[2]/377*log2(P_10[2]/377)+P_11[2]/377*log2(P_11[2]/377))
EV_12<--(P_10[3]/377*log2(P_10[3]/377)+P_11[3]/377*log2(P_11[3]/377))
EV_13<--(P_10[4]/377*log2(P_10[4]/377)+P_11[4]/377*log2(P_11[4]/377))
EV_14<--(P_10[5]/377*log2(P_10[5]/377)+P_11[5]/377*log2(P_11[5]/377))
EV_15<--(P_10[8]/377*log2(P_10[8]/377)+P_11[8]/377*log2(P_11[8]/377))
EV_16<--(P_10[9]/377*log2(P_10[9]/377)+P_11[9]/377*log2(P_11[9]/377))
EV_10
EV_11
EV_12
EV_13
EV_14
EV_15
EV_16



Entropia1<-((P_10[1]+P_11[1])*EV_10+(P_10[2]+P_11[2])*EV_11+(P_10[3]+P_11[3])*EV_12+(P_10[4]+P_11[4])*EV_13+(P_10[5]+P_11[5])*EV_14+(P_10[8]+P_11[8])*EV_15+(P_10[9]+P_11[9]))
Entropia1<-Entropia1/377
Entropia1

#Factor de Fisher
P1<-115/364
P2<-249/364

mean_featuresNorm <- sapply(c(1,2,3), function(x) mean(dataFBSE[[x]]))
sd_featuresNorm <- sapply(c(1,2,3), function(x) sd(dataFBSE[[x]]))

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}

dataFBNorm <- lapply(c(1,2,3), function (x) normalizeaDataL(dataFBSE[[x]], mean_featuresNorm[x], sd_featuresNorm[x]))

names(dataFBNorm)<- nombresTabla[c(1,3,4)]  
dataFBNorm <- as.data.frame(dataFBNorm)
Clase<-dataFBSE$clase
dataFBNorm <-cbind(dataFBNorm,Clase)

meandataNorm <- sapply(c(1,2,3), function(x) mean(dataFBNorm[[x]]))
SDdataNorm <- sapply(c(1,2,3), function(x) sd(dataFBNorm[[x]]))

#Se calculan las medias y desv.
dataFB_Clase0<-filter(dataFBNorm,dataFBNorm$Clase==0)
dataFB_Clase1<-filter(dataFBNorm,dataFBNorm$Clase==1)

meandataC0<-sapply(c(1,2,3), function(x) mean(dataFB_Clase0[[x]]))
SDdataC0 <- sapply(c(1,2,3), function(x) sd(dataFB_Clase0[[x]]))

meandataC1<-sapply(c(1,2,3), function(x) mean(dataFB_Clase1[[x]]))
SDdataC1 <- sapply(c(1,2,3), function(x) sd(dataFB_Clase1[[x]]))

#Se calcula el factor de Fisher
FFNC<-P1*meandataC0[1]^2+P2*meandataC1[1]^2
FFNC<-FFNC/(P1*SDdataC0[1]^2+P2*SDdataC1[1]^2)

FFNA<-P1*meandataC0[2]^2+P2*meandataC1[2]^2
FFNA<-FFNA/(P1*SDdataC0[2]^2+P2*SDdataC1[2]^2)

FFPS<-P1*meandataC0[3]^2+P2*meandataC1[3]^2
FFPS<-FFPS/(P1*SDdataC0[3]^2+P2*SDdataC1[3]^2)

#F1<-Número Amigos
#Paso 3: Correlacion F1-Resto

F1_NumCarac<- sum(dataFBNorm$Numero_Caracteres*dataFBNorm$Numero_Amigos)
F1_NumCarac<-F1_NumCarac/sqrt(sum(dataFBNorm$Numero_Caracteres^2)*sum(dataFBNorm$Numero_Amigos^2))

F1_NumPagS<- sum(dataFBNorm$Numero_paginas_seguidas*dataFBNorm$Numero_Amigos)
F1_NumPagS<-F1_NumPagS/sqrt(sum(dataFBNorm$Numero_paginas_seguidas^2)*sum(dataFBNorm$Numero_Amigos^2))

#Paso 4: Seleccionar 2a Característica
alpha1<-0.5
alpha2<-0.5

FS_nc<-alpha1*FFNC-alpha2*abs(F1_NumCarac)
FS_nc
FS_nps<-alpha1*FFNA-alpha2*abs(F1_NumPagS)
FS_nps

