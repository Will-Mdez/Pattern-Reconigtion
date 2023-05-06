#Importar librerías si es que se ocupan
library(dplyr)
#Cargar el dataset 5

#setwd("C://Users//uribe//OneDrive//Documentos//GitHub//ReconocimientoPatrones")
setwd("D://GitHub//ReconocimientoPatrones")

nameFileG <- "ReconocimientoPerfiles_JD.csv"

RPerfilesTable <- read.table(nameFileG, header = TRUE, sep =',')

#Descripción de los datos

summary(RPerfilesTable)

cualitativos <- c(2,3,5,6,7,8,9,10,11,13,14,15,16,20,21,22,25,26,27,28,30,31,32,33,34,35)
cuantitativos<- c(1,4,12,17,18,19,23,24,29)

nombresTabla <- colnames(RPerfilesTable)

#Separamos los datos categóricos
RPerfilesTable2 <- lapply(cualitativos,
                          function(x) factor(RPerfilesTable[,x]))

names(RPerfilesTable2) <- nombresTabla[cualitativos]
RPerfilesTable2 <- as.data.frame(RPerfilesTable2)

summary(RPerfilesTable2)

#Datos cuantitativos
RPerfiles_cuanti<-RPerfilesTable[cuantitativos]

#Imputación de datos (NO SE HACE)

#Identificación de valores extremos
dataRPerfSnEx<-filter(RPerfiles_cuanti,Edad<(mean_features[1]+3*sd_features[1]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Integrantes.Familia.num<(mean_features[2]+3*sd_features[2]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Promedio.Preparatoria<(mean_features[3]+3*sd_features[3]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Materias.Cursadas.Primer.Semestre<(mean_features[4]+3*sd_features[4]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Promedio.Primer.Semestre<(mean_features[5]+3*sd_features[5]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Materias.Aprobadas.Primer.Semestre<(mean_features[6]+3*sd_features[6]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Horas.Promedio.Estudio.Examenes<(mean_features[7]+3*sd_features[7]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Horas.Promedio.Estudio.Actividades.Escolares<(mean_features[8]+3*sd_features[8]))
dataRPerfSnEx<-filter(dataRPerfSnEx,Horas.Semana.Divertirse.con.sus.amigos<(mean_features[9]+3*sd_features[9]))

#Normalización de datos
FeatNames<-nombresTabla[cuantitativos]
mean_features <- sapply(FeatNames, function(x) mean(dataRPerfSnEx[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(dataRPerfSnEx[[x]]))

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}

dataRPerfNorm <- lapply(FeatNames, function (x) normalizeaDataL(dataRPerfSnEx[[x]], mean_features[x], sd_features[x]))

names(dataRPerfNorm)<- FeatNames  
dataRPerfNorm <- as.data.frame(dataRPerfNorm)