
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
#dim(dataTwitterFinal)

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
summary(dataTwitterCuanti)
dim(dataTwitterCuanti)

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
summary(dataTwitterNorm)

names(dataTwitterNorm)<- FeatNames  
dataTwitterNorm <- as.data.frame(dataTwitterNorm)

Clase<-dataTwitterFinal$clase
dataTwitterNorm<-cbind(dataTwitterNorm,Clase)
summary(dataTwitterNorm)

summary(dataTwitterCuanti)
mean_features

#Identificación de valores extremos
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$num_caracteres_nombre_usuario<(mean_features[1]+3*sd_features[1]))
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$seguidores<(mean_features[2]+3*sd_features[2]))
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$perfiles_seguidos<(mean_features[3]+3*sd_features[3]))
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$twitts_por_dia<(mean_features[4]+3*sd_features[4]))

summary(dataTwitterEx)

#Discretizamos para clase con Numero de Caracteres en el Usuario

#DISCRETIZACION
dataTw_Ord <- dataTwitterCuanti[order(dataTwitterCuanti$num_caracteres_nombre_usuario, decreasing = FALSE),]
dim(dataTw_Ord)
N <- dim(dataTw_Ord)[1]
N
division <- N/3
print (division) 

#Discretizacion Numero de Caracteres
dfTw_caracteres <- dataTw_Ord$`Nro Caracteres del nombre`
dfTw_caracteres[1:105] <- "Poco"
dfTw_caracteres[106:210]<- "Normal"
dfTw_caracteres[211:315]<- "Muchos"
dfTw_caracteres

#Discretizacion Seguidos
dataTw_Ord <- dataTwitterCuanti[order(dataTwitterCuanti$perfiles_seguidos, decreasing = FALSE),]
dataTw_Ord
dfTW_Seguidos <- dataTw_Ord$perfiles_seguidos
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos<200] <- "Pocos"
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos>=200 & dataTw_Ord$perfiles_seguidos<=500]<- "Normal"
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos>500]<- "Muchos"
dfTW_Seguidos


#Discretizacion Seguidores
dataTw_Ord <- dataTwitterCuanti[order(dataTwitterCuanti$seguidores, decreasing = FALSE),]
dataTw_Ord
dfTW_Seguidores <- dataTw_Ord$seguidores
dfTW_Seguidores[dataTw_Ord$seguidores<200] <- "Pocos"
dfTW_Seguidores[dataTw_Ord$seguidores>=200 & dataTw_Ord$seguidores<=500]<- "Normal"
dfTW_Seguidores[dataTw_Ord$seguidores>500]<- "Muchos"
dfTW_Seguidores


#Discretizacion Seguidores
dataTw_Ord <- dataTwitterCuanti[order(dataTwitterCuanti$twitts_por_dia, decreasing = FALSE),]
dataTw_Ord
dfTW_TwxD <- dataTw_Ord$twitts_por_dia
dfTW_TwxD[dataTw_Ord$twitts_por_dia<2] <- "Pocos"
dfTW_TwxD[dataTw_Ord$twitts_por_dia>=2 & dataTw_Ord$twitts_por_dia<=3]<- "Normal"
dfTW_TwxD[dataTw_Ord$twitts_por_dia>3]<- "Muchos"
dfTW_TwxD

#Asignamos Valores

dataTw_Ord$num_caracteres_nombre_usuario <- dfTw_caracteres
dataTw_Ord$seguidores <- dfTW_Seguidores
dataTw_Ord$twitts_por_dia <- dfTW_TwxD
dataTw_Ord$perfiles_seguidos <- dfTW_Seguidos
summary(dataTw_Ord)
dataTw_Ord$num_caracteres_nombre_usuario<-factor(dataTw_Ord$num_caracteres_nombre_usuario)
dataTw_Ord$seguidores<-factor(dataTw_Ord$seguidores)
dataTw_Ord$twitts_por_dia<-factor(dataTw_Ord$twitts_por_dia)
dataTw_Ord$perfiles_seguidos<-factor(dataTw_Ord$perfiles_seguidos)
summary(dataTw_Ord)



#Ahora Discretizamos datos Cualitativos
dataCopy <- dataTwitterFinal[c(2,3,7)]

summary(dataCopy)

valores <- c('No', 'Sí')
dataCopy$foto_de_perfil <- match(dataCopy$foto_de_perfil, valores)
dataCopy$perfil_privado <- match(dataCopy$perfil_privado, valores)
valores <- c('Lunes', 'Martes','Miércoles','Jueves','Viernes','Sábado','Domingo')
dataCopy$dia_mayor_cantidad_twitts <- match(dataCopy$dia_mayor_cantidad_twitts, valores)
#Tranformar en

summary(dataCopy)


#Frecuencias
#histograma de frecuencias

freqClass <- table(dataTw_Ord$seguidores)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("seguidores","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=seguidores, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p



#histograma de frecuencias

freqClass <- table(dataCopy$foto_de_perfil)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("foto_de_perfil","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=foto_de_perfil, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p

summary(dataTwitterCuanti)
dim(dataTwitterCuanti)
#Identificación de valores extremos
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$num_caracteres_nombre_usuario<(mean_features[1]+3*sd_features[1]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$seguidores<(mean_features[2]+3*sd_features[2]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$perfiles_seguidos<(mean_features[3]+3*sd_features[3]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterCuanti$twitts_por_dia<(mean_features[4]+3*sd_features[4]))
summary(dataTwitterEx)
dim(dataTwitterEx)

#Selección de características

#Entropia
N<-315
cualitativos <- c(2,3,7,8,9)

dataTwitterCuali<-dataTwitterFinal[cualitativos]
summary(dataTwitterCuali)


P_00<-table(dataTwitterCuali$foto_de_perfil[dataTwitterFinal$clase=="Real"])
P_01<-table(dataTwitterCuali$foto_de_perfil[dataTwitterFinal$clase=="Fake"])
P_00
P_01
P_10<-table(dataTwitterCuali$perfil_privado[dataTwitterFinal$clase=="Real"])
P_11<-table(dataTwitterCuali$perfil_privado[dataTwitterFinal$clase=="Fake"])
P_10
P_11
P_20<-table(dataTwitterCuali$comenta_publicaciones[dataTwitterFinal$clase=="Real"])
P_21<-table(dataTwitterCuali$comenta_publicaciones[dataTwitterFinal$clase=="Fake"])
P_20
P_21
P_30<-table(dataTwitterCuali$dia_mayor_cantidad_twitts[dataTwitterFinal$clase=="Real"])
P_31<-table(dataTwitterCuali$dia_mayor_cantidad_twitts[dataTwitterFinal$clase=="Fake"])
P_30<-P_30[2:9]
P_31<-P_31[2:9]
#Entropia Foto Perfil
EV_00<--(P_00[1]/315*log2(P_00[1]/315)+P_01[1]/315*log2(P_01[1]/315))
EV_01<--(P_00[2]/315*log2(P_00[2]/315)+P_01[2]/315*log2(P_01[2]/315))
E0<-((P_00[1]+P_01[1])*EV_00+(P_00[2]+P_01[2])*EV_01)/315

EV_10<--(P_10[1]/315*log2(P_10[1]/315)+P_11[1]/315*log2(P_11[1]/315))
EV_11<--(P_10[2]/315*log2(P_10[2]/315)+P_11[2]/315*log2(P_11[2]/315))
E1<-((P_10[1]+P_11[1])*EV_10+(P_10[2]+P_11[2])*EV_11)/315

EV_20<--(P_21[1]/315*log2(P_21[1]/315))
EV_21<--(P_20[2]/315*log2(P_20[2]/315)+P_21[2]/315*log2(P_21[2]/315))
E2<-((P_20[1]+P_21[1])*EV_20+(P_20[2]+P_21[2])*EV_21)/315

EV_30<--(P_30[1]/315*log2(P_30[1]/315)+P_31[1]/315*log2(P_31[1]/315))
EV_31<--(P_30[2]/315*log2(P_30[2]/315)+P_31[2]/315*log2(P_31[2]/315))
EV_32<--(P_30[3]/315*log2(P_30[3]/315)+P_31[3]/315*log2(P_31[3]/315))
EV_33<--(P_30[4]/315*log2(P_30[4]/315)+P_31[4]/315*log2(P_31[4]/315))
EV_34<--(P_30[5]/315*log2(P_30[5]/315)+P_31[5]/315*log2(P_31[5]/315))
EV_35<--(P_30[6]/315*log2(P_30[6]/315)+P_31[6]/315*log2(P_31[6]/315))
EV_36<--(P_30[7]/315*log2(P_30[7]/315)+P_31[7]/315*log2(P_31[7]/315))
EV_37<--(P_30[8]/315*log2(P_30[8]/315)+P_31[8]/315*log2(P_31[8]/315))
E3<-((P_30[1]+P_31[1])*EV_30+(P_30[2]+P_31[2])*EV_31+(P_30[3]+P_31[3])*EV_32+(P_30[4]+P_31[4])*EV_33+(P_30[5]+P_31[5])*EV_34)
E3<-E3+((P_30[6]+P_31[6])*EV_35+(P_30[7]+P_31[7])*EV_36+(P_30[8]+P_31[8])*EV_37)
E3<-E3/315

#Factor de Fisher
P1<-88/301
P2<-213/301

mean_dataTwitterNorm <- sapply(c(1,2,3,4), function(x) mean(dataTwitterNorm[[x]]))
sd_dataTwitterNorm <- sapply(c(1,2,3,4), function(x) sd(dataTwitterNorm[[x]]))

#Se calculan las medias y desv.
dataTW_Clase0<-filter(dataTwitterNorm,dataTwitterNorm$Clase=="Real")
dataTW_Clase1<-filter(dataTwitterNorm,dataTwitterNorm$Clase=="Fake")

meandataC0<-sapply(c(1,2,3,4), function(x) mean(dataTW_Clase0[[x]]))
SDdataC0 <- sapply(c(1,2,3,4), function(x) sd(dataTW_Clase0[[x]]))

meandataC1<-sapply(c(1,2,3,4), function(x) mean(dataTW_Clase1[[x]]))
SDdataC1 <- sapply(c(1,2,3,4), function(x) sd(dataTW_Clase1[[x]]))

#Se calcula el factor de Fisher
FFNC<-P1*meandataC0[1]^2+P2*meandataC1[1]^2
FFNC<-FFNC/(P1*SDdataC0[1]^2+P2*SDdataC1[1]^2)

FFS<-P1*meandataC0[2]^2+P2*meandataC1[2]^2
FFS<-FFS/(P1*SDdataC0[2]^2+P2*SDdataC1[2]^2)

FFPS<-P1*meandataC0[3]^2+P2*meandataC1[3]^2
FFPS<-FFPS/(P1*SDdataC0[3]^2+P2*SDdataC1[3]^2)

FFTD<-P1*meandataC0[4]^2+P2*meandataC1[4]^2
FFTD<-FFTD/(P1*SDdataC0[4]^2+P2*SDdataC1[4]^2)

#F1<-Perfiles Seguidos
#Paso 3: Correlacion F1-Resto

F1_NumCarac<- sum(dataTwitterNorm$num_caracteres_nombre_usuario*dataTwitterNorm$perfiles_seguidos)
F1_NumCarac<-F1_NumCarac/sqrt(sum(dataTwitterNorm$num_caracteres_nombre_usuario^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

F1_Seguidores<- sum(dataTwitterNorm$seguidores*dataTwitterNorm$perfiles_seguidos)
F1_Seguidores<-F1_Seguidores/sqrt(sum(dataTwitterNorm$seguidores^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

F1_TwiDia<- sum(dataTwitterNorm$twitts_por_dia*dataTwitterNorm$perfiles_seguidos)
F1_TwiDia<-F1_TwiDia/sqrt(sum(dataTwitterNorm$twitts_por_dia^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

#Paso 4: Seleccionar 2a Característica
alpha1<-0.5
alpha2<-0.5

FS_nc<-alpha1*FFNC-alpha2*abs(F1_NumCarac)
FS_seg<-alpha1*FFS-alpha2*abs(F1_Seguidores)
FS_td<-alpha1*FFTD-alpha2*abs(F1_TwiDia)




