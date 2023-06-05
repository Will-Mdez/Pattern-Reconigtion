library(dplyr) 
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv";
#namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"

namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv";
#namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
#namefile2 <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"


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

#IMPUTAR DATOS NA
#Descripcion de los datos
dataTwitterFinal$comenta_publicaciones[dataTwitterFinal$comenta_publicaciones==""]<-"Sí"
dataTwitterFinal$comenta_publicaciones[is.na(dataTwitterFinal$comenta_publicaciones)]<-"Sí"
dataTwitterFinal$dia_mayor_cantidad_twitts[is.na(dataTwitterFinal$dia_mayor_cantidad_twitts)]<-"Publica"
#Convertir los datos a Categoricos
dataTwitterFinal$clase<-factor(dataTwitterFinal$clase)
dataTwitterFinal$perfil_privado<-factor(dataTwitterFinal$perfil_privado)
dataTwitterFinal$foto_de_perfil<-factor(dataTwitterFinal$foto_de_perfil)
#dataTwitterFinal$dia_mayor_cantidad_twitts<-factor(dataTwitterFinal$dia_mayor_cantidad_twitts)
dataTwitterFinal$comenta_publicaciones<-factor(dataTwitterFinal$comenta_publicaciones)


#Descripcion de los datos
summary(dataTwitterFinal)
dataTwitterFinal <- as.data.frame(dataTwitterFinal)
dataTwitterFinal[dataTwitterFinal=="Lunes"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Martes"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Miércoles"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Jueves"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Viernes"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Sábado"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Domingo"]<-"Publica"
dataTwitterFinal[dataTwitterFinal=="Ninguno"]<-"No Publica"
dataTwitterFinal[dataTwitterFinal==""]<-"No Publica"
dataTwitterFinal$dia_mayor_cantidad_twitts<-factor(dataTwitterFinal$dia_mayor_cantidad_twitts)
summary(dataTwitterFinal)
dim(dataTwitterFinal)




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
dataTwitterCuanti<- data.frame(dataTwitterCuanti)
summary(dataTwitterCuanti)
mean_features

#Identificación de valores extremos
dataTwitterEx<-filter(dataTwitterCuanti,dataTwitterCuanti$num_caracteres_nombre_usuario<(mean_features[1]+3*sd_features[1]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$seguidores<(mean_features[2]+3*sd_features[2]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$perfiles_seguidos<(mean_features[3]+3*sd_features[3]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$twitts_por_dia<(mean_features[4]+3*sd_features[4]))
dataTwitterEx<- as.data.frame(dataTwitterEx)
summary(dataTwitterEx)

#Discretizamos para clase con Numero de Caracteres en el Usuario

#DISCRETIZACION
dataTw_Ord <- dataTwitterEx[order(dataTwitterEx$num_caracteres_nombre_usuario, decreasing = FALSE),]
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
dfTW_Seguidos <- dataTw_Ord$perfiles_seguidos
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos<200] <- "Pocos"
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos>=200 & dataTw_Ord$perfiles_seguidos<=500]<- "Normal"
dfTW_Seguidos[dataTw_Ord$perfiles_seguidos>500]<- "Muchos"
dfTW_Seguidos


#Discretizacion Seguidores
dataTw_Ord <- dataTwitterCuanti[order(dataTwitterCuanti$seguidores, decreasing = FALSE),]
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

dataCopy <- dataTwitterFinal[c(2,3,7,9)]

summary(dataCopy)

valores <- c('No', 'Sí')
dataCopy$foto_de_perfil <- match(dataCopy$foto_de_perfil, valores)
dataCopy$perfil_privado <- match(dataCopy$perfil_privado, valores)
valores <- c('Publica', 'No Publica')
dataCopy$dia_mayor_cantidad_twitts <- match(dataCopy$dia_mayor_cantidad_twitts, valores)
#Tranformar en

summary(dataCopy)

dataTw_Discretizada<-cbind(dataTw_Ord,dataTwitterFinal[c(2,3,7,8,9)])
summary(dataTw_Discretizada)
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
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$seguidores<(mean_features[2]+3*sd_features[2]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$perfiles_seguidos<(mean_features[3]+3*sd_features[3]))
dataTwitterEx<-filter(dataTwitterEx,dataTwitterEx$twitts_por_dia<(mean_features[4]+3*sd_features[4]))
summary(dataTwitterEx)
dim(dataTwitterEx)

#Selección de características

#Entropia
N<-315
cualitativos <- c(2,3,7,8,9)

dataTwitterCuali<-dataTwitterFinal[cualitativos]
summary(dataTwitterCuali)


P00<-table(dataTw_Discretizada$foto_de_perfil[dataTwitterFinal$clase=="Real"])
P01<-table(dataTw_Discretizada$foto_de_perfil[dataTwitterFinal$clase=="Fake"])

P10<-table(dataTw_Discretizada$perfil_privado[dataTwitterFinal$clase=="Real"])
P11<-table(dataTw_Discretizada$perfil_privado[dataTwitterFinal$clase=="Fake"])

##
P20<-table(dataTw_Discretizada$comenta_publicaciones[dataTwitterFinal$clase=="Real"])
P21<-table(dataTw_Discretizada$comenta_publicaciones[dataTwitterFinal$clase=="Fake"])

P30<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts[dataTwitterFinal$clase=="Real"])
P31<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts[dataTwitterFinal$clase=="Fake"])

P40<-table(dataTw_Discretizada$num_caracteres_nombre_usuario[dataTwitterFinal$clase=="Real"])
P41<-table(dataTw_Discretizada$num_caracteres_nombre_usuario[dataTwitterFinal$clase=="Fake"])

P50<-table(dataTw_Discretizada$seguidores[dataTwitterFinal$clase=="Real"])
P51<-table(dataTw_Discretizada$seguidores[dataTwitterFinal$clase=="Fake"])

P60<-table(dataTw_Discretizada$twitts_por_dia[dataTwitterFinal$clase=="Real"])
P61<-table(dataTw_Discretizada$twitts_por_dia[dataTwitterFinal$clase=="Fake"])

##
P70<-table(dataTw_Discretizada$perfiles_seguidos[dataTwitterFinal$clase=="Real"])
P71<-table(dataTw_Discretizada$perfiles_seguidos[dataTwitterFinal$clase=="Fake"])

#Entropia Foto Perfil
EV00<--(P00[1]/315*log2(P00[1]/315)+P01[1]/315*log2(P01[1]/315))
EV01<--(P00[2]/315*log2(P00[2]/315)+P01[2]/315*log2(P01[2]/315))
E0<-((P00[1]+P01[1])*EV00+(P00[2]+P01[2])*EV01)/315

EV10<--(P10[1]/315*log2(P10[1]/315)+P11[1]/315*log2(P11[1]/315))
EV11<--(P10[2]/315*log2(P10[2]/315)+P11[2]/315*log2(P11[2]/315))
E1<-((P10[1]+P11[1])*EV10+(P10[2]+P11[2])*EV11)/315

EV20<--(P21[1]/315*log2(P21[1]/315)+P21[1]/315*log2(P21[1]/315))
EV21<--(P20[2]/315*log2(P20[2]/315)+P21[2]/315*log2(P21[2]/315))
E2<-((P20[1]+P21[1])*EV20+(P20[2]+P21[2])*EV21)/315

EV30<--(P30[1]/315*log2(P30[1]/315)+P31[1]/315*log2(P31[1]/315))
EV31<--(P30[2]/315*log2(P30[2]/315)+P31[2]/315*log2(P31[2]/315))
E3<-((P30[1]+P31[1])*EV30+(P30[2]+P31[2])*EV31)/315


EV40<--(P40[1]/315*log2(P40[1]/315)+P41[1]/315*log2(P41[1]/315))
EV41<--(P40[2]/315*log2(P40[2]/315)+P41[2]/315*log2(P41[2]/315))
EV42<--(P40[3]/315*log2(P40[3]/315)+P41[3]/315*log2(P41[3]/315))
E4<-((P40[1]+P41[1])*EV40+(P40[2]+P41[2])*EV41+(P40[3]+P41[3])*EV42)/315


EV50<--(P50[1]/315*log2(P50[1]/315)+P51[1]/315*log2(P51[1]/315))
EV51<--(P51[2]/315*log2(P51[2]/315))
EV52<--(P50[3]/315*log2(P50[3]/315)+P51[3]/315*log2(P51[3]/315))
E5<-((P50[1]+P51[1])*EV50+(P50[2]+P51[2])*EV51+(P50[3]+P51[3])*EV52)/315


EV60<--(P60[1]/315*log2(P60[1]/315)+P61[1]/315*log2(P61[1]/315))
EV61<--(P60[2]/315*log2(P60[2]/315)+P61[2]/315*log2(P61[2]/315))
EV62<--(P60[3]/315*log2(P60[3]/315)+P61[3]/315*log2(P61[3]/315))
E6<-((P60[1]+P61[1])*EV60+(P60[2]+P61[2])*EV61+(P60[3]+P61[3])*EV62)/315


EV70<--(P70[1]/315*log2(P70[1]/315))
EV71<--(P70[2]/315*log2(P70[2]/315)+P71[2]/315*log2(P71[2]/315))
EV72<--(P70[3]/315*log2(P70[3]/315)+P71[3]/315*log2(P71[3]/315))
E7<-((P70[1]+P71[1])*EV70+(P70[2]+P71[2])*EV71+(P70[3]+P71[3])*EV71)/315

#Entropia->comenta_publicaciones
EntropiasTW<-c(E3,E4,E5,E7,E6,E0,E1,E2)
sort(EntropiasTW)

#Factor de Fisher
P1<-88/301
P2<-213/301

mean_dataTwitterNorm <- sapply(c(1,2,3,4), function(x) mean(dataTwitterNorm[[x]]))
sd_dataTwitterNorm <- sapply(c(1,2,3,4), function(x) sd(dataTwitterNorm[[x]]))

#Medias y desviacion
dataTwitter_Clase0<-filter(dataTwitterNorm,dataTwitterNorm$Clase=="Real")
dataTwitter_Clase1<-filter(dataTwitterNorm,dataTwitterNorm$Clase=="Fake")

meandataC0<-sapply(c(1,2,3,4), function(x) mean(dataTwitter_Clase0[[x]]))
sddataC0 <- sapply(c(1,2,3,4), function(x) sd(dataTwitter_Clase0[[x]]))

meandataC1<-sapply(c(1,2,3,4), function(x) mean(dataTwitter_Clase1[[x]]))
sddataC1 <- sapply(c(1,2,3,4), function(x) sd(dataTwitter_Clase1[[x]]))

#factor de Fisher
FactFishNumCarc<-P1*meandataC0[1]^2+P2*meandataC1[1]^2
FactFishNumCarc<-FactFishNumCarc/(P1*sddataC0[1]^2+P2*sddataC1[1]^2)

FactFishSeg<-P1*meandataC0[2]^2+P2*meandataC1[2]^2
FactFishSeg<-FactFishSeg/(P1*sddataC0[2]^2+P2*sddataC1[2]^2)

FactFishPerfSeg<-P1*meandataC0[3]^2+P2*meandataC1[3]^2
FactFishPerfSeg<-FactFishPerfSeg/(P1*sddataC0[3]^2+P2*sddataC1[3]^2)

FactFishTwDia<-P1*meandataC0[4]^2+P2*meandataC1[4]^2
FactFishTwDia<-FactFishTwDia/(P1*sddataC0[4]^2+P2*sddataC1[4]^2)

#F1<-Perfiles Seguidos
#Step 3: Correlacion F1-Todos

F1_NumCarac<- sum(dataTwitterNorm$num_caracteres_nombre_usuario*dataTwitterNorm$perfiles_seguidos)
F1_NumCarac<-F1_NumCarac/sqrt(sum(dataTwitterNorm$num_caracteres_nombre_usuario^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

F1_Seguidores<- sum(dataTwitterNorm$seguidores*dataTwitterNorm$perfiles_seguidos)
F1_Seguidores<-F1_Seguidores/sqrt(sum(dataTwitterNorm$seguidores^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

F1_TwiDia<- sum(dataTwitterNorm$twitts_por_dia*dataTwitterNorm$perfiles_seguidos)
F1_TwiDia<-F1_TwiDia/sqrt(sum(dataTwitterNorm$twitts_por_dia^2)*sum(dataTwitterNorm$perfiles_seguidos^2))

#Step 4: Seleccionar 2da Característica
alpha1<-0.5
alpha2<-0.5

FS_nc<-alpha1*FactFishNumCarc-alpha2*abs(F1_NumCarac)
FS_seg<-alpha1*FactFishSeg-alpha2*abs(F1_Seguidores)
FS_td<-alpha1*FactFishTwDia-alpha2*abs(F1_TwiDia)

#F2-Seguidores

F1_NumCarac<- sum(dataTwitterNorm$num_caracteres_nombre_usuario*dataTwitterNorm$seguidores)
F1_NumCarac<-F1_NumCarac/sqrt(sum(dataTwitterNorm$num_caracteres_nombre_usuario^2)*sum(dataTwitterNorm$seguidores^2))

F1_TwiDia<- sum(dataTwitterNorm$twitts_por_dia*dataTwitterNorm$seguidores)
F1_TwiDia<-F1_TwiDia/sqrt(sum(dataTwitterNorm$twitts_por_dia^2)*sum(dataTwitterNorm$seguidores^2))


#Step 5: Seleccionar 3ra Característica
alpha1<-0.5
alpha2<-0.5

FS_nc<-alpha1*FactFishNumCarc-alpha2*abs(F1_NumCarac)
FS_td<-alpha1*FactFishTwDia-alpha2*abs(F1_TwiDia)
FS_nc
FS_td

#F3-Numero Caracteres


##CORRELACION PEARSON
#Mayor Entropia
dim(dataTw_Discretizada)
N<-315
tablaDT_NC<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$num_caracteres_nombre_usuario)
sumRows_tablaDT_NC<-rowSums(tablaDT_NC)
sumCols_tablaDT_NC<-colSums(tablaDT_NC)
tablaDT_NC_chi<-matrix(c((sumRows_tablaDT_NC[1]*sumCols_tablaDT_NC)/N,(sumRows_tablaDT_NC[2]*sumCols_tablaDT_NC)/N),nrow=2)
chi1<-chisq.test(tablaDT_NC_chi)
chi1<-chi1$p.value


tablaDT_NS<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$seguidores)
sumRows_tablaDT_NS<-rowSums(tablaDT_NS)
sumCols_tablaDT_NS<-colSums(tablaDT_NS)
tablaDT_NS_chi<-matrix(c((sumRows_tablaDT_NS[1]*sumCols_tablaDT_NS)/N,(sumRows_tablaDT_NS[2]*sumCols_tablaDT_NS)/N),nrow=2)
chi2<-chisq.test(tablaDT_NS_chi)
chi2<-chi2$p.value

tablaDT_PS<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$perfiles_seguidos)
sumRows_tablaDT_PS<-rowSums(tablaDT_PS)
sumCols_tablaDT_PS<-colSums(tablaDT_PS)
tablaDT_PS_chi<-matrix(c((sumRows_tablaDT_PS[1]*sumCols_tablaDT_PS)/N,(sumRows_tablaDT_PS[2]*sumCols_tablaDT_PS)/N),nrow=2)
chi3<-chisq.test(tablaDT_PS_chi)
chi3<-chi3$p.value

tablaDT_TD<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$twitts_por_dia)
sumRows_tablaDT_TD<-rowSums(tablaDT_TD)
sumCols_tablaDT_TD<-colSums(tablaDT_TD)
tablaDT_TD_chi<-matrix(c((sumRows_tablaDT_TD[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_TD[2]*sumCols_tablaDT_TD)/N),nrow=2)
chi4<-chisq.test(tablaDT_TD_chi)
chi4<-chi4$p.value

tablaDT_FP<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$foto_de_perfil)
sumRows_tablaDT_FP<-rowSums(tablaDT_FP)
sumCols_tablaDT_FP<-colSums(tablaDT_FP)
tablaDT_FP_chi<-matrix(c((sumRows_tablaDT_FP[1]*sumCols_tablaDT_FP)/N,(sumRows_tablaDT_FP[2]*sumCols_tablaDT_FP)/N),nrow=2)
chi5<-chisq.test(tablaDT_FP_chi)
chi5<-chi5$p.value

tablaDT_PP<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$perfil_privado)
sumRows_tablaDT_PP<-rowSums(tablaDT_PP)
sumCols_tablaDT_PP<-colSums(tablaDT_PP)
tablaDT_PP_chi<-matrix(c((sumRows_tablaDT_PP[1]*sumCols_tablaDT_PP)/N,(sumRows_tablaDT_PP[2]*sumCols_tablaDT_PP)/N),nrow=2)
chi6<-chisq.test(tablaDT_PP_chi)
chi6<-chi6$p.value


tablaDT_DT<-table(dataTw_Discretizada$dia_mayor_cantidad_twitts,dataTw_Discretizada$comenta_publicaciones)
sumRows_tablaDT_DT<-rowSums(tablaDT_DT)
sumCols_tablaDT_DT<-colSums(tablaDT_DT)
tablaDT_DT_chi<-matrix(c((sumRows_tablaDT_DT[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_DT[2]*sumCols_tablaDT_DT)/N),nrow=2)
chi7<-chisq.test(tablaDT_DT_chi)
chi7<-chi7$p.value
chichis<-c(chi1,chi2,chi3,chi4,chi5,chi6,chi7)
max(EntropiasTW)
FS<-(-0.5*EntropiasTW[2:8])-0.5*(abs(chichis))
FS
#F2 <-Comenta Publicaciones
max(FS)

#3ra CAracteristica

tablaDT_NC<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$num_caracteres_nombre_usuario)

sumRows_tablaDT_NC<-rowSums(tablaDT_NC)
sumCols_tablaDT_NC<-colSums(tablaDT_NC)
tablaDT_NC_chi<-matrix(c((sumRows_tablaDT_NC[1]*sumCols_tablaDT_NC)/N,(sumRows_tablaDT_NC[2]*sumCols_tablaDT_NC)/N),nrow=2)
chi21<-chisq.test(tablaDT_NC_chi)
chi21<-chi21$p.value


tablaDT_NS<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$seguidores)
sumRows_tablaDT_NS<-rowSums(tablaDT_NS)
sumCols_tablaDT_NS<-colSums(tablaDT_NS)
tablaDT_NS_chi<-matrix(c((sumRows_tablaDT_NS[1]*sumCols_tablaDT_NS)/N,(sumRows_tablaDT_NS[2]*sumCols_tablaDT_NS)/N),nrow=2)
chi22<-chisq.test(tablaDT_NS_chi)
chi22<-chi22$p.value

tablaDT_PS<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$perfiles_seguidos)
sumRows_tablaDT_PS<-rowSums(tablaDT_PS)
sumCols_tablaDT_PS<-colSums(tablaDT_PS)
tablaDT_PS_chi<-matrix(c((sumRows_tablaDT_PS[1]*sumCols_tablaDT_PS)/N,(sumRows_tablaDT_PS[2]*sumCols_tablaDT_PS)/N),nrow=2)
chi23<-chisq.test(tablaDT_PS_chi)
chi23<-chi23$p.value


tablaDT_TD<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$twitts_por_dia)
sumRows_tablaDT_TD<-rowSums(tablaDT_TD)
sumCols_tablaDT_TD<-colSums(tablaDT_TD)
tablaDT_TD_chi<-matrix(c((sumRows_tablaDT_TD[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_TD[2]*sumCols_tablaDT_TD)/N),nrow=2)
chi24<-chisq.test(tablaDT_TD_chi)
chi24<-chi24$p.value

tablaDT_FP<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$foto_de_perfil)
sumRows_tablaDT_FP<-rowSums(tablaDT_FP)
sumCols_tablaDT_FP<-colSums(tablaDT_FP)
tablaDT_FP_chi<-matrix(c((sumRows_tablaDT_FP[1]*sumCols_tablaDT_FP)/N,(sumRows_tablaDT_FP[2]*sumCols_tablaDT_FP)/N),nrow=2)
chi25<-chisq.test(tablaDT_FP_chi)
chi25<-chi25$p.value

tablaDT_PP<-table(dataTw_Discretizada$comenta_publicaciones,dataTw_Discretizada$perfil_privado)
sumRows_tablaDT_PP<-rowSums(tablaDT_PP)
sumCols_tablaDT_PP<-colSums(tablaDT_PP)
tablaDT_PP_chi<-matrix(c((sumRows_tablaDT_PP[1]*sumCols_tablaDT_PP)/N,(sumRows_tablaDT_PP[2]*sumCols_tablaDT_PP)/N),nrow=2)
chi26<-chisq.test(tablaDT_PP_chi)
chi26<-chi26$p.value

chichis2<-c(chi21,chi22,chi23,chi24,chi25,chi26)
sort(EntropiasTW)
k<-1
FS2<-(-0.5*(EntropiasTW[2:7]))-(0.5/k)*(abs(chichis2))
FS2
#F3 <-Perfiles Seguidos
max(FS2)




######KNN

dia_mayor_cantidad_twitts <- dataTw_Discretizada$dia_mayor_cantidad_twitts
comenta_publicaciones <- dataTw_Discretizada$comenta_publicaciones
perfiles_seguidos <- dataTw_Discretizada$perfiles_seguidos
clase <- dataTw_Discretizada$clase
datasetKnnTwitter <- data.frame(dia_mayor_cantidad_twitts,comenta_publicaciones,perfiles_seguidos)
summary(datasetKnnTwitter)
datasetKnnTwitter <- as.data.frame(datasetKnnTwitter)


datasetKnnTwitter$dia_mayor_cantidad_twitts <- ifelse(datasetKnnTwitter$dia_mayor_cantidad_twitts == "Publica", 1, 2)

# Codificar la columna "comenta_publicaciones"
datasetKnnTwitter$comenta_publicaciones <- ifelse(datasetKnnTwitter$comenta_publicaciones == "Sí", 1, 2)

# Codificar la columna "perfiles_seguidos"
datasetKnnTwitter$perfiles_seguidos <- ifelse(datasetKnnTwitter$perfiles_seguidos == "Pocos", 1, ifelse(datasetKnnTwitter$perfiles_seguidos == "Normal", 2, 3))

FeatNames<-colnames(datasetKnnTwitter)
mean_features <- sapply(FeatNames, function(x) mean(datasetKnnTwitter[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(datasetKnnTwitter[[x]]))
mean_features
sd_features
315/3

#Normalizar datos 
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataF - meanF)/stdF
  return(dataFN)
}

datasetKnnTwitterNorm <- lapply(FeatNames, function (x) normalizeDataL(datasetKnnTwitter[[x]], mean_features[x], sd_features[x]))
names(datasetKnnTwitterNorm) <- FeatNames
datasetKnnTwitterNorm <- as.data.frame(datasetKnnTwitterNorm)
summary(datasetKnnTwitterNorm)



dataTwitterTrain <- datasetKnnTwitterNorm[1:210,]
dataTwitterTest <- datasetKnnTwitterNorm[211:315,]

####DISTANCIA GOVER

#install.packages("cluster")
library(cluster)

# Calcular la matriz de distancias Gower
distancias <- daisy(datasetKnnTwitter, metric = "gower")

# Imprimir la matriz de distancias
length(distancias)

distanciasGover <- matrix(distancias, nrow = nrow(datasetKnnTwitter), ncol = nrow(datasetKnnTwitter))

distanciasTw <- matrix(0, nrow = nrow(datasetKnnTwitterNorm), ncol = nrow(datasetKnnTwitterNorm))

for (i in 1:(nrow(datasetKnnTwitterNorm) - 1)) {
  for (j in (i + 1):nrow(datasetKnnTwitterNorm)) {
    suma_distancias <- 0
    for (k in 1:ncol(datasetKnnTwitterNorm)) {
      # Compara el valor de la característica entre los dos objetos
      if (datasetKnnTwitterNorm[i, k] == datasetKnnTwitterNorm[j, k]) {
        distancia <- 0
      } else if (k == 1) {
        # Si la característica es "dia_mayor_cantidad_twitts", usa una distancia de 1
        distancia <- 1
      } else if (k == 2) {
        # Si la característica es "comenta_publicaciones", usa una distancia de 1
        distancia <- 1
      } else {
        # Si la característica es "perfiles_seguidos", usa una distancia de 0.5
        distancia <- 0.5
      }
      suma_distancias <- suma_distancias + distancia
    }
    # Asigna la suma de las distancias ponderadas a la matriz de distancias
    distanciasTw[i, j] <- suma_distancias
    distanciasTw[j, i] <- suma_distancias
  }
}
print(distanciasTw)
distanciasTw <- as.matrix(distanciasTw)

###CLASIFICADOR BAYESIANO

library(e1071)

datasetBayesTwitter <- data.frame(dia_mayor_cantidad_twitts,comenta_publicaciones,perfiles_seguidos,clase)
summary(datasetBayesTwitter)
datasetBayesTwitter <- as.data.frame(datasetBayesTwitter)


# Codificar las variables categóricas como factores
datasetBayesTwitter$dia_mayor_cantidad_twitts <- factor(datasetBayesTwitter$dia_mayor_cantidad_twitts)
datasetBayesTwitter$comenta_publicaciones <- factor(datasetBayesTwitter$comenta_publicaciones)
datasetBayesTwitter$perfiles_seguidos <- factor(datasetBayesTwitter$perfiles_seguidos)

# Entrenar el modelo de clasificación bayesiana
modelo_bayesiano <- naiveBayes(clase ~ ., data = datasetBayesTwitter)

# Imprimir el resumen del modelo
print(modelo_bayesiano)

# Realizar predicciones en nuevos datos
nuevos_datos <- data.frame(dia_mayor_cantidad_twitts = c("Publica", "No Publica"),
                           comenta_publicaciones = c("Sí", "No"),
                           perfiles_seguidos = c("Pocos", "Normal"))

predicciones <- predict(modelo_bayesiano, nuevos_datos)

# Imprimir las predicciones
print(predicciones)





####
# Instala los paquetes 'dbscan' y 'class' si no están instalados
if (!require(dbscan)) {
  install.packages("dbscan")
}

if (!require(class)) {
  install.packages("class")
}

library(dbscan)
library(class)
datasetKNNTwitter <- datasetBayesTwitter
summary(datasetKNNTwitter)
# Codificar las variables categóricas como factores
datasetKNNTwitter$dia_mayor_cantidad_twitts <- factor(datasetKNNTwitter$dia_mayor_cantidad_twitts)
datasetKNNTwitter$comenta_publicaciones <- factor(datasetKNNTwitter$comenta_publicaciones)
datasetKNNTwitter$perfiles_seguidos <- factor(datasetKNNTwitter$perfiles_seguidos)

# Calcular la matriz de distancias de Gower utilizando la función 'classInt' del paquete 'dbscan'
#distancias <- classInt(datasetKNNTwitter[, 1:3], method = "gower")

# Realizar la clasificación k-NN utilizando la función 'knn' del paquete 'class'
clasificacion_knn <- knn(train = distancias, test = distancias, cl = datasetKNNTwitter$clase, k = 5)

# Imprimir las predicciones
print(clasificacion_knn)

