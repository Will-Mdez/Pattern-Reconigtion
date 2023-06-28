library(dplyr) 
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"
#namefile <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_BuenaOnda.csv"

#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv";
namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//twitter_LOU.csv"
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




# Carga de la biblioteca ggplot2
library(ggplot2)

# Gráfico para "num_caracteres_nombre_usuario"
ggplot(dataTwitterFinal, aes(x = num_caracteres_nombre_usuario)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de num_caracteres_nombre_usuario")

# Gráfico para "foto_de_perfil"
ggplot(dataTwitterFinal, aes(x = foto_de_perfil)) +
  geom_bar(fill = "green") +
  labs(title = "Distribución de foto_de_perfil")

# Gráfico para "perfil_privado"
ggplot(dataTwitterFinal, aes(x = perfil_privado)) +
  geom_bar(fill = "red") +
  labs(title = "Distribución de perfil_privado")

# Gráfico para "seguidores"
ggplot(dataTwitterFinal, aes(x = seguidores)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribución de seguidores")

# Gráfico para "perfiles_seguidos"
ggplot(dataTwitterFinal, aes(x = perfiles_seguidos)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribución de perfiles_seguidos")

# Gráfico para "twitts_por_dia"
ggplot(dataTwitterFinal, aes(x = twitts_por_dia)) +
  geom_bar(fill = "pink") +
  labs(title = "Distribución de twitts_por_dia")

# Gráfico para "dia_mayor_cantidad_twitts"
ggplot(dataTwitterFinal, aes(x = dia_mayor_cantidad_twitts)) +
  geom_bar(fill = "yellow") +
  labs(title = "Distribución de dia_mayor_cantidad_twitts")

# Gráfico para "comenta_publicaciones"
ggplot(dataTwitterFinal, aes(x = comenta_publicaciones)) +
  geom_bar(fill = "brown") +
  labs(title = "Distribución de comenta_publicaciones")

# Gráfico para la clase "clase"
ggplot(dataTwitterFinal, aes(x = clase)) +
  geom_bar(fill = "gray") +
  labs(title = "Distribución de la clase")

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


#Discretizacion twitts_por_dia
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


# Gráfico para "num_caracteres_nombre_usuario"
ggplot(dataTw_Ord, aes(x = num_caracteres_nombre_usuario)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de num_caracteres_nombre_usuario")

# Gráfico para "foto_de_perfil"
ggplot(dataTw_Ord, aes(x = seguidores)) +
  geom_bar(fill = "green") +
  labs(title = "Distribución de seguidores")

# Gráfico para "perfil_privado"
ggplot(dataTw_Ord, aes(x = twitts_por_dia)) +
  geom_bar(fill = "red") +
  labs(title = "Distribución de twitts_por_dia")

# Gráfico para "seguidores"
ggplot(dataTw_Ord, aes(x = perfiles_seguidos)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribución de perfiles_seguidos")



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

# Gráfico para "num_caracteres_nombre_usuario"
ggplot(dataTw_Discretizada, aes(x = num_caracteres_nombre_usuario)) +
  geom_bar(fill = "blue") +
  labs(title = "Distribución de num_caracteres_nombre_usuario")

# Gráfico para "foto_de_perfil"
ggplot(dataTw_Discretizada, aes(x = foto_de_perfil)) +
  geom_bar(fill = "green") +
  labs(title = "Distribución de foto_de_perfil")

# Gráfico para "perfil_privado"
ggplot(dataTw_Discretizada, aes(x = perfil_privado)) +
  geom_bar(fill = "red") +
  labs(title = "Distribución de perfil_privado")

# Gráfico para "seguidores"
ggplot(dataTw_Discretizada, aes(x = seguidores)) +
  geom_bar(fill = "purple") +
  labs(title = "Distribución de seguidores")

# Gráfico para "perfiles_seguidos"
ggplot(dataTw_Discretizada, aes(x = perfiles_seguidos)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribución de perfiles_seguidos")

# Gráfico para "twitts_por_dia"
ggplot(dataTw_Discretizada, aes(x = twitts_por_dia)) +
  geom_bar(fill = "pink") +
  labs(title = "Distribución de twitts_por_dia")

# Gráfico para "dia_mayor_cantidad_twitts"
ggplot(dataTw_Discretizada, aes(x = dia_mayor_cantidad_twitts)) +
  geom_bar(fill = "yellow") +
  labs(title = "Distribución de dia_mayor_cantidad_twitts")

# Gráfico para "comenta_publicaciones"
ggplot(dataTw_Discretizada, aes(x = comenta_publicaciones)) +
  geom_bar(fill = "brown") +
  labs(title = "Distribución de comenta_publicaciones")

# Gráfico para la clase "clase"
ggplot(dataTw_Discretizada, aes(x = clase)) +
  geom_bar(fill = "gray") +
  labs(title = "Distribución de la clase")



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

orden <- c("num_caracteres_nombre_usuario","dia_mayor_cantidad_twitts","seguidores","twitts_por_dia","foto_de_perfil","perfil_privado","comenta_publicaciones","perfiles_seguidos")
EntropiasTW<-c(E4,E3,E5,E6,E0,E1,E2,E7)
sort(EntropiasTW)

plot( EntropiasTW, main = "Gráfico de dispersión con nombres de valores", xlab = "Valores", ylab = "Arreglo")

##CORRELACION PEARSON
#Mayor Entropia
dim(dataTw_Discretizada)
N<-315
tablaDT_NC<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$num_caracteres_nombre_usuario)
sumRows_tablaDT_NC<-rowSums(tablaDT_NC)
sumCols_tablaDT_NC<-colSums(tablaDT_NC)
tablaDT_NC_chi<-matrix(c((sumRows_tablaDT_NC[1]*sumCols_tablaDT_NC)/N,(sumRows_tablaDT_NC[2]*sumCols_tablaDT_NC)/N),nrow=2)
chi1<-chisq.test(tablaDT_NC_chi)
chi1<-chi1$p.value


tablaDT_NS<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$dia_mayor_cantidad_twitts)
sumRows_tablaDT_NS<-rowSums(tablaDT_NS)
sumCols_tablaDT_NS<-colSums(tablaDT_NS)
tablaDT_NS_chi<-matrix(c((sumRows_tablaDT_NS[1]*sumCols_tablaDT_NS)/N,(sumRows_tablaDT_NS[2]*sumCols_tablaDT_NS)/N),nrow=2)
chi2<-chisq.test(tablaDT_NS_chi)
chi2<-chi2$p.value

tablaDT_PS<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$seguidores)
sumRows_tablaDT_PS<-rowSums(tablaDT_PS)
sumCols_tablaDT_PS<-colSums(tablaDT_PS)
tablaDT_PS_chi<-matrix(c((sumRows_tablaDT_PS[1]*sumCols_tablaDT_PS)/N,(sumRows_tablaDT_PS[2]*sumCols_tablaDT_PS)/N),nrow=2)
chi3<-chisq.test(tablaDT_PS_chi)
chi3<-chi3$p.value

tablaDT_TD<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$twitts_por_dia)
sumRows_tablaDT_TD<-rowSums(tablaDT_TD)
sumCols_tablaDT_TD<-colSums(tablaDT_TD)
tablaDT_TD_chi<-matrix(c((sumRows_tablaDT_TD[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_TD[2]*sumCols_tablaDT_TD)/N),nrow=2)
chi4<-chisq.test(tablaDT_TD_chi)
chi4<-chi4$p.value

tablaDT_FP<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$foto_de_perfil)
sumRows_tablaDT_FP<-rowSums(tablaDT_FP)
sumCols_tablaDT_FP<-colSums(tablaDT_FP)
tablaDT_FP_chi<-matrix(c((sumRows_tablaDT_FP[1]*sumCols_tablaDT_FP)/N,(sumRows_tablaDT_FP[2]*sumCols_tablaDT_FP)/N),nrow=2)
chi5<-chisq.test(tablaDT_FP_chi)
chi5<-chi5$p.value

tablaDT_PP<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$perfil_privado)
sumRows_tablaDT_PP<-rowSums(tablaDT_PP)
sumCols_tablaDT_PP<-colSums(tablaDT_PP)
tablaDT_PP_chi<-matrix(c((sumRows_tablaDT_PP[1]*sumCols_tablaDT_PP)/N,(sumRows_tablaDT_PP[2]*sumCols_tablaDT_PP)/N),nrow=2)
chi6<-chisq.test(tablaDT_PP_chi)
chi6<-chi6$p.value


tablaDT_DT<-table(dataTw_Discretizada$perfiles_seguidos,dataTw_Discretizada$comenta_publicaciones)
sumRows_tablaDT_DT<-rowSums(tablaDT_DT)
sumCols_tablaDT_DT<-colSums(tablaDT_DT)
tablaDT_DT_chi<-matrix(c((sumRows_tablaDT_DT[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_DT[2]*sumCols_tablaDT_DT)/N),nrow=2)
chi7<-chisq.test(tablaDT_DT_chi)
chi7<-chi7$p.value
chichis<-c(chi1,chi2,chi3,chi4,chi5,chi6,chi7)
max(EntropiasTW)
FS<-(-0.5*EntropiasTW[1:7])-(0.5/2)*(abs(chichis))
FS
#F2 <-Seguidores
sort(FS)
plot( chichis, main = "Gráfico de dispersión chi cuadrada", xlab = "Valores", ylab = "Arreglo")
plot( FS, main = "Gráfico de dispersión Factor Fisher", xlab = "Valores", ylab = "Arreglo")


#3ra CAracteristica

tablaDT_NC<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$num_caracteres_nombre_usuario)

sumRows_tablaDT_NC<-rowSums(tablaDT_NC)
sumCols_tablaDT_NC<-colSums(tablaDT_NC)
tablaDT_NC_chi<-matrix(c((sumRows_tablaDT_NC[1]*sumCols_tablaDT_NC)/N,(sumRows_tablaDT_NC[2]*sumCols_tablaDT_NC)/N),nrow=2)
chi21<-chisq.test(tablaDT_NC_chi)
chi21<-chi21$p.value


tablaDT_NS<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$comenta_publicaciones)
sumRows_tablaDT_NS<-rowSums(tablaDT_NS)
sumCols_tablaDT_NS<-colSums(tablaDT_NS)
tablaDT_NS_chi<-matrix(c((sumRows_tablaDT_NS[1]*sumCols_tablaDT_NS)/N,(sumRows_tablaDT_NS[2]*sumCols_tablaDT_NS)/N),nrow=2)
chi22<-chisq.test(tablaDT_NS_chi)
chi22<-chi22$p.value

tablaDT_PS<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$dia_mayor_cantidad_twitts)
sumRows_tablaDT_PS<-rowSums(tablaDT_PS)
sumCols_tablaDT_PS<-colSums(tablaDT_PS)
tablaDT_PS_chi<-matrix(c((sumRows_tablaDT_PS[1]*sumCols_tablaDT_PS)/N,(sumRows_tablaDT_PS[2]*sumCols_tablaDT_PS)/N),nrow=2)
chi23<-chisq.test(tablaDT_PS_chi)
chi23<-chi23$p.value


tablaDT_TD<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$twitts_por_dia)
sumRows_tablaDT_TD<-rowSums(tablaDT_TD)
sumCols_tablaDT_TD<-colSums(tablaDT_TD)
tablaDT_TD_chi<-matrix(c((sumRows_tablaDT_TD[1]*sumCols_tablaDT_TD)/N,(sumRows_tablaDT_TD[2]*sumCols_tablaDT_TD)/N),nrow=2)
chi24<-chisq.test(tablaDT_TD_chi)
chi24<-chi24$p.value

tablaDT_FP<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$foto_de_perfil)
sumRows_tablaDT_FP<-rowSums(tablaDT_FP)
sumCols_tablaDT_FP<-colSums(tablaDT_FP)
tablaDT_FP_chi<-matrix(c((sumRows_tablaDT_FP[1]*sumCols_tablaDT_FP)/N,(sumRows_tablaDT_FP[2]*sumCols_tablaDT_FP)/N),nrow=2)
chi25<-chisq.test(tablaDT_FP_chi)
chi25<-chi25$p.value

tablaDT_PP<-table(dataTw_Discretizada$seguidores,dataTw_Discretizada$perfil_privado)
sumRows_tablaDT_PP<-rowSums(tablaDT_PP)
sumCols_tablaDT_PP<-colSums(tablaDT_PP)
tablaDT_PP_chi<-matrix(c((sumRows_tablaDT_PP[1]*sumCols_tablaDT_PP)/N,(sumRows_tablaDT_PP[2]*sumCols_tablaDT_PP)/N),nrow=2)
chi26<-chisq.test(tablaDT_PP_chi)
chi26<-chi26$p.value


chichis2<-c(chi21,chi22,chi23,chi24,chi25,chi26)

k<-1
FS2<-(-0.5*(EntropiasTW[c(1,7,2,4,5,6)]))-(0.5/k)*(abs(chichis2))
FS2
#F3 <-twitts_por_dia
max(FS2)
plot( chichis2, main = "Gráfico de dispersión chi cuadrada", xlab = "Valores", ylab = "Arreglo")
plot( FS2, main = "Gráfico de dispersión Factor Fisher", xlab = "Valores", ylab = "Arreglo")

summary(dataTw_Discretizada)

### TRAINING y TEST
twitts_por_dia <- factor(dataTw_Discretizada$twitts_por_dia)
seguidores <- factor(dataTw_Discretizada$seguidores)
perfiles_seguidos <- factor(dataTw_Discretizada$perfiles_seguidos)
claseTwitter <- factor(dataTw_Discretizada$clase)

datasetTwitterFULL <- data.frame(twitts_por_dia,seguidores,perfiles_seguidos,claseTwitter)
summary(datasetTwitterFULL)

library(caret)
proporcion_entrenamiento <- 0.7
set.seed(123)
indices_entrenamiento <- createDataPartition(datasetTwitterFULL$twitts_por_dia, 
                                             times = 1,
                                             p = proporcion_entrenamiento,
                                             list = FALSE)
datos_entrenamiento <- datasetTwitterFULL[indices_entrenamiento, ]
datos_prueba <- datasetTwitterFULL[-indices_entrenamiento, ]
print("Conjunto de datos de entrenamiento:")
print(datos_entrenamiento)

print("Conjunto de datos de prueba:")
print(datos_prueba)

summary(datos_prueba)
summary(datos_entrenamiento)

###########################################################KNN
library(e1071)
library(naivebayes)
library(caret)
library(FNN)
library(sp)
library(raster)
library(dismo)
library(proxy)

#install.packages("proxy")
twitts_por_dia <- factor(dataTw_Discretizada$twitts_por_dia)
seguidores <- factor(dataTw_Discretizada$seguidores)
perfiles_seguidos <- factor(dataTw_Discretizada$perfiles_seguidos)
claseTwitter <- factor(dataTw_Discretizada$clase)

datasetKnnTwitter <- data.frame(twitts_por_dia,seguidores,perfiles_seguidos,claseTwitter)
summary(datasetKnnTwitter)


X <- datasetKnnTwitter[, c("twitts_por_dia", "seguidores", "perfiles_seguidos")]
y <- datasetKnnTwitter$claseTwitter

#Crear el clasificador KNN utilizando la distancia de Gower
library(caret)
library(cluster)

k_folds <- 5
accuracy <- vector("numeric", k_folds)  # Vector para almacenar las precisiones

# Obtener los índices de los folds
indices <- createFolds(y = datasetKnnTwitter$claseTwitter, k = k_folds)

for (i in 1:k_folds) {
  # Crear los dataframes de Train y Test
  train_indices <- indices[[i]]  # Índices para el conjunto de entrenamiento
  test_indices <- indices[[i]]  # Índices para el conjunto de prueba
  
  train_df <- datasetKnnTwitter[train_indices, ]  # Dataframe de entrenamiento
  test_df <- datasetKnnTwitter[test_indices, ]  # Dataframe de prueba
  
  # Calcular la matriz de distancia de Gower para el fold de entrenamiento
  dist_matrix <- proxy::dist(train_df, method = "Gower")
  
  # Calcular la matriz de distancia de Gower para el fold de prueba
  test_dist_matrix <- proxy::dist(test_df, train_df, method = "Gower")
  
  # Realizar el clasificador KNN utilizando la distancia de Gower
  k <- 3  # Número de vecinos
  knn_result <- knn(train = dist_matrix, test = test_dist_matrix, cl = train_df$claseTwitter, k = k)
  knn_result <- factor(knn_result, levels = levels(test_df$claseTwitter))
  cat("---------------------------------------------------------------\n")
  cat("Fold", i, ":\n")
  print(confusionMatrix(knn_result,test_df$claseTwitter))
  print(confusionMatrix(knn_result,test_df$claseTwitter)$byClass)
  # Calcular la precisión para el fold actual
  accuracy[i] <- sum(knn_result == test_df$claseTwitter) / nrow(test_df)
  
}

# Calcular la precisión promedio
mean_accuracy <- mean(accuracy)

# Imprimir la precisión promedio
print(mean_accuracy)



##################################################    KNN LEAVE ONE OUT
library(FNN)
library(cluster)

df_encoded <- data.frame(model.matrix(~.-1, data = datasetKnnTwitter))

distances <- daisy(df_encoded, metric = "gower")

precisions <- c()
confusion_matrices <- list()
for (i in 1:nrow(datasetKnnTwitter)) {
  # Divide los datos en conjunto de entrenamiento y prueba para leave-one-out
  train_data <- df_encoded[-i, ]
  test_data <- df_encoded[i, ]
  
  # Divide las etiquetas de clase en conjunto de entrenamiento y prueba
  train_labels <- datasetKnnTwitter$claseTwitter[-i]
  test_label <- datasetKnnTwitter$claseTwitter[i]
  
  # Realiza la clasificación k-NN con distancia de Gower y k = 3
  predicted_label <- knn(train_data, test_data, train_labels, k = 3)
  
  # Calcula la precisión
  precision <- ifelse(predicted_label == test_label, 1, 0)
  precisions <- c(precisions, precision)
  
  # Crea la matriz de confusión
  confusion_matrix <- table(Actual = test_label, Predicted = predicted_label)
  confusion_matrices[[i]] <- confusion_matrix
}

# Calcula el promedio de la precisión y la matriz de confusión final
average_precision <- mean(precisions)
final_confusion_matrix <- Reduce(`+`, confusion_matrices)

# Imprime los resultados
print(average_precision)
print(final_confusion_matrix)


#########################################################   BAYESIANO

datasetBayesTwitter <- data.frame(twitts_por_dia,seguidores,perfiles_seguidos,claseTwitter)
dim(datasetBayesTwitter)
datasetBayesTwitter <- as.data.frame(datasetBayesTwitter)

# Codificar las variables categóricas como factores
datasetBayesTwitter$twitts_por_dia <- factor(datasetBayesTwitter$twitts_por_dia)
datasetBayesTwitter$perfiles_seguidos <- factor(datasetBayesTwitter$perfiles_seguidos)
datasetBayesTwitter$perfiles_seguidos <- factor(datasetBayesTwitter$perfiles_seguidos)
#  Definir los parámetros de K-Fold Cross Validation
k_folds <- 5

# Realizar la validación cruzada
set.seed(123)  # Establecer una semilla para reproducibilidad
folds <- sample(1:k_folds, nrow(datasetBayesTwitter), replace = TRUE)  # Asignar aleatoriamente los folds

accuracy <- vector("numeric", k_folds)  # Vector para almacenar las precisiones
matriz <- vector("list", k_folds)  # Vector para almacenar las precisiones

for (i in 1:k_folds) {
  # Separar los datos en conjunto de entrenamiento y prueba para el fold actual
  train_df <- datasetBayesTwitter[folds != i, ]
  test_df <- datasetBayesTwitter[folds == i, ]
  
  # Entrenar el clasificador bayesiano
  model <- naiveBayes(claseTwitter ~ ., data = train_df)
  
  # Realizar predicciones en el conjunto de prueba
  predictions <- predict(model, test_df)
  matriz[[i]] <- confusionMatrix(predictions, test_df$claseTwitter)
  cat("---------------------------------------------------------------\n")
  cat("Fold", i, ":\n")
  print(confusionMatrix(predictions,test_df$claseTwitter))
  print(confusionMatrix(predictions,test_df$claseTwitter)$byClass)
  # Calcular la precisión para el fold actual
  accuracy[i] <- sum(predictions == test_df$claseTwitter) / nrow(test_df)
  
  # Imprimir resultados del fold actual
  #cat("Fold", i, ":\n")
  #cat("Predicciones:", predictions, "\n")
  #cat("Clases verdaderas:", test_df$claseTwitter, "\n")
  #cat("Precisión:", accuracy[i], "\n\n")
}

# Calcular la precisión promedio
mean_accuracy <- mean(accuracy)
cat("Precisión promedio:", mean_accuracy, "\n")





#####################################    APLICANDO LEAVE ONE OUT
# Crear el modelo de clasificador bayesiano

library(e1071)
precisions <- c()
confusion_matrices <- list()
modelo <- naive_bayes(x = X, y = y)
for (i in 1:nrow(datasetBayesTwitter)) {
  # Divide los datos en conjunto de entrenamiento y prueba para leave-one-out
  train_data <- datasetBayesTwitter[-i, ]
  test_data <- datasetBayesTwitter[i, ]
  
  # Crea el modelo de clasificación Naive Bayes
  model <- naiveBayes(claseTwitter ~ ., data = train_data)
  
  # Realiza las predicciones en el objeto de prueba
  predicted_labels <- predict(model, test_data)
  
  # Obtiene la etiqueta de clase verdadera del objeto de prueba
  true_label <- test_data$claseTwitter
  
  # Calcula la precisión
  precision <- ifelse(predicted_labels == true_label, 1, 0)
  precisions <- c(precisions, precision)
  
  # Crea la matriz de confusión
  confusion_matrix <- table(Actual = true_label, Predicted = predicted_labels)
  confusion_matrices[[i]] <- confusion_matrix
}
average_precision <- mean(precisions)
final_confusion_matrix <- Reduce(`+`, confusion_matrices)
print(paste("Precisión promedio:", average_precision))
print(final_confusion_matrix)





#################################     ENTORTNO GRAFICO MODELO KNN

library(shiny)
library(cluster)
library(class)
dist_matrix <- proxy::dist(datasetKnnTwitter[, c("twitts_por_dia", "seguidores", "perfiles_seguidos")], method = "Gower")


# Definir la interfaz de usuario
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("twitts", "Número de twitts por día (  [0,2)-> Pocos, [2,3]->Normal, (3,infinito)-> Muchos  ):",
                  choices = levels(datasetKnnTwitter$twitts_por_dia)),
      selectInput("seguidores", "Número de seguidores (  [0,200)-> Pocos, [200,500]->Normal, (500,infinito)-> Muchos  ):",
                  choices = levels(datasetKnnTwitter$seguidores)),
      selectInput("perfiles", "Número de perfiles seguidos (  [0,200)-> Pocos, [200,500]->Normal, (500,infinito)-> Muchos  ):",
                  choices = levels(datasetKnnTwitter$perfiles_seguidos)),
      actionButton("predictButton", "Predecir"),
      verbatimTextOutput("predictionOutput")
    ),
    mainPanel(
      # Aquí puedes agregar elementos adicionales en el panel principal si lo deseas
    )
  )
)

# Definir la función para realizar la predicción
predictClass <- function(twitts, seguidores, perfiles) {
  # Crear el dataframe con los valores de entrada
  input_df <- data.frame(twitts_por_dia = factor(twitts,
                                                 levels = levels(datasetKnnTwitter$twitts_por_dia)),
                         seguidores = factor(seguidores,
                                             levels = levels(datasetKnnTwitter$seguidores)),
                         perfiles_seguidos = factor(perfiles,
                                                    levels = levels(datasetKnnTwitter$perfiles_seguidos)))
  
  # Calcular la matriz de distancia de Gower para el fold de prueba
  test_dist_matrix <- proxy::dist(input_df, datasetKnnTwitter[, c("twitts_por_dia", "seguidores", "perfiles_seguidos")], method = "Gower")
  
  # Realizar el clasificador KNN utilizando la distancia de Gower
  k <- 3  # Número de vecinos
  prediction <- knn(train = dist_matrix, test = test_dist_matrix, cl = datasetKnnTwitter$claseTwitter, k = k)
  
  return(prediction)
}

# Definir el servidor
server <- function(input, output){
  observeEvent(input$predictButton, {
    twitts <- input$twitts
    seguidores <- input$seguidores
    perfiles <- input$perfiles
    
    prediction <- predictClass(twitts, seguidores, perfiles)
    
    output$predictionOutput <- renderText({
      paste("Predicción:", prediction)
    })
  })
}

shinyApp(ui = ui, server = server)

########################################    PRUEBA CON TEST DATASET


test_dist_matrix <- proxy::dist(datos_prueba[-4], datasetKnnTwitter[, c("twitts_por_dia", "seguidores", "perfiles_seguidos")], method = "Gower")
k <- 3  # Número de vecinos
prediction <- knn(train = dist_matrix, test = test_dist_matrix, cl = datasetKnnTwitter$claseTwitter, k = k)
prediction
cm<-confusionMatrix(prediction,datos_prueba$claseTwitter)
print(confusionMatrix(prediction,datos_prueba$claseTwitter))
print(confusionMatrix(prediction,datos_prueba$claseTwitter)$byClass)

plot(cm$table, col = cm$byClass, 
     main = paste("Matriz de Confusión\nExactitud:", round(cm$overall['Accuracy'], 3)))



#########ENTORTNO GRAFICO MODELO BAYESIANO


library(shiny)
library(e1071)  # Paquete para naiveBayes

# Define la interfaz de usuario (UI)
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("twitts", "Número de twitts por día:",
                  choices = levels(twitts_por_dia)),
      selectInput("seguidores", "Número de seguidores:",
                  choices = levels(seguidores)),
      selectInput("perfiles", "Número de perfiles seguidos:",
                  choices = levels(perfiles_seguidos)),
      actionButton("predictButton", "Predecir"),
      verbatimTextOutput("predictionOutput")
    ),
    mainPanel(
      # Aquí puedes agregar elementos adicionales en el panel principal si lo deseas
    )
  )
)

# Define el servidor (server)
server <- function(input, output) {
  

  
  # Entrenar el modelo de clasificación bayesiano
  model <- naiveBayes(claseTwitter ~ ., data = datasetBayesTwitter)
  
  # Función para realizar predicciones y devolver el resultado
  predictResult <- function(twitts, seguidores, perfiles) {
    # Crear un nuevo conjunto de datos con los valores introducidos por el usuario
    new_data <- data.frame(
      twitts_por_dia = twitts,
      seguidores = seguidores,
      perfiles_seguidos = perfiles
    )
    
    # Realizar la predicción utilizando el modelo entrenado
    prediction <- predict(model, newdata = new_data)
    
    # Devolver la predicción
    return(prediction)
  }
  
  # Realizar la predicción cuando se hace clic en el botón
  observeEvent(input$predictButton, {
    twitts <- input$twitts
    seguidores <- input$seguidores
    perfiles <- input$perfiles
    
    prediction <- predictResult(twitts, seguidores, perfiles)
    
    output$predictionOutput <- renderText({
      paste("Predicción:", prediction)
    })
  })
}

# Ejecutar la aplicación shiny
shinyApp(ui, server)

