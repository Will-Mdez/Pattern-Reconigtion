#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
#namefile <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"

#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv"
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv";
namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv"
#namefile2 <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_ROCKET2.csv"
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
dataFacebook2$`Páginas que siguen` <- as.numeric(dataFacebook2$`Páginas que siguen`)
dataFacebook2[27,]
summary(dataFacebook2)
#UNIMOS DATASETS
dataFacebookFinal <- rbind(dataFacebook,dataFacebook2)

#Descripción de los datos

summary(dataFacebookFinal)
dataFacebookFinal <- as.data.frame(dataFacebookFinal)
dataFacebookFinal$`Día de la semana con más actividad`[dataFacebookFinal$`Día de la semana con más actividad`=="NaN"]<-"Publica"
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
dataFacebookFinal[375,3]
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

dataFBSE<-filter(dataFB_cuanti,dataFB_cuanti$`Nro Caracteres del nombre`<(mean_features[1]+3*sd_features[1]))
dataFBSE<-filter(dataFBSE,dataFBSE$Amigos<(mean_features[2]+3*sd_features[2]))
dataFBSE<-filter(dataFBSE,dataFBSE$`Páginas que siguen`<(mean_features[3]+3*sd_features[3]))
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
valores <- c('Publica','No Publica')
dataCopy$`Día de la semana con más actividad` <- match(dataCopy$`Día de la semana con más actividad`, valores)

summary(dataCopy)
dataFaceDiscretizado<-cbind(dataFB_ordenado[,1:3],dataFacebookFinal[,c(2,5)],dataFB_ordenado[4])
dataFaceDiscretizado$clase <- factor(dataFaceDiscretizado$clase)
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
N<-377
summary(dataFaceDiscretizado)

P_00<-table(dataFaceDiscretizado$`Foto de Perfil`[dataFaceDiscretizado$clase==1])
P_01<-table(dataFaceDiscretizado$`Foto de Perfil`[dataFaceDiscretizado$clase==2])

P_10<-table(dataFaceDiscretizado$`Día de la semana con más actividad`[dataFaceDiscretizado$clase==1])
P_11<-table(dataFaceDiscretizado$`Día de la semana con más actividad`[dataFaceDiscretizado$clase==2])

P_20<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`[dataFaceDiscretizado$clase==1])
P_21<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`[dataFaceDiscretizado$clase==2])

P_30<-table(dataFaceDiscretizado$Amigos[dataFaceDiscretizado$clase==1])
P_31<-table(dataFaceDiscretizado$Amigos[dataFaceDiscretizado$clase==2])

P_40<-table(dataFaceDiscretizado$`Páginas que siguen`[dataFaceDiscretizado$clase==1])
P_41<-table(dataFaceDiscretizado$`Páginas que siguen`[dataFaceDiscretizado$clase==2])

#Entropia Foto Perfil
EV_00<--(P_00[1]/N*log2(P_00[1]/N)+P_01[1]/N*log2(P_01[1]/N))
EV_01<--(P_00[2]/N*log2(P_00[2]/N)+P_01[2]/N*log2(P_01[2]/N))
E0<-((P_00[1]+P_01[1])*EV_00+(P_00[2]+P_01[2])*EV_01)/N

EV_10<--(P_10[1]/N*log2(P_10[1]/N)+P_11[1]/N*log2(P_11[1]/N))
EV_11<--(P_10[2]/N*log2(P_10[2]/N)+P_11[2]/N*log2(P_11[2]/N))

E1<-((P_10[1]+P_11[1])*EV_10+(P_10[2]+P_11[2])*EV_11)/N

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

EntropiasFB<-c(E2,E3,E4,E0,E1)
EntropiasFB
sort(EntropiasFB)

#La menor entropía es la de E1 que corresponde a Nro Caracteres del nombre`


summary(dataFaceDiscretizado)
dim(dataFaceDiscretizado)

N<-377
tablaNC_A<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`,dataFaceDiscretizado$Amigos)
sumRows_tablaNC_A<-rowSums(tablaNC_A)
sumCols_tablaNC_A<-colSums(tablaNC_A)
tablaNC_A_chi<-matrix(c((sumRows_tablaNC_A[1]*sumCols_tablaNC_A)/N,(sumRows_tablaNC_A[2]*sumCols_tablaNC_A)/N,(sumRows_tablaNC_A[3]*sumCols_tablaNC_A)/N),nrow=3)
chi1<-chisq.test(tablaNC_A_chi)
chi1<-chi1$p.value


tablaNC_PS<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`,dataFaceDiscretizado$`Páginas que siguen`)
sumRows_tablaNC_PS<-rowSums(tablaNC_PS)
sumCols_tablaNC_PS<-colSums(tablaNC_PS)
tablaNC_PS_chi<-matrix(c((sumRows_tablaNC_PS[1]*sumCols_tablaNC_PS)/N,(sumRows_tablaNC_PS[2]*sumCols_tablaNC_PS)/N,(sumRows_tablaNC_PS[3]*sumCols_tablaNC_PS)/N),nrow=3)
chi2<-chisq.test(tablaNC_PS_chi)
chi2<-chi2$p.value

#mosaicplot(tablaNC_PS_chi, color=TRUE, main="Plot de mosaico")

tablaNC_FP<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`,dataFaceDiscretizado$`Foto de Perfil`)
sumRows_tablaNC_FP<-rowSums(tablaNC_FP)
sumCols_tablaNC_FP<-colSums(tablaNC_FP)
tablaNC_FP_chi<-matrix(c((sumRows_tablaNC_FP[1]*sumCols_tablaNC_FP)/N,(sumRows_tablaNC_FP[2]*sumCols_tablaNC_FP)/N),nrow=2)
chi3<-chisq.test(tablaNC_FP_chi)
chi3<-chi3$p.value

tablaNC_DA<-table(dataFaceDiscretizado$`Nro Caracteres del nombre`,dataFaceDiscretizado$`Día de la semana con más actividad`)
sumRows_tablaNC_DA<-rowSums(tablaNC_DA)
sumCols_tablaNC_DA<-colSums(tablaNC_DA)
tablaNC_DA_chi<-matrix(c((sumRows_tablaNC_DA[1]*sumCols_tablaNC_DA)/N,(sumRows_tablaNC_DA[2]*sumCols_tablaNC_DA)/N),nrow=2)
chi4<-chisq.test(tablaNC_DA_chi)
chi4<-chi4$p.value

chichisFB<-c(chi1,chi2,chi3,chi4)
max(EntropiasFB)
FSFB<-(-0.5*EntropiasFB[2:5])-0.5*(abs(chichisFB))
FSFB
#F2 <-Num Caracteres
max(FSFB)


#3ra CAracteristica

tablaA_PS<-table(dataFaceDiscretizado$Amigos,dataFaceDiscretizado$`Páginas que siguen`)
sumRows_tablaA_PS<-rowSums(tablaA_PS)
sumCols_tablaA_PS<-colSums(tablaA_PS)
tablaA_PS_chi<-matrix(c((sumRows_tablaA_PS[1]*sumCols_tablaA_PS)/N,(sumRows_tablaA_PS[2]*sumCols_tablaA_PS)/N,(sumRows_tablaA_PS[3]*sumCols_tablaA_PS)/N),nrow=3)
chi21<-chisq.test(tablaA_PS_chi)
chi21<-chi21$p.value


tablaA_FP<-table(dataFaceDiscretizado$Amigos,dataFaceDiscretizado$`Foto de Perfil`)
sumRows_tablaA_FP<-rowSums(tablaA_FP)
sumCols_tablaA_FP<-colSums(tablaA_FP)
tablaA_NS_chi<-matrix(c((sumRows_tablaA_FP[1]*sumCols_tablaA_FP)/N,(sumRows_tablaA_FP[2]*sumCols_tablaA_FP)/N,(sumRows_tablaA_FP[3]*sumCols_tablaA_FP)/N),nrow=3)
chi22<-chisq.test(tablaA_NS_chi)
chi22<-chi22$p.value

tablaA_DA<-table(dataFaceDiscretizado$Amigos,dataFaceDiscretizado$`Día de la semana con más actividad`)
sumRows_tablaA_DA<-rowSums(tablaA_DA)
sumCols_tablaA_DA<-colSums(tablaA_DA)
tablaA_PS_chi<-matrix(c((sumRows_tablaA_DA[1]*sumCols_tablaA_DA)/N,(sumRows_tablaA_DA[2]*sumCols_tablaA_DA)/N,(sumRows_tablaA_DA[2]*sumCols_tablaA_DA)/N),nrow=3)
chi23<-chisq.test(tablaA_PS_chi)
chi23<-chi23$p.value


chichisFB2<-c(chi21,chi22,chi23)
sort(EntropiasFB)
k<-1
FS2<-(-0.5*(EntropiasFB[3:5]))-(0.5/k)*(abs(chichisFB2))
FS2
#F3 <-Foto PErfil
max(FS2)




######KNN

Nro_Caracteres <- dataFaceDiscretizado$`Nro Caracteres del nombre`
Amigos <- dataFaceDiscretizado$Amigos
Foto_Perfil <- dataFaceDiscretizado$`Foto de Perfil`
claseFB <- dataFaceDiscretizado$clase
datasetKnnFB <- data.frame(Nro_Caracteres,Amigos,Foto_Perfil)
dim(datasetKnnFB)
datasetKnnFB <- as.data.frame(datasetKnnFB)
#install.packages("cluster")
library(cluster)

# Calcular la matriz de distancias Gower
distancias <- daisy(datasetKnnFB, metric = "gower")

# Imprimir la matriz de distancias
length(distancias)
distanciasGover <- matrix(distancias, nrow = nrow(datasetKnnFB), ncol = nrow(datasetKnnFB))



datasetKnnFB$Nro_Caracteres <- ifelse(datasetKnnFB$Nro_Caracteres == "Muchas", 1, ifelse(datasetKnnFB$Nro_Caracteres == "Normal", 2, 3))

# Codificar la columna "Amigos"
datasetKnnFB$Amigos <- ifelse(datasetKnnFB$Amigos == "Muchas", 1, ifelse(datasetKnnFB$Amigos == "Normal", 2, 3))

# Codificar la columna "Foto_Perfil"
datasetKnnFB$Foto_Perfil <- ifelse(datasetKnnFB$Foto_Perfil == "No", 1, 2)



FeatNames<-colnames(datasetKnnFB)
mean_features <- sapply(FeatNames, function(x) mean(datasetKnnFB[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(datasetKnnFB[[x]]))
mean_features
sd_features

163/3

#Normalizar datos 
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN <- dataF
  dataFN <- (dataF - meanF)/stdF
  return(dataFN)
}

datasetKnnFBNorm <- lapply(FeatNames, function (x) normalizeDataL(datasetKnnFB[[x]], mean_features[x], sd_features[x]))
names(datasetKnnFBNorm) <- FeatNames
datasetKnnFBNorm <- as.data.frame(datasetKnnFBNorm)
summary(datasetKnnFBNorm)



datasetKnnFBTrain <- datasetKnnFBNorm[1:109,]
datasetKnnFBTest <- datasetKnnFBNorm[110:163,]

####DISTANCIA GOVER

# Calcular la matriz de distancias Gower manualmente
distanciasFB <- matrix(0, nrow = nrow(datasetKnnFBNorm), ncol = nrow(datasetKnnFBNorm))

for (i in 1:(nrow(datasetKnnFBNorm) - 1)) {
  for (j in (i + 1):nrow(datasetKnnFBNorm)) {
    suma_distancias <- 0
    for (k in 1:ncol(datasetKnnFBNorm)) {
      # Compara el valor de la característica entre los dos objetos
      if (datasetKnnFBNorm[i, k] == datasetKnnFBNorm[j, k]) {
        distancia <- 0
      } else if (k == 1 || k == 2) {
        # Si la característica es "Nro_Caracteres" o "Amigos", usa una distancia de 0.5
        distancia <- 0.5
      } else {
        # Si la característica es "Foto_Perfil", usa una distancia de 1
        distancia <- 1
      }
      suma_distancias <- suma_distancias + distancia
    }
    # Asigna la suma de las distancias ponderadas a la matriz de distancias
    distanciasFB[i, j] <- suma_distancias
    distanciasFB[j, i] <- suma_distancias
  }
}
print(distanciasFB)



###CLASIFICADOR BAYESIANO
library(e1071)

datasetBayesFb <- data.frame(Nro_Caracteres,Amigos,Foto_Perfil,claseFB)
summary(datasetBayesFb)
datasetBayesFb <- as.data.frame(datasetBayesFb)


# Codificar las variables categóricas como factores
datasetBayesFb$Nro_Caracteres <- factor(datasetBayesFb$Nro_Caracteres)
datasetBayesFb$Amigos <- factor(datasetBayesFb$Amigos)
datasetBayesFb$Foto_Perfil <- factor(datasetBayesFb$Foto_Perfil)

# Entrenar el modelo de clasificación bayesiana
modelo_bayesiano_Fb <- naiveBayes(claseFB ~ ., data = datasetBayesFb)

# Imprimir el resumen del modelo
print(modelo_bayesiano_Fb)

# Realizar predicciones en nuevos datos
nuevos_datos_Fb <- data.frame(Horas.Semana.Divertirse.con.sus.amigos = c("Muchas", "Muchas"),
                                    Materias.Aprobadas.Primer.Semestre = c("Regular", "Irregular"),
                                    Edad = c("Group2", "Group1"))

prediccionesFb <- predict(modelo_bayesiano_Fb, nuevos_datos_Fb)

# Imprimir las predicciones
print(prediccionesFb)





