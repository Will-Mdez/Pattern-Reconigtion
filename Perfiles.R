#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"


dataPerfiles <- read.table(namefile, header = TRUE, sep =',')

#Descripción de los datos
##Nombre a las columnas de datos


caracteristicas<-c(1,2,6,7,8,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)

dataPerfiles<-dataPerfiles[caracteristicas]
#dimensiones
dim(dataPerfiles)

summary(dataPerfiles)
names(dataPerfiles)

#Columnas con datos Cualitativos
cualitativos <- c(2,3,4,5,7,8,9,10,14,15,16,19,20,21,22,23,24,25,26,27,28,29)
#Columnas con datos Cuantitativos
cuantitativos<- c(1,6,11,12,13,17,18,23)
namesP <- colnames(dataPerfiles)

#Separamos los datos categóricos
dataPerfiles2 <- lapply(cualitativos,function(x) factor(dataPerfiles[,x]))

names(dataPerfiles2) <- namesP[cualitativos]
dataPerfiles2 <- as.data.frame(dataPerfiles2)

summary(dataPerfiles2)
dim(dataPerfiles2)

#Datos cuantitativos
dataPerfiles_cuanti<-dataPerfiles[cuantitativos]
summary(dataPerfiles_cuanti)

#Imputación de datos 
FeatNames<-namesP[cuantitativos]
mean_features <- sapply(FeatNames, function(x) mean(dataPerfiles_cuanti[[x]]))
mean_features
sd_features <- sapply(FeatNames, function(x) sd(dataPerfiles_cuanti[[x]]))
sd_features
#Identificación de valores extremos
dataPerfiles_SE<-filter(dataPerfiles_cuanti,dataPerfiles_cuanti$Edad<(mean_features[1]+3*sd_features[1]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Preparatoria<(mean_features[2]+3*sd_features[2]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Cursadas.Primer.Semestre<(mean_features[3]+3*sd_features[3]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Primer.Semestre<(mean_features[4]+3*sd_features[4]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Aprobadas.Primer.Semestre<(mean_features[5]+3*sd_features[5]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Examenes<(mean_features[6]+3*sd_features[6]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Actividades.Escolares<(mean_features[7]+3*sd_features[7]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Semana.Divertirse.con.sus.amigos<(mean_features[8]+3*sd_features[8]))
summary(dataPerfiles_SE)
#Normalización de datos
mean_features <- sapply(FeatNames, function(x) mean(dataPerfiles_cuanti[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(dataPerfiles_cuanti[[x]]))

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}

dataPerfiles_Norm <- lapply(FeatNames, function (x) normalizeaDataL(dataPerfiles_cuanti[[x]], mean_features[x], sd_features[x]))

names(dataPerfiles_Norm)<- FeatNames  
dataPerfiles_Norm <- as.data.frame(dataPerfiles_Norm)
summary(dataPerfiles_Norm)

#Discretizamos para clase con promedio
dataP_Promedio <- dataPerfiles_cuanti$Promedio.Primer.Semestre
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre<6] <- "Malo"
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre>=6 & dataPerfiles_cuanti$Promedio.Primer.Semestre<8]<- "Regular"
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre>=8]<- "Bueno"
dataP_Promedio

#Aquí pasamos al df original
dataPerfiles_cuanti['Clase'] <- dataP_Promedio

#Ordenamos el df
dataP_Ord <- dataPerfiles_cuanti
dataP_Ord <- dataPerfiles_cuanti[order(dataPerfiles_cuanti$Edad, decreasing = FALSE),]

N <- dim(dataP_Ord)[1]
N
division <- N/3
print (division) 

#Discretizacion Edad
dataEdad <- dataP_Ord$Edad
dataEdad[1:54] <- "Group1"
dataEdad[55:108]<- "Group2"
dataEdad[109:163]<- "Group3"

#Discretizacion Promedio Prepa
dataP_PromedioPrepa <- dataP_Ord$Promedio.Preparatoria
dataP_PromedioPrepa[dataP_Ord$Promedio.Preparatoria<6] <- "Malo"
dataP_PromedioPrepa[dataP_Ord$Promedio.Preparatoria>=6 & dataP_Ord$Promedio.Preparatoria<8]<- "Regular"
dataP_PromedioPrepa[dataP_Ord$Promedio.Preparatoria>=8]<- "Bueno"

#Discretizacion Familia
dataFamilia <- dataP_Ord$Integrantes.Familia.num
dataFamilia[dataP_Ord$Integrantes.Familia.num<5] <- "Pequeña"
dataFamilia[dataP_Ord$Integrantes.Familia.num>=5]<- "Grande"

#Discretizacion Materias Cursadas
dataMateriasCursa <- dataP_Ord$Materias.Cursadas.Primer.Semestre
dataMateriasCursa[dataP_Ord$Materias.Cursadas.Primer.Semestre==5 | dataP_Ord$Materias.Cursadas.Primer.Semestre==6]<- "Regular"
dataMateriasCursa[dataP_Ord$Materias.Cursadas.Primer.Semestre<5 | dataP_Ord$Materias.Cursadas.Primer.Semestre>6]<- "Irregular"

#Discretizacion Materias Aprobadas
dataMateriasAprob <- dataP_Ord$Materias.Aprobadas.Primer.Semestre
dataMateriasAprob[dataP_Ord$Materias.Aprobadas.Primer.Semestre>4]<- "Regular"
dataMateriasAprob[dataP_Ord$Materias.Aprobadas.Primer.Semestre<=4]<- "Irregular"

#Discretizacion Horas Examenes
dataHrsEstudia <- dataP_Ord$Horas.Promedio.Estudio.Examenes
dataHrsEstudia[dataP_Ord$Horas.Promedio.Estudio.Examenes<1]<- "Ninguna"
dataHrsEstudia[dataP_Ord$Horas.Promedio.Estudio.Examenes>=1 & dataP_Ord$Horas.Promedio.Estudio.Examenes<3]<- "Normal"
dataHrsEstudia[dataP_Ord$Horas.Promedio.Estudio.Examenes>=3]<- "Muchas"

#Discretizacion Horas Actividades
dataHrsAct <- dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares
dataHrsAct[dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares<1]<- "Ninguna"
dataHrsAct[dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares>=1 & dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares<3]<- "Normal"
dataHrsAct[dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares>=3]<- "Muchas"

#Discretizacion Diversión
dataHrsDiversion <- dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos
dataHrsDiversion[dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos<2]<- "Pocas"
dataHrsDiversion[dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos>=2 & dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos<5]<- "Normal"
dataHrsDiversion[dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos>=5]<- "Muchas"



#Asignamos valores

dataP_Ord$Edad <- dataEdad
#dataP_Ord$Promedio.Primer.Semestre <- dataP_Promedio
#dataP_Ord$Integrantes.Familia.num <- dataFamilia
dataP_Ord$Promedio.Preparatoria <- dataP_PromedioPrepa
dataP_Ord$Materias.Aprobadas.Primer.Semestre <- dataMateriasAprob
dataP_Ord$Horas.Promedio.Estudio.Examenes <- dataHrsEstudia
dataP_Ord$Materias.Cursadas.Primer.Semestre <- dataMateriasCursa
dataP_Ord$Horas.Promedio.Estudio.Actividades.Escolares <- dataHrsAct
dataP_Ord$Horas.Semana.Divertirse.con.sus.amigos <- dataHrsDiversion
summary(dataP_Ord)


##Discretizacion Datos Cualitativos
dataCopy <- dataPerfiles2
summary(dataCopy)

valores <- c('No', 'Sí')
dataCopy$Vive.con.Familia <- match(dataCopy$Vive.con.Familia, valores)
dataCopy$Enfermedad.CrÃ.nica <- match(dataCopy$Enfermedad.CrÃ.nica, valores)
dataCopy$Materias.Relacionadas <- match(dataCopy$Materias.Relacionadas, valores)
dataCopy$Carrera.Elegida.Ajusta.A.Gustos.Habilidades <- match(dataCopy$Carrera.Elegida.Ajusta.A.Gustos.Habilidades, valores)
dataCopy$Buena.RelaciÃ.n.Familiar <- match(dataCopy$Buena.RelaciÃ.n.Familiar, valores)
dataCopy$Buena.RelaciÃ.n.Amigos.CompaÃ.eros <- match(dataCopy$Buena.RelaciÃ.n.Amigos.CompaÃ.eros, valores)
dataCopy$Materias.Reprobadas.De.ComputaciÃ.n <- match(dataCopy$Materias.Reprobadas.De.ComputaciÃ.n, valores)
dataCopy$Materias.Reprobadas.De.MatemÃ.ticas <- match(dataCopy$Materias.Reprobadas.De.MatemÃ.ticas, valores)
dataCopy$Materias.Reprobadas.De.Ciensias.Sociales <- match(dataCopy$Materias.Reprobadas.De.Ciensias.Sociales, valores)
dataCopy$Cursos.Extra.Primer.Semestre <- match(dataCopy$Cursos.Extra.Primer.Semestre, valores)
dataCopy$Noviazgo <- match(dataCopy$Noviazgo, valores)
dataCopy$Actividad.Cultural.o.Deportiva <- match(dataCopy$Actividad.Cultural.o.Deportiva, valores)
dataCopy$Tiempo.con.sus.amigos.compaÃ.eros <- match(dataCopy$Tiempo.con.sus.amigos.compaÃ.eros, valores)
dataCopy$Gustan.Videojuegos <- match(dataCopy$Gustan.Videojuegos, valores)
valores <- c('Masculino','Femenino','Prefiero no decirlo')
dataCopy$Genero <- match(dataCopy$Genero, valores)
#valores <- c('Cañada', 'Costa', 'Istmo', 'Mixteca', 'Región Cuenca del Papaloapan','Sierra Sur','Sierra Norte', 'Valles Centrales', 'Otro estado')
#dataCopy$Region <- match(dataCopy$Region, valores)
#valores <- c('Ambos','Mamá','Papá')
#dataCopy$Proveedor.Familia <- match(dataCopy$Proveedor.Familia, valores)
valores <- c('Otra persona','Madre','Padre','Pariente')
dataCopy$Tutor <- match(dataCopy$Tutor, valores)
#valores <- c('Ingeniería en Computación', 'Ingeniería en Electrónica',
#                              'Ingeniería en Mecatrónica', 'Ingeniería en Diseño',
 #                              'Ingeniería en Alimentos', 'Ingeniería Industrial',
  #                             'Ingeniería en Física Aplicada', 'Ingeniería en Mecánica Automotriz',
   #                            'Ingeniería Civil', 'Licenciatura en Ciencias Empresariales',
    #                           'Licenciatura en Matemáticas Aplicadas')
#dataCopy$Carrera <- match(dataCopy$Carrera, valores)
#valores <- c('COBAO','CETIS', 'CBTIS', 'Preparatoria-UABJO', 'CONALEP',
     #                          'Preparatoria/Bachillerato particular', 'Otro')
#dataCopy$Preparatoria <- match(dataCopy$Preparatoria, valores)
valores <- c('nunca', 'casi-nunca', 'a veces', 'regularmente')
dataCopy$ConsumÃ.a.Alcohol.Entre.semana <- match(dataCopy$ConsumÃ.a.Alcohol.Entre.semana, valores)
dataCopy$ConsumÃ.a.Alcohol.Fin.semana <- match(dataCopy$ConsumÃ.a.Alcohol.Fin.semana, valores)
valores <- c('0','<1','1-2', '>2')
dataCopy$Horas.DÃ.a.Dedicada.Redes.Sociales <- match(dataCopy$Horas.DÃ.a.Dedicada.Redes.Sociales, valores)
valores <- c('0','1-3', '>3')
dataCopy$Horas.Semana.Dedicada.Videojuegos <- match(dataCopy$Horas.Semana.Dedicada.Videojuegos, valores)
dataCopy$Horas.Semana.Dedicada.Actividades.Entretenimiento <- match(dataCopy$Horas.Semana.Dedicada.Actividades.Entretenimiento, valores)
summary(dataCopy)


#Frecuencias
#histograma de frecuencias

freqClass <- table(dataCopy$Tutor)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("Carrera","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=Carrera, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p


#histograma de frecuencias

freqClass <- table(dataP_Ord$Promedio.Preparatoria)
freqClass
freqClass_df <- as.data.frame(freqClass)
freqClass_df
names(freqClass_df) <- c("Promedio.Preparatoria","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=Promedio.Preparatoria, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p

summary(dataPerfiles)
cualitativos <- c(2,3,4,5,7,8,9,10,14,15,16,19,20,21,22,24,25,26,27,28,29)
dataPerfiles_Final<-dataPerfiles
summary(dataPerfiles_Final)
dataPerfiles_Final <- lapply(cualitativos,function(x) factor(dataPerfiles_Final[,x]))
summary(dataPerfiles_Final)
#Rankeo
#Entropia
N<-155

P_00<-table(RPerfilesTable$Edad[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_01<-table(RPerfilesTable$Edad[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_02<-table(RPerfilesTable$Edad[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_10<-table(RPerfilesTable$Promedio.Preparatoria[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_11<-table(RPerfilesTable$Promedio.Preparatoria[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_12<-table(RPerfilesTable$Promedio.Preparatoria[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_20<-table(RPerfilesTable$Materias.Cursadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_21<-table(RPerfilesTable$Materias.Cursadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_22<-table(RPerfilesTable$Materias.Cursadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_30<-table(RPerfilesTable$Materias.Aprobadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_31<-table(RPerfilesTable$Materias.Aprobadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_32<-table(RPerfilesTable$Materias.Aprobadas.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_40<-table(RPerfilesTable$Horas.Promedio.Estudio.Examenes[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_41<-table(RPerfilesTable$Horas.Promedio.Estudio.Examenes[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_42<-table(RPerfilesTable$Horas.Promedio.Estudio.Examenes[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_50<-table(RPerfilesTable$Horas.Promedio.Estudio.Actividades.Escolares[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_51<-table(RPerfilesTable$Horas.Promedio.Estudio.Actividades.Escolares[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_52<-table(RPerfilesTable$Horas.Promedio.Estudio.Actividades.Escolares[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_60<-table(RPerfilesTable$Horas.Semana.Divertirse.con.sus.amigos[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_61<-table(RPerfilesTable$Horas.Semana.Divertirse.con.sus.amigos[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_62<-table(RPerfilesTable$Horas.Semana.Divertirse.con.sus.amigos[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_70<-table(RPerfilesTable$Genero[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_71<-table(RPerfilesTable$Genero[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_72<-table(RPerfilesTable$Genero[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_80<-table(RPerfilesTable$Tutor[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_81<-table(RPerfilesTable$Tutor[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_82<-table(RPerfilesTable$Tutor[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_90<-table(RPerfilesTable$Vive.con.Familia[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_91<-table(RPerfilesTable$Vive.con.Familia[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_92<-table(RPerfilesTable$Vive.con.Familia[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_100<-table(RPerfilesTable$Enfermedad.CrÃ.nica[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_101<-table(RPerfilesTable$Enfermedad.CrÃ.nica[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_102<-table(RPerfilesTable$Enfermedad.CrÃ.nica[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_110<-table(RPerfilesTable$Materias.Relacionadas[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_111<-table(RPerfilesTable$Materias.Relacionadas[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_112<-table(RPerfilesTable$Materias.Relacionadas[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_120<-table(RPerfilesTable$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_121<-table(RPerfilesTable$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_122<-table(RPerfilesTable$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_130<-table(RPerfilesTable$Buena.RelaciÃ.n.Familiar[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_131<-table(RPerfilesTable$Buena.RelaciÃ.n.Familiar[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_132<-table(RPerfilesTable$Buena.RelaciÃ.n.Familiar[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_140<-table(RPerfilesTable$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_141<-table(RPerfilesTable$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_142<-table(RPerfilesTable$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_150<-table(RPerfilesTable$Materias.Reprobadas.De.ComputaciÃ.n[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_151<-table(RPerfilesTable$Materias.Reprobadas.De.ComputaciÃ.n[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_152<-table(RPerfilesTable$Materias.Reprobadas.De.ComputaciÃ.n[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_160<-table(RPerfilesTable$Materias.Reprobadas.De.MatemÃ.ticas[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_161<-table(RPerfilesTable$Materias.Reprobadas.De.MatemÃ.ticas[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_162<-table(RPerfilesTable$Materias.Reprobadas.De.MatemÃ.ticas[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_170<-table(RPerfilesTable$Materias.Reprobadas.De.Ciensias.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_171<-table(RPerfilesTable$Materias.Reprobadas.De.Ciensias.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_172<-table(RPerfilesTable$Materias.Reprobadas.De.Ciensias.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_180<-table(RPerfilesTable$Cursos.Extra.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_181<-table(RPerfilesTable$Cursos.Extra.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_182<-table(RPerfilesTable$Cursos.Extra.Primer.Semestre[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_190<-table(RPerfilesTable$Noviazgo[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_191<-table(RPerfilesTable$Noviazgo[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_192<-table(RPerfilesTable$Noviazgo[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_200<-table(RPerfilesTable$Actividad.Cultural.o.Deportiva[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_201<-table(RPerfilesTable$Actividad.Cultural.o.Deportiva[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_202<-table(RPerfilesTable$Actividad.Cultural.o.Deportiva[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_210<-table(RPerfilesTable$Tiempo.con.sus.amigos.compaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_211<-table(RPerfilesTable$Tiempo.con.sus.amigos.compaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_212<-table(RPerfilesTable$Tiempo.con.sus.amigos.compaÃ.eros[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_220<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Entre.semana[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_221<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Entre.semana[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_222<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Entre.semana[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_230<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Fin.semana[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_231<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Fin.semana[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_232<-table(RPerfilesTable$ConsumÃ.a.Alcohol.Fin.semana[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_240<-table(RPerfilesTable$Horas.DÃ.a.Dedicada.Redes.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_241<-table(RPerfilesTable$Horas.DÃ.a.Dedicada.Redes.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_242<-table(RPerfilesTable$Horas.DÃ.a.Dedicada.Redes.Sociales[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_250<-table(RPerfilesTable$Gustan.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_251<-table(RPerfilesTable$Gustan.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_252<-table(RPerfilesTable$Gustan.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_260<-table(RPerfilesTable$Horas.Semana.Dedicada.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_261<-table(RPerfilesTable$Horas.Semana.Dedicada.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_262<-table(RPerfilesTable$Horas.Semana.Dedicada.Videojuegos[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

P_270<-table(RPerfilesTable$Horas.Semana.Dedicada.Actividades.Entretenimiento[RPerfilesTable$Promedio.Primer.Semestre=="Bueno"])
P_271<-table(RPerfilesTable$Horas.Semana.Dedicada.Actividades.Entretenimiento[RPerfilesTable$Promedio.Primer.Semestre=="Malo"])
P_272<-table(RPerfilesTable$Horas.Semana.Dedicada.Actividades.Entretenimiento[RPerfilesTable$Promedio.Primer.Semestre=="Regular"])

#Calculo de entropías
EV_00<--(P_00[1]/N*log2(P_00[1]/N)+P_01[1]/N*log2(P_01[1]/N)+P_02[1]/N*log2(P_02[1]/N))
EV_01<--(P_00[2]/N*log2(P_00[2]/N)+P_01[2]/N*log2(P_01[2]/N)+P_02[2]/N*log2(P_02[2]/N))
EV_02<--(P_00[3]/N*log2(P_00[3]/N)+P_01[3]/N*log2(P_01[3]/N)+P_02[3]/N*log2(P_02[3]/N))
E0<-((P_00[1]+P_01[1]+P_02[1])*EV_00+(P_00[2]+P_01[2]+P_02[2])*EV_01+(P_00[3]+P_01[3]+P_02[3])*EV_02)/N

EV_10<--(P_10[1]/N*log2(P_10[1]/N)+P_11[1]/N*log2(P_11[1]/N)+P_12[1]/N*log2(P_12[1]/N))
EV_11<--(P_10[2]/N*log2(P_10[2]/N)+P_11[2]/N*log2(P_11[2]/N)+P_12[2]/N*log2(P_12[2]/N))
E1<-((P_10[1]+P_11[1]+P_12[1])*EV_10+(P_10[2]+P_11[2]+P_12[2])*EV_11)/N

EV_20<--(P_20[1]/N*log2(P_20[1]/N)+P_21[1]/N*log2(P_21[1]/N)+P_22[1]/N*log2(P_22[1]/N))
EV_21<--(P_20[2]/N*log2(P_20[2]/N)+P_21[2]/N*log2(P_21[2]/N)+P_22[2]/N*log2(P_22[2]/N))
E2<-((P_20[1]+P_21[1]+P_22[1])*EV_20+(P_20[2]+P_21[2]+P_22[2])*EV_21)/N

EV_30<--(P_30[1]/N*log2(P_30[1]/N)+P_31[1]/N*log2(P_31[1]/N)+P_32[1]/N*log2(P_32[1]/N))
EV_31<--(P_30[2]/N*log2(P_30[2]/N)+P_31[2]/N*log2(P_31[2]/N)+P_32[2]/N*log2(P_32[2]/N))
E3<-((P_30[1]+P_31[1]+P_32[1])*EV_30+(P_30[2]+P_31[2]+P_32[2])*EV_31)/N

EV_40<--(P_40[1]/N*log2(P_40[1]/N)+P_41[1]/N*log2(P_41[1]/N)+P_42[1]/N*log2(P_42[1]/N))
EV_41<--(P_40[2]/N*log2(P_40[2]/N)+P_42[2]/N*log2(P_42[2]/N))
EV_42<--(P_40[3]/N*log2(P_40[3]/N)+P_41[3]/N*log2(P_41[3]/N)+P_42[3]/N*log2(P_42[3]/N))
E4<-((P_40[1]+P_41[1]+P_42[1])*EV_40+(P_40[2]+P_41[2]+P_42[2])*EV_41+(P_40[3]+P_41[3]+P_42[3])*EV_42)/N

EV_50<--(P_50[1]/N*log2(P_50[1]/N)+P_51[1]/N*log2(P_51[1]/N)+P_52[1]/N*log2(P_52[1]/N))
EV_51<--(P_50[2]/N*log2(P_50[2]/N)+P_52[2]/N*log2(P_52[2]/N))
EV_52<--(P_50[3]/N*log2(P_50[3]/N)+P_51[3]/N*log2(P_51[3]/N)+P_52[3]/N*log2(P_52[3]/N))
E5<-((P_50[1]+P_51[1]+P_52[1])*EV_50+(P_50[2]+P_51[2]+P_52[2])*EV_51+(P_50[3]+P_51[3]+P_52[3])*EV_52)/N

EV_60<--(P_60[1]/N*log2(P_60[1]/N)+P_62[1]/N*log2(P_62[1]/N))
EV_61<--(P_60[2]/N*log2(P_60[2]/N)+P_61[2]/N*log2(P_61[2]/N)+P_62[2]/N*log2(P_62[2]/N))
EV_62<--(P_60[3]/N*log2(P_60[3]/N)+P_61[3]/N*log2(P_61[3]/N)+P_62[3]/N*log2(P_62[3]/N))
E6<-((P_60[1]+P_61[1]+P_62[1])*EV_60+(P_60[2]+P_61[2]+P_62[2])*EV_61+(P_60[3]+P_61[3]+P_62[3])*EV_62)/N

EV_70<--(P_70[1]/N*log2(P_70[1]/N)+P_71[1]/N*log2(P_71[1]/N)+P_72[1]/N*log2(P_72[1]/N))
EV_71<--(P_70[2]/N*log2(P_70[2]/N)+P_71[2]/N*log2(P_71[2]/N)+P_72[2]/N*log2(P_72[2]/N))
E7<-((P_70[1]+P_71[1]+P_72[1])*EV_70+(P_70[2]+P_71[2]+P_72[2])*EV_71)/N

EV_80<--(P_80[1]/N*log2(P_80[1]/N)+P_81[1]/N*log2(P_81[1]/N)+P_82[1]/N*log2(P_82[1]/N))
EV_81<--(P_80[2]/N*log2(P_80[2]/N)+P_81[2]/N*log2(P_81[2]/N)+P_82[2]/N*log2(P_82[2]/N))
EV_82<--(P_80[3]/N*log2(P_80[3]/N)+P_81[3]/N*log2(P_81[3]/N)+P_82[3]/N*log2(P_82[3]/N))
EV_83<--(P_80[4]/N*log2(P_80[4]/N)+P_81[4]/N*log2(P_81[4]/N))
E8<-((P_80[1]+P_81[1]+P_82[1])*EV_80+(P_80[2]+P_81[2]+P_82[2])*EV_81+(P_80[3]+P_81[3]+P_82[3])*EV_82+(P_80[4]+P_81[4]+P_82[4])*EV_83)/N

EV_90<--(P_90[1]/N*log2(P_90[1]/N)+P_91[1]/N*log2(P_91[1]/N)+P_92[1]/N*log2(P_92[1]/N))
EV_91<--(P_90[2]/N*log2(P_90[2]/N)+P_91[2]/N*log2(P_91[2]/N)+P_92[2]/N*log2(P_92[2]/N))
E9<-((P_90[1]+P_91[1]+P_92[1])*EV_90+(P_90[2]+P_91[2]+P_92[2])*EV_91)/N

EV_100<--(P_100[1]/N*log2(P_100[1]/N)+P_101[1]/N*log2(P_101[1]/N)+P_102[1]/N*log2(P_102[1]/N))
EV_101<--(P_100[2]/N*log2(P_100[2]/N)+P_102[2]/N*log2(P_102[2]/N))
E10<-((P_100[1]+P_101[1]+P_102[1])*EV_100+(P_100[2]+P_101[2]+P_102[2])*EV_101)/N

EV_110<--(P_110[1]/N*log2(P_110[1]/N)+P_111[1]/N*log2(P_111[1]/N)+P_112[1]/N*log2(P_112[1]/N))
EV_111<--(P_110[2]/N*log2(P_110[2]/N)+P_111[2]/N*log2(P_111[2]/N)+P_112[2]/N*log2(P_112[2]/N))
E11<-((P_110[1]+P_111[1]+P_112[1])*EV_110+(P_110[2]+P_111[2]+P_112[2])*EV_111)/N

EV_120<--(P_120[1]/N*log2(P_120[1]/N)+P_121[1]/N*log2(P_121[1]/N)+P_122[1]/N*log2(P_122[1]/N))
EV_121<--(P_120[2]/N*log2(P_120[2]/N)+P_121[2]/N*log2(P_121[2]/N)+P_122[2]/N*log2(P_122[2]/N))
E12<-((P_120[1]+P_121[1]+P_122[1])*EV_120+(P_120[2]+P_121[2]+P_122[2])*EV_121)/N

EV_130<--(P_130[1]/N*log2(P_130[1]/N)+P_131[1]/N*log2(P_131[1]/N)+P_132[1]/N*log2(P_132[1]/N))
EV_131<--(P_130[2]/N*log2(P_130[2]/N)+P_131[2]/N*log2(P_131[2]/N)+P_132[2]/N*log2(P_132[2]/N))
E13<-((P_130[1]+P_131[1]+P_132[1])*EV_130+(P_130[2]+P_131[2]+P_132[2])*EV_131)/N

EV_140<--(P_140[1]/N*log2(P_140[1]/N)+P_141[1]/N*log2(P_141[1]/N)+P_142[1]/N*log2(P_142[1]/N))
EV_141<--(P_140[2]/N*log2(P_140[2]/N)+P_141[2]/N*log2(P_141[2]/N)+P_142[2]/N*log2(P_142[2]/N))
E14<-((P_140[1]+P_141[1]+P_142[1])*EV_140+(P_140[2]+P_141[2]+P_142[2])*EV_141)/N

EV_150<--(P_150[1]/N*log2(P_150[1]/N)+P_151[1]/N*log2(P_151[1]/N)+P_152[1]/N*log2(P_152[1]/N))
EV_151<--(P_150[2]/N*log2(P_150[2]/N)+P_151[2]/N*log2(P_151[2]/N)+P_152[2]/N*log2(P_152[2]/N))
E15<-((P_150[1]+P_151[1]+P_152[1])*EV_150+(P_150[2]+P_151[2]+P_152[2])*EV_151)/N

EV_160<--(P_160[1]/N*log2(P_160[1]/N)+P_161[1]/N*log2(P_161[1]/N)+P_162[1]/N*log2(P_162[1]/N))
EV_161<--(P_160[2]/N*log2(P_160[2]/N)+P_161[2]/N*log2(P_161[2]/N)+P_162[2]/N*log2(P_162[2]/N))
E16<-((P_160[1]+P_161[1]+P_162[1])*EV_160+(P_160[2]+P_161[2]+P_162[2])*EV_161)/N

EV_170<--(P_170[1]/N*log2(P_170[1]/N)+P_171[1]/N*log2(P_171[1]/N)+P_172[1]/N*log2(P_172[1]/N))
EV_171<--(P_170[2]/N*log2(P_170[2]/N)+P_172[2]/N*log2(P_172[2]/N))
E17<-((P_170[1]+P_171[1]+P_172[1])*EV_170+(P_170[2]+P_171[2]+P_172[2])*EV_171)/N

EV_180<--(P_180[1]/N*log2(P_180[1]/N)+P_181[1]/N*log2(P_181[1]/N)+P_182[1]/N*log2(P_182[1]/N))
EV_181<--(P_180[2]/N*log2(P_180[2]/N)+P_181[2]/N*log2(P_181[2]/N)+P_182[2]/N*log2(P_182[2]/N))
E18<-((P_180[1]+P_181[1]+P_182[1])*EV_180+(P_180[2]+P_181[2]+P_182[2])*EV_181)/N

EV_190<--(P_190[1]/N*log2(P_190[1]/N)+P_191[1]/N*log2(P_191[1]/N)+P_192[1]/N*log2(P_192[1]/N))
EV_191<--(P_190[2]/N*log2(P_190[2]/N)+P_192[2]/N*log2(P_192[2]/N))
E19<-((P_190[1]+P_191[1]+P_192[1])*EV_190+(P_190[2]+P_191[2]+P_192[2])*EV_191)/N

EV_180<--(P_180[1]/N*log2(P_180[1]/N)+P_181[1]/N*log2(P_181[1]/N)+P_182[1]/N*log2(P_182[1]/N))
EV_181<--(P_180[2]/N*log2(P_180[2]/N)+P_181[2]/N*log2(P_181[2]/N)+P_182[2]/N*log2(P_182[2]/N))
E18<-((P_180[1]+P_181[1]+P_182[1])*EV_180+(P_180[2]+P_181[2]+P_182[2])*EV_181)/N

EV_200<--(P_200[1]/N*log2(P_200[1]/N)+P_201[1]/N*log2(P_201[1]/N)+P_202[1]/N*log2(P_202[1]/N))
EV_201<--(P_200[2]/N*log2(P_200[2]/N)+P_201[2]/N*log2(P_201[2]/N)+P_202[2]/N*log2(P_202[2]/N))
E20<-((P_200[1]+P_201[1]+P_202[1])*EV_200+(P_200[2]+P_201[2]+P_202[2])*EV_201)/N

EV_210<--(P_210[1]/N*log2(P_210[1]/N)+P_211[1]/N*log2(P_211[1]/N)+P_212[1]/N*log2(P_212[1]/N))
EV_211<--(P_210[2]/N*log2(P_210[2]/N)+P_211[2]/N*log2(P_211[2]/N)+P_212[2]/N*log2(P_212[2]/N))
E21<-((P_210[1]+P_211[1]+P_212[1])*EV_210+(P_210[2]+P_211[2]+P_212[2])*EV_211)/N

EV_220<--(P_220[1]/N*log2(P_220[1]/N)+P_221[1]/N*log2(P_221[1]/N)+P_222[1]/N*log2(P_222[1]/N))
EV_221<--(P_220[2]/N*log2(P_220[2]/N)+P_222[2]/N*log2(P_222[2]/N))
EV_222<--(P_220[3]/N*log2(P_220[3]/N)+P_221[3]/N*log2(P_221[3]/N)+P_222[3]/N*log2(P_222[3]/N))
EV_223<--(P_220[4]/N*log2(P_220[4]/N))
E22<-((P_220[1]+P_221[1]+P_222[1])*EV_220+(P_220[2]+P_221[2]+P_222[2])*EV_221+(P_220[3]+P_221[3]+P_222[3])*EV_222+(P_220[4]+P_221[4]+P_222[4])*EV_223)/N

EV_230<--(P_230[1]/N*log2(P_230[1]/N)+P_231[1]/N*log2(P_231[1]/N)+P_232[1]/N*log2(P_232[1]/N))
EV_231<--(P_230[2]/N*log2(P_230[2]/N)+P_231[2]/N*log2(P_231[2]/N)+P_232[2]/N*log2(P_232[2]/N))
EV_232<--(P_230[3]/N*log2(P_230[3]/N)+P_231[3]/N*log2(P_231[3]/N)+P_232[3]/N*log2(P_232[3]/N))
EV_233<--(P_230[4]/N*log2(P_230[4]/N)+P_232[4]/N*log2(P_232[4]/N))
E23<-((P_230[1]+P_231[1]+P_232[1])*EV_230+(P_230[2]+P_231[2]+P_232[2])*EV_231+(P_230[3]+P_231[3]+P_232[3])*EV_232+(P_230[4]+P_231[4]+P_232[4])*EV_233)/N

EV_240<--(P_240[1]/N*log2(P_240[1]/N)+P_241[1]/N*log2(P_241[1]/N)+P_242[1]/N*log2(P_242[1]/N))
EV_241<--(P_240[2]/N*log2(P_240[2]/N)+P_241[2]/N*log2(P_241[2]/N)+P_242[2]/N*log2(P_242[2]/N))
EV_242<--(P_240[3]/N*log2(P_240[3]/N)+P_241[3]/N*log2(P_241[3]/N)+P_242[3]/N*log2(P_242[3]/N))
EV_243<--(P_240[4]/N*log2(P_240[4]/N)+P_242[4]/N*log2(P_242[4]/N))
E24<-((P_240[1]+P_241[1]+P_242[1])*EV_240+(P_240[2]+P_241[2]+P_242[2])*EV_241+(P_240[3]+P_241[3]+P_242[3])*EV_242+(P_240[4]+P_241[4]+P_242[4])*EV_243)/N

EV_250<--(P_250[1]/N*log2(P_250[1]/N)+P_251[1]/N*log2(P_251[1]/N)+P_252[1]/N*log2(P_252[1]/N))
EV_251<--(P_250[2]/N*log2(P_250[2]/N)+P_251[2]/N*log2(P_251[2]/N)+P_252[2]/N*log2(P_252[2]/N))
E25<-((P_250[1]+P_251[1]+P_252[1])*EV_250+(P_250[2]+P_251[2]+P_252[2])*EV_251)/N

EV_260<--(P_260[1]/N*log2(P_260[1]/N)+P_261[1]/N*log2(P_261[1]/N)+P_262[1]/N*log2(P_262[1]/N))
EV_261<--(P_260[2]/N*log2(P_260[2]/N)+P_261[2]/N*log2(P_261[2]/N)+P_262[2]/N*log2(P_262[2]/N))
EV_262<--(P_260[3]/N*log2(P_260[3]/N)+P_261[3]/N*log2(P_261[3]/N)+P_262[3]/N*log2(P_262[3]/N))
E26<-((P_260[1]+P_261[1]+P_262[1])*EV_260+(P_260[2]+P_261[2]+P_262[2])*EV_261+(P_260[3]+P_261[3]+P_262[3])*EV_262)/N

EV_270<--(P_270[1]/N*log2(P_270[1]/N)+P_271[1]/N*log2(P_271[1]/N)+P_272[1]/N*log2(P_272[1]/N))
EV_271<--(P_270[2]/N*log2(P_270[2]/N)+P_272[2]/N*log2(P_272[2]/N))
EV_272<--(P_270[3]/N*log2(P_270[3]/N)+P_271[3]/N*log2(P_271[3]/N)+P_272[3]/N*log2(P_272[3]/N))
E27<-((P_270[1]+P_271[1]+P_272[1])*EV_270+(P_270[2]+P_271[2]+P_272[2])*EV_271+(P_270[3]+P_271[3]+P_272[3])*EV_272)/N

#La entropía menor corresponde a E0 que es Edad

