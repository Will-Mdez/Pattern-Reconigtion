#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
#namefile <- "//home//will-mdez//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
library(dplyr) 

dataPerfiles <- read.table(namefile, header = TRUE, sep =',')

#Descripción de los datos
##Nombre a las columnas de datos


caracteristicas<-c(1,2,6,7,8,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35)

dataPerfiles<-dataPerfiles[caracteristicas]
dataPerfiles[dataPerfiles=="regularmente"]<-"a veces"
dataPerfiles[dataPerfiles=="Pariente"]<-"Otra persona"
dataPerfiles$Horas.DÃ.a.Dedicada.Redes.Sociales[dataPerfiles$Horas.DÃ.a.Dedicada.Redes.Sociales=="0"]<-"<1"
#dimensiones
dim(dataPerfiles)

#summary(dataPerfiles)
#names(dataPerfiles)

#Columnas con datos Cualitativos
cualitativos <- c(2,3,4,5,7,8,9,10,14,15,16,19,20,21,22,23,24,25,26,27,28,29)
#Columnas con datos Cuantitativos
cuantitativos<- c(1,6,11,12,13,17,18,23)
namesP <- colnames(dataPerfiles)

#Separamos los datos categóricos
dataPerfiles2 <- lapply(cualitativos,function(x) factor(dataPerfiles[,x]))

names(dataPerfiles2) <- namesP[cualitativos]
dataPerfiles2 <- as.data.frame(dataPerfiles2)

#summary(dataPerfiles2)
dim(dataPerfiles2)

#Datos cuantitativos
dataPerfiles_cuanti<-dataPerfiles[cuantitativos]
#summary(dataPerfiles_cuanti)

#Imputación de datos 
FeatNames<-namesP[cuantitativos]
mean_features <- sapply(FeatNames, function(x) mean(dataPerfiles_cuanti[[x]]))
#mean_features
sd_features <- sapply(FeatNames, function(x) sd(dataPerfiles_cuanti[[x]]))
#sd_features
#Identificación de valores extremos
dataPerfiles_SE<-filter(dataPerfiles_cuanti,dataPerfiles_cuanti$Edad<(mean_features[1]+3*sd_features[1]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Promedio.Preparatoria<(mean_features[2]+3*sd_features[2]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Materias.Cursadas.Primer.Semestre<(mean_features[3]+3*sd_features[3]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Promedio.Primer.Semestre<(mean_features[4]+3*sd_features[4]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Materias.Aprobadas.Primer.Semestre<(mean_features[5]+3*sd_features[5]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Horas.Promedio.Estudio.Examenes<(mean_features[6]+3*sd_features[6]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Horas.Promedio.Estudio.Actividades.Escolares<(mean_features[7]+3*sd_features[7]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_SE$Horas.Semana.Divertirse.con.sus.amigos<(mean_features[8]+3*sd_features[8]))
#summary(dataPerfiles_SE)
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
#summary(dataPerfiles_Norm)

#Discretizamos para clase con promedio
dataP_Promedio <- dataPerfiles_cuanti$Promedio.Primer.Semestre
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre<6] <- "Malo"
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre>=6 & dataPerfiles_cuanti$Promedio.Primer.Semestre<8]<- "Regular"
dataP_Promedio[dataPerfiles_cuanti$Promedio.Primer.Semestre>=8]<- "Bueno"


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
#summary(dataP_Ord)


##Discretizacion Datos Cualitativos
dataCopy <- dataPerfiles2

dataPerfilesDiscretizada<-cbind(dataP_Ord,dataPerfiles2[,c(1:15,17:22)])
summary(dataPerfilesDiscretizada$ConsumÃ.a.Alcohol.Entre.semana)
dataPerfilesDiscretizada$Edad<-factor(dataPerfilesDiscretizada$Edad)
dataPerfilesDiscretizada$Promedio.Preparatoria<-factor(dataPerfilesDiscretizada$Promedio.Preparatoria)
dataPerfilesDiscretizada$Materias.Cursadas.Primer.Semestre<-factor(dataPerfilesDiscretizada$Materias.Cursadas.Primer.Semestre)
dataPerfilesDiscretizada$Promedio.Primer.Semestre<-factor(dataPerfilesDiscretizada$Promedio.Primer.Semestre)
dataPerfilesDiscretizada$Materias.Aprobadas.Primer.Semestre<-factor(dataPerfilesDiscretizada$Materias.Aprobadas.Primer.Semestre)
dataPerfilesDiscretizada$Horas.Promedio.Estudio.Examenes<-factor(dataPerfilesDiscretizada$Horas.Promedio.Estudio.Examenes)
dataPerfilesDiscretizada$Horas.Promedio.Estudio.Actividades.Escolares<-factor(dataPerfilesDiscretizada$Horas.Promedio.Estudio.Actividades.Escolares)
dataPerfilesDiscretizada$Horas.Semana.Divertirse.con.sus.amigos<-factor(dataPerfilesDiscretizada$Horas.Semana.Divertirse.con.sus.amigos)
dataPerfilesDiscretizada$Clase<-factor(dataPerfilesDiscretizada$Clase)

dim(dataPerfilesDiscretizada)
dataPerfilesDiscretizada<-dataPerfilesDiscretizada[,c(1:3,5:30)]
dim(dataPerfilesDiscretizada)
summary(dataPerfilesDiscretizada)

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

freqClass <- table(dataP_Ord$Clase)
freqClass
freqClass_df <- as.data.frame(freqClass)
names(freqClass_df) <- c("Clase","freq")
library(ggplot2)
p <- ggplot(data=freqClass_df,aes(x=Clase, y=freq)) + geom_bar(stat="identity", fill="#95E4E6")
p



dataPerfiles_Final<-dataPerfilesDiscretizada
summary(dataPerfiles_Final)
#Rankeo
#Entropia
N<-155

Pij00<-table(dataPerfiles_Final$Edad[dataPerfiles_Final$Clase=="Bueno"])
Pij01<-table(dataPerfiles_Final$Edad[dataPerfiles_Final$Clase=="Malo"])
Pij02<-table(dataPerfiles_Final$Edad[dataPerfiles_Final$Clase=="Regular"])

Pij10<-table(dataPerfiles_Final$Promedio.Preparatoria[dataPerfiles_Final$Clase=="Bueno"])
Pij11<-table(dataPerfiles_Final$Promedio.Preparatoria[dataPerfiles_Final$Clase=="Malo"])
Pij12<-table(dataPerfiles_Final$Promedio.Preparatoria[dataPerfiles_Final$Clase=="Regular"])

Pij20<-table(dataPerfiles_Final$Materias.Cursadas.Primer.Semestre[dataPerfiles_Final$Clase=="Bueno"])
Pij21<-table(dataPerfiles_Final$Materias.Cursadas.Primer.Semestre[dataPerfiles_Final$Clase=="Malo"])
Pij22<-table(dataPerfiles_Final$Materias.Cursadas.Primer.Semestre[dataPerfiles_Final$Clase=="Regular"])

Pij30<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre[dataPerfiles_Final$Clase=="Bueno"])
Pij31<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre[dataPerfiles_Final$Clase=="Malo"])
Pij32<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre[dataPerfiles_Final$Clase=="Regular"])

Pij40<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Examenes[dataPerfiles_Final$Clase=="Bueno"])
Pij41<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Examenes[dataPerfiles_Final$Clase=="Malo"])
Pij42<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Examenes[dataPerfiles_Final$Clase=="Regular"])

Pij50<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Actividades.Escolares[dataPerfiles_Final$Clase=="Bueno"])
Pij51<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Actividades.Escolares[dataPerfiles_Final$Clase=="Malo"])
Pij52<-table(dataPerfiles_Final$Horas.Promedio.Estudio.Actividades.Escolares[dataPerfiles_Final$Clase=="Regular"])

Pij60<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos[dataPerfiles_Final$Clase=="Bueno"])
Pij61<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos[dataPerfiles_Final$Clase=="Malo"])
Pij62<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos[dataPerfiles_Final$Clase=="Regular"])

Pij70<-table(dataPerfiles_Final$Genero[dataPerfiles_Final$Clase=="Bueno"])
Pij71<-table(dataPerfiles_Final$Genero[dataPerfiles_Final$Clase=="Malo"])
Pij72<-table(dataPerfiles_Final$Genero[dataPerfiles_Final$Clase=="Regular"])

Pij80<-table(dataPerfiles_Final$Tutor[dataPerfiles_Final$Clase=="Bueno"])
Pij81<-table(dataPerfiles_Final$Tutor[dataPerfiles_Final$Clase=="Malo"])
Pij82<-table(dataPerfiles_Final$Tutor[dataPerfiles_Final$Clase=="Regular"])

Pij90<-table(dataPerfiles_Final$Vive.con.Familia[dataPerfiles_Final$Clase=="Bueno"])
Pij91<-table(dataPerfiles_Final$Vive.con.Familia[dataPerfiles_Final$Clase=="Malo"])
Pij92<-table(dataPerfiles_Final$Vive.con.Familia[dataPerfiles_Final$Clase=="Regular"])

Pij100<-table(dataPerfiles_Final$Enfermedad.CrÃ.nica[dataPerfiles_Final$Clase=="Bueno"])
Pij101<-table(dataPerfiles_Final$Enfermedad.CrÃ.nica[dataPerfiles_Final$Clase=="Malo"])
Pij102<-table(dataPerfiles_Final$Enfermedad.CrÃ.nica[dataPerfiles_Final$Clase=="Regular"])

Pij110<-table(dataPerfiles_Final$Materias.Relacionadas[dataPerfiles_Final$Clase=="Bueno"])
Pij111<-table(dataPerfiles_Final$Materias.Relacionadas[dataPerfiles_Final$Clase=="Malo"])
Pij112<-table(dataPerfiles_Final$Materias.Relacionadas[dataPerfiles_Final$Clase=="Regular"])

Pij120<-table(dataPerfiles_Final$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[dataPerfiles_Final$Clase=="Bueno"])
Pij121<-table(dataPerfiles_Final$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[dataPerfiles_Final$Clase=="Malo"])
Pij122<-table(dataPerfiles_Final$Carrera.Elegida.Ajusta.A.Gustos.Habilidades[dataPerfiles_Final$Clase=="Regular"])

Pij130<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Familiar[dataPerfiles_Final$Clase=="Bueno"])
Pij131<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Familiar[dataPerfiles_Final$Clase=="Malo"])
Pij132<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Familiar[dataPerfiles_Final$Clase=="Regular"])

Pij140<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[dataPerfiles_Final$Clase=="Bueno"])
Pij141<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[dataPerfiles_Final$Clase=="Malo"])
Pij142<-table(dataPerfiles_Final$Buena.RelaciÃ.n.Amigos.CompaÃ.eros[dataPerfiles_Final$Clase=="Regular"])

Pij150<-table(dataPerfiles_Final$Materias.Reprobadas.De.ComputaciÃ.n[dataPerfiles_Final$Clase=="Bueno"])
Pij151<-table(dataPerfiles_Final$Materias.Reprobadas.De.ComputaciÃ.n[dataPerfiles_Final$Clase=="Malo"])
Pij152<-table(dataPerfiles_Final$Materias.Reprobadas.De.ComputaciÃ.n[dataPerfiles_Final$Clase=="Regular"])

Pij160<-table(dataPerfiles_Final$Materias.Reprobadas.De.MatemÃ.ticas[dataPerfiles_Final$Clase=="Bueno"])
Pij161<-table(dataPerfiles_Final$Materias.Reprobadas.De.MatemÃ.ticas[dataPerfiles_Final$Clase=="Malo"])
Pij162<-table(dataPerfiles_Final$Materias.Reprobadas.De.MatemÃ.ticas[dataPerfiles_Final$Clase=="Regular"])

Pij170<-table(dataPerfiles_Final$Materias.Reprobadas.De.Ciensias.Sociales[dataPerfiles_Final$Clase=="Bueno"])
Pij171<-table(dataPerfiles_Final$Materias.Reprobadas.De.Ciensias.Sociales[dataPerfiles_Final$Clase=="Malo"])
Pij172<-table(dataPerfiles_Final$Materias.Reprobadas.De.Ciensias.Sociales[dataPerfiles_Final$Clase=="Regular"])

Pij180<-table(dataPerfiles_Final$Cursos.Extra.Primer.Semestre[dataPerfiles_Final$Clase=="Bueno"])
Pij181<-table(dataPerfiles_Final$Cursos.Extra.Primer.Semestre[dataPerfiles_Final$Clase=="Malo"])
Pij182<-table(dataPerfiles_Final$Cursos.Extra.Primer.Semestre[dataPerfiles_Final$Clase=="Regular"])

Pij190<-table(dataPerfiles_Final$Noviazgo[dataPerfiles_Final$Clase=="Bueno"])
Pij191<-table(dataPerfiles_Final$Noviazgo[dataPerfiles_Final$Clase=="Malo"])
Pij192<-table(dataPerfiles_Final$Noviazgo[dataPerfiles_Final$Clase=="Regular"])

Pij200<-table(dataPerfiles_Final$Actividad.Cultural.o.Deportiva[dataPerfiles_Final$Clase=="Bueno"])
Pij201<-table(dataPerfiles_Final$Actividad.Cultural.o.Deportiva[dataPerfiles_Final$Clase=="Malo"])
Pij202<-table(dataPerfiles_Final$Actividad.Cultural.o.Deportiva[dataPerfiles_Final$Clase=="Regular"])

Pij210<-table(dataPerfiles_Final$Tiempo.con.sus.amigos.compaÃ.eros[dataPerfiles_Final$Clase=="Bueno"])
Pij211<-table(dataPerfiles_Final$Tiempo.con.sus.amigos.compaÃ.eros[dataPerfiles_Final$Clase=="Malo"])
Pij212<-table(dataPerfiles_Final$Tiempo.con.sus.amigos.compaÃ.eros[dataPerfiles_Final$Clase=="Regular"])

Pij220<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Entre.semana[dataPerfiles_Final$Clase=="Bueno"])
Pij221<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Entre.semana[dataPerfiles_Final$Clase=="Malo"])
Pij222<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Entre.semana[dataPerfiles_Final$Clase=="Regular"])

Pij230<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Fin.semana[dataPerfiles_Final$Clase=="Bueno"])
Pij231<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Fin.semana[dataPerfiles_Final$Clase=="Malo"])
Pij232<-table(dataPerfiles_Final$ConsumÃ.a.Alcohol.Fin.semana[dataPerfiles_Final$Clase=="Regular"])

Pij240<-table(dataPerfiles_Final$Horas.DÃ.a.Dedicada.Redes.Sociales[dataPerfiles_Final$Clase=="Bueno"])
Pij241<-table(dataPerfiles_Final$Horas.DÃ.a.Dedicada.Redes.Sociales[dataPerfiles_Final$Clase=="Malo"])
Pij242<-table(dataPerfiles_Final$Horas.DÃ.a.Dedicada.Redes.Sociales[dataPerfiles_Final$Clase=="Regular"])

Pij250<-table(dataPerfiles_Final$Gustan.Videojuegos[dataPerfiles_Final$Clase=="Bueno"])
Pij251<-table(dataPerfiles_Final$Gustan.Videojuegos[dataPerfiles_Final$Clase=="Malo"])
Pij252<-table(dataPerfiles_Final$Gustan.Videojuegos[dataPerfiles_Final$Clase=="Regular"])

Pij260<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Videojuegos[dataPerfiles_Final$Clase=="Bueno"])
Pij261<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Videojuegos[dataPerfiles_Final$Clase=="Malo"])
Pij262<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Videojuegos[dataPerfiles_Final$Clase=="Regular"])

Pij270<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Actividades.Entretenimiento[dataPerfiles_Final$Clase=="Bueno"])
Pij271<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Actividades.Entretenimiento[dataPerfiles_Final$Clase=="Malo"])
Pij272<-table(dataPerfiles_Final$Horas.Semana.Dedicada.Actividades.Entretenimiento[dataPerfiles_Final$Clase=="Regular"])

#Calculo de entropías
EVij00<--(Pij00[1]/N*log2(Pij00[1]/N)+Pij01[1]/N*log2(Pij01[1]/N)+Pij02[1]/N*log2(Pij02[1]/N))
EVij01<--(Pij00[2]/N*log2(Pij00[2]/N)+Pij01[2]/N*log2(Pij01[2]/N)+Pij02[2]/N*log2(Pij02[2]/N))
EVij02<--(Pij00[3]/N*log2(Pij00[3]/N)+Pij01[3]/N*log2(Pij01[3]/N)+Pij02[3]/N*log2(Pij02[3]/N))
E0<-((Pij00[1]+Pij01[1]+Pij02[1])*EVij00+(Pij00[2]+Pij01[2]+Pij02[2])*EVij01+(Pij00[3]+Pij01[3]+Pij02[3])*EVij02)/N

EVij10<--(Pij10[1]/N*log2(Pij10[1]/N)+Pij11[1]/N*log2(Pij11[1]/N)+Pij12[1]/N*log2(Pij12[1]/N))
EVij11<--(Pij10[2]/N*log2(Pij10[2]/N)+Pij11[2]/N*log2(Pij11[2]/N)+Pij12[2]/N*log2(Pij12[2]/N))
E1<-((Pij10[1]+Pij11[1]+Pij12[1])*EVij10+(Pij10[2]+Pij11[2]+Pij12[2])*EVij11)/N

EVij20<--(Pij20[1]/N*log2(Pij20[1]/N)+Pij21[1]/N*log2(Pij21[1]/N)+Pij22[1]/N*log2(Pij22[1]/N))
EVij21<--(Pij20[2]/N*log2(Pij20[2]/N)+Pij21[2]/N*log2(Pij21[2]/N)+Pij22[2]/N*log2(Pij22[2]/N))
E2<-((Pij20[1]+Pij21[1]+Pij22[1])*EVij20+(Pij20[2]+Pij21[2]+Pij22[2])*EVij21)/N

EVij30<--(Pij30[1]/N*log2(Pij30[1]/N)+Pij31[1]/N*log2(Pij31[1]/N)+Pij32[1]/N*log2(Pij32[1]/N))
EVij31<--(Pij30[2]/N*log2(Pij30[2]/N)+Pij31[2]/N*log2(Pij31[2]/N)+Pij32[2]/N*log2(Pij32[2]/N))
E3<-((Pij30[1]+Pij31[1]+Pij32[1])*EVij30+(Pij30[2]+Pij31[2]+Pij32[2])*EVij31)/N

EVij40<--(Pij40[1]/N*log2(Pij40[1]/N)+Pij41[1]/N*log2(Pij41[1]/N)+Pij42[1]/N*log2(Pij42[1]/N))
EVij41<--(Pij40[2]/N*log2(Pij40[2]/N)+Pij42[2]/N*log2(Pij42[2]/N))
EVij42<--(Pij40[3]/N*log2(Pij40[3]/N)+Pij41[3]/N*log2(Pij41[3]/N)+Pij42[3]/N*log2(Pij42[3]/N))
E4<-((Pij40[1]+Pij41[1]+Pij42[1])*EVij40+(Pij40[2]+Pij41[2]+Pij42[2])*EVij41+(Pij40[3]+Pij41[3]+Pij42[3])*EVij42)/N

EVij50<--(Pij50[1]/N*log2(Pij50[1]/N)+Pij51[1]/N*log2(Pij51[1]/N)+Pij52[1]/N*log2(Pij52[1]/N))
EVij51<--(Pij50[2]/N*log2(Pij50[2]/N)+Pij52[2]/N*log2(Pij52[2]/N))
EVij52<--(Pij50[3]/N*log2(Pij50[3]/N)+Pij51[3]/N*log2(Pij51[3]/N)+Pij52[3]/N*log2(Pij52[3]/N))
E5<-((Pij50[1]+Pij51[1]+Pij52[1])*EVij50+(Pij50[2]+Pij51[2]+Pij52[2])*EVij51+(Pij50[3]+Pij51[3]+Pij52[3])*EVij52)/N

EVij60<--(Pij60[1]/N*log2(Pij60[1]/N)+Pij62[1]/N*log2(Pij62[1]/N))
EVij61<--(Pij60[2]/N*log2(Pij60[2]/N)+Pij61[2]/N*log2(Pij61[2]/N)+Pij62[2]/N*log2(Pij62[2]/N))
EVij62<--(Pij60[3]/N*log2(Pij60[3]/N)+Pij61[3]/N*log2(Pij61[3]/N)+Pij62[3]/N*log2(Pij62[3]/N))
E6<-((Pij60[1]+Pij61[1]+Pij62[1])*EVij60+(Pij60[2]+Pij61[2]+Pij62[2])*EVij61+(Pij60[3]+Pij61[3]+Pij62[3])*EVij62)/N

EVij70<--(Pij70[1]/N*log2(Pij70[1]/N)+Pij71[1]/N*log2(Pij71[1]/N)+Pij72[1]/N*log2(Pij72[1]/N))
EVij71<--(Pij70[2]/N*log2(Pij70[2]/N)+Pij71[2]/N*log2(Pij71[2]/N)+Pij72[2]/N*log2(Pij72[2]/N))
E7<-((Pij70[1]+Pij71[1]+Pij72[1])*EVij70+(Pij70[2]+Pij71[2]+Pij72[2])*EVij71)/N

EVij80<--(Pij80[1]/N*log2(Pij80[1]/N)+Pij81[1]/N*log2(Pij81[1]/N)+Pij82[1]/N*log2(Pij82[1]/N))
EVij81<--(Pij80[2]/N*log2(Pij80[2]/N)+Pij81[2]/N*log2(Pij81[2]/N)+Pij82[2]/N*log2(Pij82[2]/N))
EVij82<--(Pij80[3]/N*log2(Pij80[3]/N)+Pij81[3]/N*log2(Pij81[3]/N)+Pij82[3]/N*log2(Pij82[3]/N))
E8<-((Pij80[1]+Pij81[1]+Pij82[1])*EVij80+(Pij80[2]+Pij81[2]+Pij82[2])*EVij81+(Pij80[3]+Pij81[3]+Pij82[3])*EVij82)/N

EVij90<--(Pij90[1]/N*log2(Pij90[1]/N)+Pij91[1]/N*log2(Pij91[1]/N)+Pij92[1]/N*log2(Pij92[1]/N))
EVij91<--(Pij90[2]/N*log2(Pij90[2]/N)+Pij91[2]/N*log2(Pij91[2]/N)+Pij92[2]/N*log2(Pij92[2]/N))
E9<-((Pij90[1]+Pij91[1]+Pij92[1])*EVij90+(Pij90[2]+Pij91[2]+Pij92[2])*EVij91)/N

EVij100<--(Pij100[1]/N*log2(Pij100[1]/N)+Pij101[1]/N*log2(Pij101[1]/N)+Pij102[1]/N*log2(Pij102[1]/N))
EVij101<--(Pij100[2]/N*log2(Pij100[2]/N)+Pij102[2]/N*log2(Pij102[2]/N))
E10<-((Pij100[1]+Pij101[1]+Pij102[1])*EVij100+(Pij100[2]+Pij101[2]+Pij102[2])*EVij101)/N

EVij110<--(Pij110[1]/N*log2(Pij110[1]/N)+Pij111[1]/N*log2(Pij111[1]/N)+Pij112[1]/N*log2(Pij112[1]/N))
EVij111<--(Pij110[2]/N*log2(Pij110[2]/N)+Pij111[2]/N*log2(Pij111[2]/N)+Pij112[2]/N*log2(Pij112[2]/N))
E11<-((Pij110[1]+Pij111[1]+Pij112[1])*EVij110+(Pij110[2]+Pij111[2]+Pij112[2])*EVij111)/N

EVij120<--(Pij120[1]/N*log2(Pij120[1]/N)+Pij121[1]/N*log2(Pij121[1]/N)+Pij122[1]/N*log2(Pij122[1]/N))
EVij121<--(Pij120[2]/N*log2(Pij120[2]/N)+Pij121[2]/N*log2(Pij121[2]/N)+Pij122[2]/N*log2(Pij122[2]/N))
E12<-((Pij120[1]+Pij121[1]+Pij122[1])*EVij120+(Pij120[2]+Pij121[2]+Pij122[2])*EVij121)/N

EVij130<--(Pij130[1]/N*log2(Pij130[1]/N)+Pij131[1]/N*log2(Pij131[1]/N)+Pij132[1]/N*log2(Pij132[1]/N))
EVij131<--(Pij130[2]/N*log2(Pij130[2]/N)+Pij131[2]/N*log2(Pij131[2]/N)+Pij132[2]/N*log2(Pij132[2]/N))
E13<-((Pij130[1]+Pij131[1]+Pij132[1])*EVij130+(Pij130[2]+Pij131[2]+Pij132[2])*EVij131)/N

EVij140<--(Pij140[1]/N*log2(Pij140[1]/N)+Pij141[1]/N*log2(Pij141[1]/N)+Pij142[1]/N*log2(Pij142[1]/N))
EVij141<--(Pij140[2]/N*log2(Pij140[2]/N)+Pij141[2]/N*log2(Pij141[2]/N)+Pij142[2]/N*log2(Pij142[2]/N))
E14<-((Pij140[1]+Pij141[1]+Pij142[1])*EVij140+(Pij140[2]+Pij141[2]+Pij142[2])*EVij141)/N

EVij150<--(Pij150[1]/N*log2(Pij150[1]/N)+Pij151[1]/N*log2(Pij151[1]/N)+Pij152[1]/N*log2(Pij152[1]/N))
EVij151<--(Pij150[2]/N*log2(Pij150[2]/N)+Pij151[2]/N*log2(Pij151[2]/N)+Pij152[2]/N*log2(Pij152[2]/N))
E15<-((Pij150[1]+Pij151[1]+Pij152[1])*EVij150+(Pij150[2]+Pij151[2]+Pij152[2])*EVij151)/N

EVij160<--(Pij160[1]/N*log2(Pij160[1]/N)+Pij161[1]/N*log2(Pij161[1]/N)+Pij162[1]/N*log2(Pij162[1]/N))
EVij161<--(Pij160[2]/N*log2(Pij160[2]/N)+Pij161[2]/N*log2(Pij161[2]/N)+Pij162[2]/N*log2(Pij162[2]/N))
E16<-((Pij160[1]+Pij161[1]+Pij162[1])*EVij160+(Pij160[2]+Pij161[2]+Pij162[2])*EVij161)/N

EVij170<--(Pij170[1]/N*log2(Pij170[1]/N)+Pij171[1]/N*log2(Pij171[1]/N)+Pij172[1]/N*log2(Pij172[1]/N))
EVij171<--(Pij170[2]/N*log2(Pij170[2]/N)+Pij172[2]/N*log2(Pij172[2]/N))
E17<-((Pij170[1]+Pij171[1]+Pij172[1])*EVij170+(Pij170[2]+Pij171[2]+Pij172[2])*EVij171)/N

EVij180<--(Pij180[1]/N*log2(Pij180[1]/N)+Pij181[1]/N*log2(Pij181[1]/N)+Pij182[1]/N*log2(Pij182[1]/N))
EVij181<--(Pij180[2]/N*log2(Pij180[2]/N)+Pij181[2]/N*log2(Pij181[2]/N)+Pij182[2]/N*log2(Pij182[2]/N))
E18<-((Pij180[1]+Pij181[1]+Pij182[1])*EVij180+(Pij180[2]+Pij181[2]+Pij182[2])*EVij181)/N

EVij190<--(Pij190[1]/N*log2(Pij190[1]/N)+Pij191[1]/N*log2(Pij191[1]/N)+Pij192[1]/N*log2(Pij192[1]/N))
EVij191<--(Pij190[2]/N*log2(Pij190[2]/N)+Pij192[2]/N*log2(Pij192[2]/N))
E19<-((Pij190[1]+Pij191[1]+Pij192[1])*EVij190+(Pij190[2]+Pij191[2]+Pij192[2])*EVij191)/N

EVij180<--(Pij180[1]/N*log2(Pij180[1]/N)+Pij181[1]/N*log2(Pij181[1]/N)+Pij182[1]/N*log2(Pij182[1]/N))
EVij181<--(Pij180[2]/N*log2(Pij180[2]/N)+Pij181[2]/N*log2(Pij181[2]/N)+Pij182[2]/N*log2(Pij182[2]/N))
E18<-((Pij180[1]+Pij181[1]+Pij182[1])*EVij180+(Pij180[2]+Pij181[2]+Pij182[2])*EVij181)/N

EVij200<--(Pij200[1]/N*log2(Pij200[1]/N)+Pij201[1]/N*log2(Pij201[1]/N)+Pij202[1]/N*log2(Pij202[1]/N))
EVij201<--(Pij200[2]/N*log2(Pij200[2]/N)+Pij201[2]/N*log2(Pij201[2]/N)+Pij202[2]/N*log2(Pij202[2]/N))
E20<-((Pij200[1]+Pij201[1]+Pij202[1])*EVij200+(Pij200[2]+Pij201[2]+Pij202[2])*EVij201)/N

EVij210<--(Pij210[1]/N*log2(Pij210[1]/N)+Pij211[1]/N*log2(Pij211[1]/N)+Pij212[1]/N*log2(Pij212[1]/N))
EVij211<--(Pij210[2]/N*log2(Pij210[2]/N)+Pij211[2]/N*log2(Pij211[2]/N)+Pij212[2]/N*log2(Pij212[2]/N))
E21<-((Pij210[1]+Pij211[1]+Pij212[1])*EVij210+(Pij210[2]+Pij211[2]+Pij212[2])*EVij211)/N

EVij220<--(Pij220[1]/N*log2(Pij220[1]/N)+Pij222[1]/N*log2(Pij222[1]/N))
EVij221<--(Pij220[2]/N*log2(Pij220[2]/N)+Pij222[2]/N*log2(Pij222[2]/N))
EVij222<--(Pij220[3]/N*log2(Pij220[3]/N)+Pij221[3]/N*log2(Pij221[3]/N)+Pij222[3]/N*log2(Pij222[3]/N))
E22<-((Pij220[1]+Pij221[1]+Pij222[1])*EVij220+(Pij220[2]+Pij221[2]+Pij222[2])*EVij221+(Pij220[3]+Pij221[3]+Pij222[3])*EVij222)/N
#
EVij230<--(Pij230[1]/N*log2(Pij230[1]/N)+Pij231[1]/N*log2(Pij231[1]/N)+Pij232[1]/N*log2(Pij232[1]/N))
EVij231<--(Pij230[2]/N*log2(Pij230[2]/N)+Pij232[2]/N*log2(Pij232[2]/N))
EVij232<--(Pij230[3]/N*log2(Pij230[3]/N)+Pij231[3]/N*log2(Pij231[3]/N)+Pij232[3]/N*log2(Pij232[3]/N))
E23<-((Pij230[1]+Pij231[1]+Pij232[1])*EVij230+(Pij230[2]+Pij231[2]+Pij232[2])*EVij231+(Pij230[3]+Pij231[3]+Pij232[3])*EVij232)/N

EVij240<--(Pij240[1]/N*log2(Pij240[1]/N)+Pij241[1]/N*log2(Pij241[1]/N)+Pij242[1]/N*log2(Pij242[1]/N))
EVij241<--(Pij240[2]/N*log2(Pij240[2]/N)+Pij241[2]/N*log2(Pij241[2]/N)+Pij242[2]/N*log2(Pij242[2]/N))
EVij242<--(Pij240[3]/N*log2(Pij240[3]/N)+Pij241[3]/N*log2(Pij241[3]/N)+Pij242[3]/N*log2(Pij242[3]/N))
E24<-((Pij240[1]+Pij241[1]+Pij242[1])*EVij240+(Pij240[2]+Pij241[2]+Pij242[2])*EVij241+(Pij240[3]+Pij241[3]+Pij242[3])*EVij242)/N

EVij250<--(Pij250[1]/N*log2(Pij250[1]/N)+Pij251[1]/N*log2(Pij251[1]/N)+Pij252[1]/N*log2(Pij252[1]/N))
EVij251<--(Pij250[2]/N*log2(Pij250[2]/N)+Pij251[2]/N*log2(Pij251[2]/N)+Pij252[2]/N*log2(Pij252[2]/N))
E25<-((Pij250[1]+Pij251[1]+Pij252[1])*EVij250+(Pij250[2]+Pij251[2]+Pij252[2])*EVij251)/N

EVij260<--(Pij260[1]/N*log2(Pij260[1]/N)+Pij261[1]/N*log2(Pij261[1]/N)+Pij262[1]/N*log2(Pij262[1]/N))
EVij261<--(Pij260[2]/N*log2(Pij260[2]/N)+Pij261[2]/N*log2(Pij261[2]/N)+Pij262[2]/N*log2(Pij262[2]/N))
EVij262<--(Pij260[3]/N*log2(Pij260[3]/N)+Pij261[3]/N*log2(Pij261[3]/N)+Pij262[3]/N*log2(Pij262[3]/N))
E26<-((Pij260[1]+Pij261[1]+Pij262[1])*EVij260+(Pij260[2]+Pij261[2]+Pij262[2])*EVij261+(Pij260[3]+Pij261[3]+Pij262[3])*EVij262)/N

EVij270<--(Pij270[1]/N*log2(Pij270[1]/N)+Pij271[1]/N*log2(Pij271[1]/N)+Pij272[1]/N*log2(Pij272[1]/N))
EVij271<--(Pij270[2]/N*log2(Pij270[2]/N)+Pij272[2]/N*log2(Pij272[2]/N))
EVij272<--(Pij270[3]/N*log2(Pij270[3]/N)+Pij271[3]/N*log2(Pij271[3]/N)+Pij272[3]/N*log2(Pij272[3]/N))
E27<-((Pij270[1]+Pij271[1]+Pij272[1])*EVij270+(Pij270[2]+Pij271[2]+Pij272[2])*EVij271+(Pij270[3]+Pij271[3]+Pij272[3])*EVij272)/N

EntropiasP<-c(E0,E1,E2,E3,E4,E5,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15,E16,E17,E18,E19,E20,E21,E22,E23,E24,E25,E26,E27)
names(EntropiasP)<-names(dataPerfiles_Final[c(1:7,9:29)])
EntropiasP
sort(EntropiasP,index.return=TRUE)
#La entropía menor corresponde a E0 que es Horas.Semana.Divertirse.con.sus.amigos

#La mayor es Enfermedad Cronica


##CORRELACION PEARSON
#Mayor Entropia
dim(dataPerfiles_Final)
N<-163
tablaHSDA_E<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Edad)
sumRows_tablaHSDA_E<-rowSums(tablaHSDA_E)
sumCols_tablaHSDA_E<-colSums(tablaHSDA_E)
tablaHSDA_E_chi<-matrix(c((sumRows_tablaHSDA_E[1]*sumCols_tablaHSDA_E)/N,(sumRows_tablaHSDA_E[2]*sumCols_tablaHSDA_E)/N,(sumRows_tablaHSDA_E[3]*sumCols_tablaHSDA_E)/N),nrow=3)
chi1<-chisq.test(tablaHSDA_E_chi)
chi1<-chi1$p.value


tablaHSDA_PPrepa<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Promedio.Preparatoria)
sumRows_tablaHSDA_PPrepa<-rowSums(tablaHSDA_PPrepa)
sumCols_tablaHSDA_PPrepa<-colSums(tablaHSDA_PPrepa)
tablaHSDA_NS_chi<-matrix(c((sumRows_tablaHSDA_PPrepa[1]*sumCols_tablaHSDA_PPrepa)/N,(sumRows_tablaHSDA_PPrepa[2]*sumCols_tablaHSDA_PPrepa)/N,(sumRows_tablaHSDA_PPrepa[3]*sumCols_tablaHSDA_PPrepa)/N),nrow=3)
chi2<-chisq.test(tablaHSDA_NS_chi)
chi2<-chi2$p.value

tablaHSDA_MCPS<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Cursadas.Primer.Semestre)
sumRows_tablaHSDA_MCPS<-rowSums(tablaHSDA_MCPS)
sumCols_tablaHSDA_MCPS<-colSums(tablaHSDA_MCPS)
tablaHSDA_MCPS_chi<-matrix(c((sumRows_tablaHSDA_MCPS[1]*sumCols_tablaHSDA_MCPS)/N,(sumRows_tablaHSDA_MCPS[2]*sumCols_tablaHSDA_MCPS)/N,(sumRows_tablaHSDA_MCPS[3]*sumCols_tablaHSDA_MCPS)/N),nrow=3)
chi3<-chisq.test(tablaHSDA_MCPS_chi)
chi3<-chi3$p.value

tablaHSDA_MAPS<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre)
sumRows_tablaHSDA_MAPS<-rowSums(tablaHSDA_MAPS)
sumCols_tablaHSDA_MAPS<-colSums(tablaHSDA_MAPS)
tablaHSDA_MAPS_chi<-matrix(c((sumRows_tablaHSDA_MAPS[1]*sumCols_tablaHSDA_MAPS)/N,(sumRows_tablaHSDA_MAPS[2]*sumCols_tablaHSDA_MAPS)/N,(sumRows_tablaHSDA_MAPS[3]*sumCols_tablaHSDA_MAPS)/N),nrow=3)
chi4<-chisq.test(tablaHSDA_MAPS_chi)
chi4<-chi4$p.value

tablaHSDA_HPEE<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.Promedio.Estudio.Examenes)
sumRows_tablaHSDA_HPEE<-rowSums(tablaHSDA_HPEE)
sumCols_tablaHSDA_HPEE<-colSums(tablaHSDA_HPEE)
tablaHSDA_HPEE_chi<-matrix(c((sumRows_tablaHSDA_HPEE[1]*sumCols_tablaHSDA_HPEE)/N,(sumRows_tablaHSDA_HPEE[2]*sumCols_tablaHSDA_HPEE)/N,(sumRows_tablaHSDA_HPEE[3]*sumCols_tablaHSDA_HPEE)/N),nrow=3)
chi5<-chisq.test(tablaHSDA_HPEE_chi)
chi5<-chi5$p.value

tablaHSDA_HPEAE<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.Promedio.Estudio.Actividades.Escolares)
sumRows_tablaHSDA_HPEAE<-rowSums(tablaHSDA_HPEAE)
sumCols_tablaHSDA_HPEAE<-colSums(tablaHSDA_HPEAE)
tablaHSDA_HPEAE_chi<-matrix(c((sumRows_tablaHSDA_HPEAE[1]*sumCols_tablaHSDA_HPEAE)/N,(sumRows_tablaHSDA_HPEAE[2]*sumCols_tablaHSDA_HPEAE)/N,(sumRows_tablaHSDA_HPEAE[3]*sumCols_tablaHSDA_HPEAE)/N),nrow=3)
chi6<-chisq.test(tablaHSDA_HPEAE_chi)
chi6<-chi6$p.value


#tablaHSDA_HSDA<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos)
#sumRows_tablaHSDA_HSDA<-rowSums(tablaHSDA_HSDA)
#sumCols_tablaHSDA_HSDA<-colSums(tablaHSDA_HSDA)
#tablaHSDA_HSDA_chi<-matrix(c((sumRows_tablaHSDA_HSDA[1]*sumCols_tablaHSDA_HSDA)/N,(sumRows_tablaHSDA_HSDA[2]*sumCols_tablaHSDA_HSDA)/N),nrow=2)
#chi7<-chisq.test(tablaHSDA_HSDA_chi)
#chi7<-chi7$p.value

tablaHSDA_Gen<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Genero)
sumRows_tablaHSDA_Gen<-rowSums(tablaHSDA_Gen)
sumCols_tablaHSDA_Gen<-colSums(tablaHSDA_Gen)
tablaHSDA_Gen_chi<-matrix(c((sumRows_tablaHSDA_Gen[1]*sumCols_tablaHSDA_Gen)/N,(sumRows_tablaHSDA_Gen[2]*sumCols_tablaHSDA_Gen)/N,(sumRows_tablaHSDA_Gen[3]*sumCols_tablaHSDA_Gen)/N),nrow=3)
chi7<-chisq.test(tablaHSDA_Gen_chi)
chi7<-chi7$p.value

tablaHSDA_Tut<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Tutor)
sumRows_tablaHSDA_Tut<-rowSums(tablaHSDA_Tut)
sumCols_tablaHSDA_Tut<-colSums(tablaHSDA_Tut)
tablaHSDA_Tut_chi<-matrix(c((sumRows_tablaHSDA_Tut[1]*sumCols_tablaHSDA_Tut)/N,(sumRows_tablaHSDA_Tut[2]*sumCols_tablaHSDA_Tut)/N,(sumRows_tablaHSDA_Tut[3]*sumCols_tablaHSDA_Tut)/N),nrow=3)
chi8<-chisq.test(tablaHSDA_Tut_chi)
chi8<-chi8$p.value

tablaHSDA_VF<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Vive.con.Familia)
sumRows_tablaHSDA_VF<-rowSums(tablaHSDA_VF)
sumCols_tablaHSDA_VF<-colSums(tablaHSDA_VF)
tablaHSDA_VF_chi<-matrix(c((sumRows_tablaHSDA_VF[1]*sumCols_tablaHSDA_VF)/N,(sumRows_tablaHSDA_VF[2]*sumCols_tablaHSDA_VF)/N,(sumRows_tablaHSDA_VF[3]*sumCols_tablaHSDA_VF)/N),nrow=3)
chi9<-chisq.test(tablaHSDA_VF_chi)
chi9<-chi9$p.value

tablaHSDA_EnfCro<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Enfermedad.CrÃ.nica)
sumRows_tablaHSDA_EnfCro<-rowSums(tablaHSDA_EnfCro)
sumCols_tablaHSDA_EnfCro<-colSums(tablaHSDA_EnfCro)
tablaHSDA_EnfCro_chi<-matrix(c((sumRows_tablaHSDA_EnfCro[1]*sumCols_tablaHSDA_EnfCro)/N,(sumRows_tablaHSDA_EnfCro[2]*sumCols_tablaHSDA_EnfCro)/N,(sumRows_tablaHSDA_EnfCro[3]*sumCols_tablaHSDA_EnfCro)/N),nrow=3)
chi10<-chisq.test(tablaHSDA_EnfCro_chi)
chi10<-chi10$p.value

tablaHSDA_MatRel<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Relacionadas)
sumRows_tablaHSDA_MatRel<-rowSums(tablaHSDA_MatRel)
sumCols_tablaHSDA_MatRel<-colSums(tablaHSDA_MatRel)
tablaHSDA_MatRel_chi<-matrix(c((sumRows_tablaHSDA_MatRel[1]*sumCols_tablaHSDA_MatRel)/N,(sumRows_tablaHSDA_MatRel[2]*sumCols_tablaHSDA_MatRel)/N,(sumRows_tablaHSDA_MatRel[3]*sumCols_tablaHSDA_MatRel)/N),nrow=3)
chi11<-chisq.test(tablaHSDA_MatRel_chi)
chi11<-chi11$p.value

tablaHSDA_CEAGH<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Carrera.Elegida.Ajusta.A.Gustos.Habilidades)
sumRows_tablaHSDA_CEAGH<-rowSums(tablaHSDA_CEAGH)
sumCols_tablaHSDA_CEAGH<-colSums(tablaHSDA_CEAGH)
tablaHSDA_CEAGH_chi<-matrix(c((sumRows_tablaHSDA_CEAGH[1]*sumCols_tablaHSDA_CEAGH)/N,(sumRows_tablaHSDA_CEAGH[2]*sumCols_tablaHSDA_CEAGH)/N,(sumRows_tablaHSDA_CEAGH[3]*sumCols_tablaHSDA_CEAGH)/N),nrow=3)
chi12<-chisq.test(tablaHSDA_CEAGH_chi)
chi12<-chi12$p.value

tablaHSDA_BRF<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Buena.RelaciÃ.n.Familiar)
sumRows_tablaHSDA_BRF<-rowSums(tablaHSDA_BRF)
sumCols_tablaHSDA_BRF<-colSums(tablaHSDA_BRF)
tablaHSDA_BRF_chi<-matrix(c((sumRows_tablaHSDA_BRF[1]*sumCols_tablaHSDA_BRF)/N,(sumRows_tablaHSDA_BRF[2]*sumCols_tablaHSDA_BRF)/N,(sumRows_tablaHSDA_BRF[3]*sumCols_tablaHSDA_BRF)/N),nrow=3)
chi13<-chisq.test(tablaHSDA_BRF_chi)
chi13<-chi13$p.value

tablaHSDA_BRAC<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Buena.RelaciÃ.n.Amigos.CompaÃ.eros)
sumRows_tablaHSDA_BRAC<-rowSums(tablaHSDA_BRAC)
sumCols_tablaHSDA_BRAC<-colSums(tablaHSDA_BRAC)
tablaHSDA_BRAC_chi<-matrix(c((sumRows_tablaHSDA_BRAC[1]*sumCols_tablaHSDA_BRAC)/N,(sumRows_tablaHSDA_BRAC[2]*sumCols_tablaHSDA_BRAC)/N,(sumRows_tablaHSDA_BRAC[3]*sumCols_tablaHSDA_BRAC)/N),nrow=3)
chi14<-chisq.test(tablaHSDA_BRAC_chi)
chi14<-chi14$p.value

tablaHSDA_MRComp<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Reprobadas.De.ComputaciÃ.n)
sumRows_tablaHSDA_MRComp<-rowSums(tablaHSDA_MRComp)
sumCols_tablaHSDA_MRComp<-colSums(tablaHSDA_MRComp)
tablaHSDA_MRComp_chi<-matrix(c((sumRows_tablaHSDA_MRComp[1]*sumCols_tablaHSDA_MRComp)/N,(sumRows_tablaHSDA_MRComp[2]*sumCols_tablaHSDA_MRComp)/N,(sumRows_tablaHSDA_MRComp[3]*sumCols_tablaHSDA_MRComp)/N),nrow=3)
chi15<-chisq.test(tablaHSDA_MRComp_chi)
chi15<-chi15$p.value
 
tablaHSDA_MRMat<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Reprobadas.De.MatemÃ.ticas)
sumRows_tablaHSDA_MRMat<-rowSums(tablaHSDA_MRMat)
sumCols_tablaHSDA_MRMat<-colSums(tablaHSDA_MRMat)
tablaHSDA_MRMat_chi<-matrix(c((sumRows_tablaHSDA_MRMat[1]*sumCols_tablaHSDA_MRMat)/N,(sumRows_tablaHSDA_MRMat[2]*sumCols_tablaHSDA_MRMat)/N,(sumRows_tablaHSDA_MRMat[3]*sumCols_tablaHSDA_MRMat)/N),nrow=3)
chi16<-chisq.test(tablaHSDA_MRMat_chi)
chi16<-chi16$p.value

tablaHSDA_MRCiSos<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Materias.Reprobadas.De.Ciensias.Sociales)
sumRows_tablaHSDA_MRCiSos<-rowSums(tablaHSDA_MRCiSos)
sumCols_tablaHSDA_MRCiSos<-colSums(tablaHSDA_MRCiSos)
tablaHSDA_MRCiSos_chi<-matrix(c((sumRows_tablaHSDA_MRCiSos[1]*sumCols_tablaHSDA_MRCiSos)/N,(sumRows_tablaHSDA_MRCiSos[2]*sumCols_tablaHSDA_MRCiSos)/N,(sumRows_tablaHSDA_MRCiSos[3]*sumCols_tablaHSDA_MRCiSos)/N),nrow=3)
chi17<-chisq.test(tablaHSDA_MRCiSos_chi)
chi17<-chi17$p.value

tablaHSDA_CEPS<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Cursos.Extra.Primer.Semestre)
sumRows_tablaHSDA_CEPS<-rowSums(tablaHSDA_CEPS)
sumCols_tablaHSDA_CEPS<-colSums(tablaHSDA_CEPS)
tablaHSDA_CEPS_chi<-matrix(c((sumRows_tablaHSDA_CEPS[1]*sumCols_tablaHSDA_CEPS)/N,(sumRows_tablaHSDA_CEPS[2]*sumCols_tablaHSDA_CEPS)/N,(sumRows_tablaHSDA_CEPS[3]*sumCols_tablaHSDA_CEPS)/N),nrow=3)
chi18<-chisq.test(tablaHSDA_CEPS_chi)
chi18<-chi18$p.value

tablaHSDA_Nov<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Noviazgo)
sumRows_tablaHSDA_Nov<-rowSums(tablaHSDA_Nov)
sumCols_tablaHSDA_Nov<-colSums(tablaHSDA_Nov)
tablaHSDA_Nov_chi<-matrix(c((sumRows_tablaHSDA_Nov[1]*sumCols_tablaHSDA_Nov)/N,(sumRows_tablaHSDA_Nov[2]*sumCols_tablaHSDA_Nov)/N,(sumRows_tablaHSDA_Nov[3]*sumCols_tablaHSDA_Nov)/N),nrow=3)
chi19<-chisq.test(tablaHSDA_Nov_chi)
chi19<-chi19$p.value

tablaHSDA_ACoD<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Actividad.Cultural.o.Deportiva)
sumRows_tablaHSDA_ACoD<-rowSums(tablaHSDA_ACoD)
sumCols_tablaHSDA_ACoD<-colSums(tablaHSDA_ACoD)
tablaHSDA_ACoD_chi<-matrix(c((sumRows_tablaHSDA_ACoD[1]*sumCols_tablaHSDA_ACoD)/N,(sumRows_tablaHSDA_ACoD[2]*sumCols_tablaHSDA_ACoD)/N,(sumRows_tablaHSDA_ACoD[3]*sumCols_tablaHSDA_ACoD)/N),nrow=3)
chi20<-chisq.test(tablaHSDA_ACoD_chi)
chi20<-chi20$p.value

tablaHSDA_TAmComp<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Tiempo.con.sus.amigos.compaÃ.eros)
sumRows_tablaHSDA_TAmComp<-rowSums(tablaHSDA_TAmComp)
sumCols_tablaHSDA_TAmComp<-colSums(tablaHSDA_TAmComp)
tablaHSDA_TAmComp_chi<-matrix(c((sumRows_tablaHSDA_TAmComp[1]*sumCols_tablaHSDA_TAmComp)/N,(sumRows_tablaHSDA_TAmComp[2]*sumCols_tablaHSDA_TAmComp)/N,(sumRows_tablaHSDA_TAmComp[3]*sumCols_tablaHSDA_TAmComp)/N),nrow=3)
chi21<-chisq.test(tablaHSDA_TAmComp_chi)
chi21<-chi21$p.value

tablaHSDA_CAlcES<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$ConsumÃ.a.Alcohol.Entre.semana)
sumRows_tablaHSDA_CAlcES<-rowSums(tablaHSDA_CAlcES)
sumCols_tablaHSDA_CAlcES<-colSums(tablaHSDA_CAlcES)
tablaHSDA_CAlcES_chi<-matrix(c((sumRows_tablaHSDA_CAlcES[1]*sumCols_tablaHSDA_CAlcES)/N,(sumRows_tablaHSDA_CAlcES[2]*sumCols_tablaHSDA_CAlcES)/N,(sumRows_tablaHSDA_CAlcES[3]*sumCols_tablaHSDA_CAlcES)/N),nrow=3)
chi22<-chisq.test(tablaHSDA_CAlcES_chi)
chi22<-chi22$p.value

tablaHSDA_CAlcFS<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$ConsumÃ.a.Alcohol.Fin.semana)
sumRows_tablaHSDA_CAlcFS<-rowSums(tablaHSDA_CAlcFS)
sumCols_tablaHSDA_CAlcFS<-colSums(tablaHSDA_CAlcFS)
tablaHSDA_CAlcFS_chi<-matrix(c((sumRows_tablaHSDA_CAlcFS[1]*sumCols_tablaHSDA_CAlcFS)/N,(sumRows_tablaHSDA_CAlcFS[2]*sumCols_tablaHSDA_CAlcFS)/N,(sumRows_tablaHSDA_CAlcFS[3]*sumCols_tablaHSDA_CAlcFS)/N),nrow=3)
chi23<-chisq.test(tablaHSDA_CAlcFS_chi)
chi23<-chi23$p.value

tablaHSDA_HrDRS<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.DÃ.a.Dedicada.Redes.Sociales)
sumRows_tablaHSDA_HrDRS<-rowSums(tablaHSDA_HrDRS)
sumCols_tablaHSDA_HrDRS<-colSums(tablaHSDA_HrDRS)
tablaHSDA_HrDRS_chi<-matrix(c((sumRows_tablaHSDA_HrDRS[1]*sumCols_tablaHSDA_HrDRS)/N,(sumRows_tablaHSDA_HrDRS[2]*sumCols_tablaHSDA_HrDRS)/N,(sumRows_tablaHSDA_HrDRS[3]*sumCols_tablaHSDA_HrDRS)/N),nrow=3)
chi24<-chisq.test(tablaHSDA_HrDRS_chi)
chi24<-chi24$p.value

tablaHSDA_GVJ<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Gustan.Videojuegos)
sumRows_tablaHSDA_GVJ<-rowSums(tablaHSDA_GVJ)
sumCols_tablaHSDA_GVJ<-colSums(tablaHSDA_GVJ)
tablaHSDA_GVJ_chi<-matrix(c((sumRows_tablaHSDA_GVJ[1]*sumCols_tablaHSDA_GVJ)/N,(sumRows_tablaHSDA_GVJ[2]*sumCols_tablaHSDA_GVJ)/N,(sumRows_tablaHSDA_GVJ[3]*sumCols_tablaHSDA_GVJ)/N),nrow=3)
chi25<-chisq.test(tablaHSDA_GVJ_chi)
chi25<-chi25$p.value

tablaHSDA_HrSDVJ<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.Semana.Dedicada.Videojuegos)
sumRows_tablaHSDA_HrSDVJ<-rowSums(tablaHSDA_HrSDVJ)
sumCols_tablaHSDA_HrSDVJ<-colSums(tablaHSDA_HrSDVJ)
tablaHSDA_HrSDVJ_chi<-matrix(c((sumRows_tablaHSDA_HrSDVJ[1]*sumCols_tablaHSDA_HrSDVJ)/N,(sumRows_tablaHSDA_HrSDVJ[2]*sumCols_tablaHSDA_HrSDVJ)/N,(sumRows_tablaHSDA_HrSDVJ[3]*sumCols_tablaHSDA_HrSDVJ)/N),nrow=3)
chi26<-chisq.test(tablaHSDA_HrSDVJ_chi)
chi26<-chi26$p.value

tablaHSDA_HrSDAEntr<-table(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos,dataPerfiles_Final$Horas.Semana.Dedicada.Actividades.Entretenimiento)
sumRows_tablaHSDA_HrSDAEntr<-rowSums(tablaHSDA_HrSDAEntr)
sumCols_tablaHSDA_HrSDAEntr<-colSums(tablaHSDA_HrSDAEntr)
tablaHSDA_HrSDAEntr_chi<-matrix(c((sumRows_tablaHSDA_HrSDAEntr[1]*sumCols_tablaHSDA_HrSDAEntr)/N,(sumRows_tablaHSDA_HrSDAEntr[2]*sumCols_tablaHSDA_HrSDAEntr)/N,(sumRows_tablaHSDA_HrSDAEntr[3]*sumCols_tablaHSDA_HrSDAEntr)/N),nrow=3)
chi27<-chisq.test(tablaHSDA_HrSDAEntr_chi)
chi27<-chi27$p.value






chichiPerfiles<-c(chi1,chi2,chi3,chi4,chi5,chi6,chi7,chi8,chi9,chi10,chi11,chi12,chi13,chi14,chi15,chi16,chi17,chi18,chi19,chi20,chi21,chi22,chi23,chi24,chi25,chi26,chi27)
max(EntropiasP)
names(EntropiasP[c(1:6,8:28)])
FS<-(-0.5*EntropiasP[c(1:6,8:28)])-0.5*(abs(chichiPerfiles))
sort(FS,index.return=TRUE)
EntropiasP[4]
#F2 <-Materias.Aprobadas.Primer.Semestre
max(FS)



tablaMatApPrSem_E<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Edad)
sumRows_tablaMatApPrSem_E<-rowSums(tablaMatApPrSem_E)
sumCols_tablaMatApPrSem_E<-colSums(tablaMatApPrSem_E)
tablaMatApPrSem_E_chi<-matrix(c((sumRows_tablaMatApPrSem_E[1]*sumCols_tablaMatApPrSem_E)/N,(sumRows_tablaMatApPrSem_E[2]*sumCols_tablaMatApPrSem_E)/N),nrow=2)
chi_21<-chisq.test(tablaMatApPrSem_E_chi)
chi_21<-chi_21$p.value


tablaMatApPrSem_PPrepa<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Promedio.Preparatoria)
sumRows_tablaMatApPrSem_PPrepa<-rowSums(tablaMatApPrSem_PPrepa)
sumCols_tablaMatApPrSem_PPrepa<-colSums(tablaMatApPrSem_PPrepa)
tablaMatApPrSem_NS_chi<-matrix(c((sumRows_tablaMatApPrSem_PPrepa[1]*sumCols_tablaMatApPrSem_PPrepa)/N,(sumRows_tablaMatApPrSem_PPrepa[2]*sumCols_tablaMatApPrSem_PPrepa)/N),nrow=2)
chi_22<-chisq.test(tablaMatApPrSem_NS_chi)
chi_22<-chi_22$p.value

tablaMatApPrSem_MCPS<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Cursadas.Primer.Semestre)
sumRows_tablaMatApPrSem_MCPS<-rowSums(tablaMatApPrSem_MCPS)
sumCols_tablaMatApPrSem_MCPS<-colSums(tablaMatApPrSem_MCPS)
tablaMatApPrSem_MCPS_chi<-matrix(c((sumRows_tablaMatApPrSem_MCPS[1]*sumCols_tablaMatApPrSem_MCPS)/N,(sumRows_tablaMatApPrSem_MCPS[2]*sumCols_tablaMatApPrSem_MCPS)/N),nrow=2)
chi_23<-chisq.test(tablaMatApPrSem_MCPS_chi)
chi_23<-chi_23$p.value

#tablaMatApPrSem_MAPS<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre)
#sumRows_tablaMatApPrSem_MAPS<-rowSums(tablaMatApPrSem_MAPS)
#sumCols_tablaMatApPrSem_MAPS<-colSums(tablaMatApPrSem_MAPS)
#tablaMatApPrSem_MAPS_chi<-matrix(c((sumRows_tablaMatApPrSem_MAPS[1]*sumCols_tablaMatApPrSem_MAPS)/N,(sumRows_tablaMatApPrSem_MAPS[2]*sumCols_tablaMatApPrSem_MAPS)/N,(s),nrow=2)
#chi_24<-chisq.test(tablaMatApPrSem_MAPS_chi)
#chi_24<-chi_24$p.value

tablaMatApPrSem_HPEE<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Horas.Promedio.Estudio.Examenes)
sumRows_tablaMatApPrSem_HPEE<-rowSums(tablaMatApPrSem_HPEE)
sumCols_tablaMatApPrSem_HPEE<-colSums(tablaMatApPrSem_HPEE)
tablaMatApPrSem_HPEE_chi<-matrix(c((sumRows_tablaMatApPrSem_HPEE[1]*sumCols_tablaMatApPrSem_HPEE)/N,(sumRows_tablaMatApPrSem_HPEE[2]*sumCols_tablaMatApPrSem_HPEE)/N),nrow=2)
chi_25<-chisq.test(tablaMatApPrSem_HPEE_chi)
chi_25<-chi_25$p.value

tablaMatApPrSem_HPEAE<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Horas.Promedio.Estudio.Actividades.Escolares)
sumRows_tablaMatApPrSem_HPEAE<-rowSums(tablaMatApPrSem_HPEAE)
sumCols_tablaMatApPrSem_HPEAE<-colSums(tablaMatApPrSem_HPEAE)
tablaMatApPrSem_HPEAE_chi<-matrix(c((sumRows_tablaMatApPrSem_HPEAE[1]*sumCols_tablaMatApPrSem_HPEAE)/N,(sumRows_tablaMatApPrSem_HPEAE[2]*sumCols_tablaMatApPrSem_HPEAE)/N),nrow=2)
chi_26<-chisq.test(tablaMatApPrSem_HPEAE_chi)
chi_26<-chi_26$p.value

tablaMatApPrSem_Gen<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Genero)
sumRows_tablaMatApPrSem_Gen<-rowSums(tablaMatApPrSem_Gen)
sumCols_tablaMatApPrSem_Gen<-colSums(tablaMatApPrSem_Gen)
tablaMatApPrSem_Gen_chi<-matrix(c((sumRows_tablaMatApPrSem_Gen[1]*sumCols_tablaMatApPrSem_Gen)/N,(sumRows_tablaMatApPrSem_Gen[2]*sumCols_tablaMatApPrSem_Gen)/N),nrow=2)
chi_27<-chisq.test(tablaMatApPrSem_Gen_chi)
chi_27<-chi_27$p.value

tablaMatApPrSem_Tut<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Tutor)
sumRows_tablaMatApPrSem_Tut<-rowSums(tablaMatApPrSem_Tut)
sumCols_tablaMatApPrSem_Tut<-colSums(tablaMatApPrSem_Tut)
tablaMatApPrSem_Tut_chi<-matrix(c((sumRows_tablaMatApPrSem_Tut[1]*sumCols_tablaMatApPrSem_Tut)/N,(sumRows_tablaMatApPrSem_Tut[2]*sumCols_tablaMatApPrSem_Tut)/N),nrow=2)
chi_28<-chisq.test(tablaMatApPrSem_Tut_chi)
chi_28<-chi_28$p.value

tablaMatApPrSem_VF<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Vive.con.Familia)
sumRows_tablaMatApPrSem_VF<-rowSums(tablaMatApPrSem_VF)
sumCols_tablaMatApPrSem_VF<-colSums(tablaMatApPrSem_VF)
tablaMatApPrSem_VF_chi<-matrix(c((sumRows_tablaMatApPrSem_VF[1]*sumCols_tablaMatApPrSem_VF)/N,(sumRows_tablaMatApPrSem_VF[2]*sumCols_tablaMatApPrSem_VF)/N),nrow=2)
chi_29<-chisq.test(tablaMatApPrSem_VF_chi)
chi_29<-chi_29$p.value

tablaMatApPrSem_EnfCro<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Enfermedad.CrÃ.nica)
sumRows_tablaMatApPrSem_EnfCro<-rowSums(tablaMatApPrSem_EnfCro)
sumCols_tablaMatApPrSem_EnfCro<-colSums(tablaMatApPrSem_EnfCro)
tablaMatApPrSem_EnfCro_chi<-matrix(c((sumRows_tablaMatApPrSem_EnfCro[1]*sumCols_tablaMatApPrSem_EnfCro)/N,(sumRows_tablaMatApPrSem_EnfCro[2]*sumCols_tablaMatApPrSem_EnfCro)/N),nrow=2)
chi210<-chisq.test(tablaMatApPrSem_EnfCro_chi)
chi210<-chi210$p.value

tablaMatApPrSem_MatRel<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Relacionadas)
sumRows_tablaMatApPrSem_MatRel<-rowSums(tablaMatApPrSem_MatRel)
sumCols_tablaMatApPrSem_MatRel<-colSums(tablaMatApPrSem_MatRel)
tablaMatApPrSem_MatRel_chi<-matrix(c((sumRows_tablaMatApPrSem_MatRel[1]*sumCols_tablaMatApPrSem_MatRel)/N,(sumRows_tablaMatApPrSem_MatRel[2]*sumCols_tablaMatApPrSem_MatRel)/N),nrow=2)
chi211<-chisq.test(tablaMatApPrSem_MatRel_chi)
chi211<-chi211$p.value

tablaMatApPrSem_CEAGH<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Carrera.Elegida.Ajusta.A.Gustos.Habilidades)
sumRows_tablaMatApPrSem_CEAGH<-rowSums(tablaMatApPrSem_CEAGH)
sumCols_tablaMatApPrSem_CEAGH<-colSums(tablaMatApPrSem_CEAGH)
tablaMatApPrSem_CEAGH_chi<-matrix(c((sumRows_tablaMatApPrSem_CEAGH[1]*sumCols_tablaMatApPrSem_CEAGH)/N,(sumRows_tablaMatApPrSem_CEAGH[2]*sumCols_tablaMatApPrSem_CEAGH)/N),nrow=2)
chi212<-chisq.test(tablaMatApPrSem_CEAGH_chi)
chi212<-chi212$p.value

tablaMatApPrSem_BRF<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Buena.RelaciÃ.n.Familiar)
sumRows_tablaMatApPrSem_BRF<-rowSums(tablaMatApPrSem_BRF)
sumCols_tablaMatApPrSem_BRF<-colSums(tablaMatApPrSem_BRF)
tablaMatApPrSem_BRF_chi<-matrix(c((sumRows_tablaMatApPrSem_BRF[1]*sumCols_tablaMatApPrSem_BRF)/N,(sumRows_tablaMatApPrSem_BRF[2]*sumCols_tablaMatApPrSem_BRF)/N),nrow=2)
chi213<-chisq.test(tablaMatApPrSem_BRF_chi)
chi213<-chi213$p.value

tablaMatApPrSem_BRAC<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Buena.RelaciÃ.n.Amigos.CompaÃ.eros)
sumRows_tablaMatApPrSem_BRAC<-rowSums(tablaMatApPrSem_BRAC)
sumCols_tablaMatApPrSem_BRAC<-colSums(tablaMatApPrSem_BRAC)
tablaMatApPrSem_BRAC_chi<-matrix(c((sumRows_tablaMatApPrSem_BRAC[1]*sumCols_tablaMatApPrSem_BRAC)/N,(sumRows_tablaMatApPrSem_BRAC[2]*sumCols_tablaMatApPrSem_BRAC)/N),nrow=2)
chi214<-chisq.test(tablaMatApPrSem_BRAC_chi)
chi214<-chi214$p.value

tablaMatApPrSem_MRComp<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Reprobadas.De.ComputaciÃ.n)
sumRows_tablaMatApPrSem_MRComp<-rowSums(tablaMatApPrSem_MRComp)
sumCols_tablaMatApPrSem_MRComp<-colSums(tablaMatApPrSem_MRComp)
tablaMatApPrSem_MRComp_chi<-matrix(c((sumRows_tablaMatApPrSem_MRComp[1]*sumCols_tablaMatApPrSem_MRComp)/N,(sumRows_tablaMatApPrSem_MRComp[2]*sumCols_tablaMatApPrSem_MRComp)/N),nrow=2)
chi215<-chisq.test(tablaMatApPrSem_MRComp_chi)
chi215<-chi215$p.value

tablaMatApPrSem_MRMat<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Reprobadas.De.MatemÃ.ticas)
sumRows_tablaMatApPrSem_MRMat<-rowSums(tablaMatApPrSem_MRMat)
sumCols_tablaMatApPrSem_MRMat<-colSums(tablaMatApPrSem_MRMat)
tablaMatApPrSem_MRMat_chi<-matrix(c((sumRows_tablaMatApPrSem_MRMat[1]*sumCols_tablaMatApPrSem_MRMat)/N,(sumRows_tablaMatApPrSem_MRMat[2]*sumCols_tablaMatApPrSem_MRMat)/N),nrow=2)
chi216<-chisq.test(tablaMatApPrSem_MRMat_chi)
chi216<-chi216$p.value

tablaMatApPrSem_MRCiSos<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Materias.Reprobadas.De.Ciensias.Sociales)
sumRows_tablaMatApPrSem_MRCiSos<-rowSums(tablaMatApPrSem_MRCiSos)
sumCols_tablaMatApPrSem_MRCiSos<-colSums(tablaMatApPrSem_MRCiSos)
tablaMatApPrSem_MRCiSos_chi<-matrix(c((sumRows_tablaMatApPrSem_MRCiSos[1]*sumCols_tablaMatApPrSem_MRCiSos)/N,(sumRows_tablaMatApPrSem_MRCiSos[2]*sumCols_tablaMatApPrSem_MRCiSos)/N),nrow=2)
chi217<-chisq.test(tablaMatApPrSem_MRCiSos_chi)
chi217<-chi217$p.value

tablaMatApPrSem_CEPS<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Cursos.Extra.Primer.Semestre)
sumRows_tablaMatApPrSem_CEPS<-rowSums(tablaMatApPrSem_CEPS)
sumCols_tablaMatApPrSem_CEPS<-colSums(tablaMatApPrSem_CEPS)
tablaMatApPrSem_CEPS_chi<-matrix(c((sumRows_tablaMatApPrSem_CEPS[1]*sumCols_tablaMatApPrSem_CEPS)/N,(sumRows_tablaMatApPrSem_CEPS[2]*sumCols_tablaMatApPrSem_CEPS)/N),nrow=2)
chi218<-chisq.test(tablaMatApPrSem_CEPS_chi)
chi218<-chi218$p.value

tablaMatApPrSem_Nov<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Noviazgo)
sumRows_tablaMatApPrSem_Nov<-rowSums(tablaMatApPrSem_Nov)
sumCols_tablaMatApPrSem_Nov<-colSums(tablaMatApPrSem_Nov)
tablaMatApPrSem_Nov_chi<-matrix(c((sumRows_tablaMatApPrSem_Nov[1]*sumCols_tablaMatApPrSem_Nov)/N,(sumRows_tablaMatApPrSem_Nov[2]*sumCols_tablaMatApPrSem_Nov)/N),nrow=2)
chi219<-chisq.test(tablaMatApPrSem_Nov_chi)
chi219<-chi219$p.value

tablaMatApPrSem_ACoD<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Actividad.Cultural.o.Deportiva)
sumRows_tablaMatApPrSem_ACoD<-rowSums(tablaMatApPrSem_ACoD)
sumCols_tablaMatApPrSem_ACoD<-colSums(tablaMatApPrSem_ACoD)
tablaMatApPrSem_ACoD_chi<-matrix(c((sumRows_tablaMatApPrSem_ACoD[1]*sumCols_tablaMatApPrSem_ACoD)/N,(sumRows_tablaMatApPrSem_ACoD[2]*sumCols_tablaMatApPrSem_ACoD)/N),nrow=2)
chi220<-chisq.test(tablaMatApPrSem_ACoD_chi)
chi220<-chi220$p.value

tablaMatApPrSem_TAmComp<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Tiempo.con.sus.amigos.compaÃ.eros)
sumRows_tablaMatApPrSem_TAmComp<-rowSums(tablaMatApPrSem_TAmComp)
sumCols_tablaMatApPrSem_TAmComp<-colSums(tablaMatApPrSem_TAmComp)
tablaMatApPrSem_TAmComp_chi<-matrix(c((sumRows_tablaMatApPrSem_TAmComp[1]*sumCols_tablaMatApPrSem_TAmComp)/N,(sumRows_tablaMatApPrSem_TAmComp[2]*sumCols_tablaMatApPrSem_TAmComp)/N),nrow=2)
chi221<-chisq.test(tablaMatApPrSem_TAmComp_chi)
chi221<-chi221$p.value

tablaMatApPrSem_CAlcES<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$ConsumÃ.a.Alcohol.Entre.semana)
sumRows_tablaMatApPrSem_CAlcES<-rowSums(tablaMatApPrSem_CAlcES)
sumCols_tablaMatApPrSem_CAlcES<-colSums(tablaMatApPrSem_CAlcES)
tablaMatApPrSem_CAlcES_chi<-matrix(c((sumRows_tablaMatApPrSem_CAlcES[1]*sumCols_tablaMatApPrSem_CAlcES)/N,(sumRows_tablaMatApPrSem_CAlcES[2]*sumCols_tablaMatApPrSem_CAlcES)/N),nrow=2)
chi222<-chisq.test(tablaMatApPrSem_CAlcES_chi)
chi222<-chi222$p.value

tablaMatApPrSem_CAlcFS<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$ConsumÃ.a.Alcohol.Fin.semana)
sumRows_tablaMatApPrSem_CAlcFS<-rowSums(tablaMatApPrSem_CAlcFS)
sumCols_tablaMatApPrSem_CAlcFS<-colSums(tablaMatApPrSem_CAlcFS)
tablaMatApPrSem_CAlcFS_chi<-matrix(c((sumRows_tablaMatApPrSem_CAlcFS[1]*sumCols_tablaMatApPrSem_CAlcFS)/N,(sumRows_tablaMatApPrSem_CAlcFS[2]*sumCols_tablaMatApPrSem_CAlcFS)/N),nrow=2)
chi223<-chisq.test(tablaMatApPrSem_CAlcFS_chi)
chi223<-chi223$p.value

tablaMatApPrSem_HrDRS<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Horas.DÃ.a.Dedicada.Redes.Sociales)
sumRows_tablaMatApPrSem_HrDRS<-rowSums(tablaMatApPrSem_HrDRS)
sumCols_tablaMatApPrSem_HrDRS<-colSums(tablaMatApPrSem_HrDRS)
tablaMatApPrSem_HrDRS_chi<-matrix(c((sumRows_tablaMatApPrSem_HrDRS[1]*sumCols_tablaMatApPrSem_HrDRS)/N,(sumRows_tablaMatApPrSem_HrDRS[2]*sumCols_tablaMatApPrSem_HrDRS)/N),nrow=2)
chi224<-chisq.test(tablaMatApPrSem_HrDRS_chi)
chi224<-chi224$p.value

tablaMatApPrSem_GVJ<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Gustan.Videojuegos)
sumRows_tablaMatApPrSem_GVJ<-rowSums(tablaMatApPrSem_GVJ)
sumCols_tablaMatApPrSem_GVJ<-colSums(tablaMatApPrSem_GVJ)
tablaMatApPrSem_GVJ_chi<-matrix(c((sumRows_tablaMatApPrSem_GVJ[1]*sumCols_tablaMatApPrSem_GVJ)/N,(sumRows_tablaMatApPrSem_GVJ[2]*sumCols_tablaMatApPrSem_GVJ)/N),nrow=2)
chi225<-chisq.test(tablaMatApPrSem_GVJ_chi)
chi225<-chi225$p.value

tablaMatApPrSem_HrSDVJ<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Horas.Semana.Dedicada.Videojuegos)
sumRows_tablaMatApPrSem_HrSDVJ<-rowSums(tablaMatApPrSem_HrSDVJ)
sumCols_tablaMatApPrSem_HrSDVJ<-colSums(tablaMatApPrSem_HrSDVJ)
tablaMatApPrSem_HrSDVJ_chi<-matrix(c((sumRows_tablaMatApPrSem_HrSDVJ[1]*sumCols_tablaMatApPrSem_HrSDVJ)/N,(sumRows_tablaMatApPrSem_HrSDVJ[2]*sumCols_tablaMatApPrSem_HrSDVJ)/N),nrow=2)
chi226<-chisq.test(tablaMatApPrSem_HrSDVJ_chi)
chi226<-chi226$p.value

tablaMatApPrSem_HrSDAEntr<-table(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre,dataPerfiles_Final$Horas.Semana.Dedicada.Actividades.Entretenimiento)
sumRows_tablaMatApPrSem_HrSDAEntr<-rowSums(tablaMatApPrSem_HrSDAEntr)
sumCols_tablaMatApPrSem_HrSDAEntr<-colSums(tablaMatApPrSem_HrSDAEntr)
tablaMatApPrSem_HrSDAEntr_chi<-matrix(c((sumRows_tablaMatApPrSem_HrSDAEntr[1]*sumCols_tablaMatApPrSem_HrSDAEntr)/N,(sumRows_tablaMatApPrSem_HrSDAEntr[2]*sumCols_tablaMatApPrSem_HrSDAEntr)/N),nrow=2)
chi227<-chisq.test(tablaMatApPrSem_HrSDAEntr_chi)
chi227<-chi227$p.value

chichiPerfiles2<-c(chi_21,chi_22,chi_23,chi_25,chi_26,chi_27,chi_28,chi_29,chi210,chi211,chi212,chi213,chi214,chi215,chi216,chi217,chi218,chi219,chi220,chi221,chi222,chi223,chi224,chi225,chi226,chi227)
max(EntropiasP)
names(EntropiasP[c(1:3,5,6,8:28)])
FS<-(-0.5*EntropiasP[c(1:3,5,6,8:28)])-0.5*(abs(chichiPerfiles2))
sort(FS,index.return=TRUE)
EntropiasP[1]
#Edad <-Edad
max(FS)





######KNN
library(e1071)
library(naivebayes)
library(caret)
library(FNN)
library(sp)
library(raster)
library(dismo)
library(proxy)

#install.packages("proxy")
Horas.Semana.Divertirse.con.sus.amigos <- factor(dataPerfiles_Final$Horas.Semana.Divertirse.con.sus.amigos)
Materias.Aprobadas.Primer.Semestre <- factor(dataPerfiles_Final$Materias.Aprobadas.Primer.Semestre)
Edad <- factor(dataPerfiles_Final$Edad)
clasePerfiles <- factor(dataPerfiles_Final$Clase)

datasetKnnPerfiles <- data.frame(Horas.Semana.Divertirse.con.sus.amigos,Materias.Aprobadas.Primer.Semestre,Edad,clasePerfiles)
summary(datasetKnnPerfiles)


X <- datasetKnnPerfiles[, c("Horas.Semana.Divertirse.con.sus.amigos", "Materias.Aprobadas.Primer.Semestre", "Edad")]
y <- datasetKnnPerfiles$clasePerfiles

#Crear el clasificador KNN utilizando la distancia de Gower
library(caret)
library(cluster)

k_folds <- 5
accuracy <- vector("numeric", k_folds)  # Vector para almacenar las precisiones

# Obtener los índices de los folds
indices <- createFolds(y = datasetKnnPerfiles$clasePerfiles, k = k_folds)

for (i in 1:k_folds) {
  # Crear los dataframes de Train y Test
  train_indices <- indices[[i]]  # Índices para el conjunto de entrenamiento
  test_indices <- indices[[i]]  # Índices para el conjunto de prueba
  
  train_df <- datasetKnnPerfiles[train_indices, ]  # Dataframe de entrenamiento
  test_df <- datasetKnnPerfiles[test_indices, ]  # Dataframe de prueba
  
  dim(train_df)
  
  # Calcular la matriz de distancia de Gower para el fold de entrenamiento
  dist_matrix <- proxy::dist(train_df, method = "Gower")
  
  # Calcular la matriz de distancia de Gower para el fold de prueba
  test_dist_matrix <- proxy::dist(test_df, train_df, method = "Gower")
  
  # Realizar el clasificador KNN utilizando la distancia de Gower
  k <- 3  # Número de vecinos
  knn_result <- knn(train = dist_matrix, test = test_dist_matrix, cl = train_df$clasePerfiles, k = k)
  knn_result <- factor(knn_result, levels = levels(test_df$clasePerfiles))
  print(confusionMatrix(knn_result,test_df$clasePerfiles))
  # Calcular la precisión para el fold actual
  accuracy[i] <- sum(knn_result == test_df$clasePerfiles) / nrow(test_df)
  
}

# Calcular la precisión promedio
mean_accuracy <- mean(accuracy)

# Imprimir la precisión promedio
print(mean_accuracy)



###KNN LEAVE ONE OUT
library(FNN)
library(cluster)

df_encoded <- data.frame(model.matrix(~.-1, data = datasetKnnPerfiles))

distances <- daisy(df_encoded, metric = "gower")

precisions <- c()
confusion_matrices <- list()
for (i in 1:nrow(datasetKnnPerfiles)) {
  # Divide los datos en conjunto de entrenamiento y prueba para leave-one-out
  train_data <- df_encoded[-i, ]
  test_data <- df_encoded[i, ]
  
  # Divide las etiquetas de clase en conjunto de entrenamiento y prueba
  train_labels <- df$clasePerfiles[-i]
  test_label <- df$clasePerfiles[i]
  
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







  ################# BAYESIANO
datasetBayesPerfiles <- data.frame(Horas.Semana.Divertirse.con.sus.amigos,Materias.Aprobadas.Primer.Semestre,Edad,clasePerfiles)
dim(datasetBayesPerfiles)
datasetBayesPerfiles <- as.data.frame(datasetBayesPerfiles)

# Codificar las variables categóricas como factores
datasetBayesPerfiles$Horas.Semana.Divertirse.con.sus.amigos <- factor(datasetBayesPerfiles$Horas.Semana.Divertirse.con.sus.amigos)
datasetBayesPerfiles$Materias.Aprobadas.Primer.Semestre <- factor(datasetBayesPerfiles$Materias.Aprobadas.Primer.Semestre)
datasetBayesPerfiles$Edad <- factor(datasetBayesPerfiles$Edad)
#  Definir los parámetros de K-Fold Cross Validation
k_folds <- 5

# Realizar la validación cruzada
set.seed(123)  # Establecer una semilla para reproducibilidad
folds <- sample(1:k_folds, nrow(datasetBayesPerfiles), replace = TRUE)  # Asignar aleatoriamente los folds

accuracy <- vector("numeric", k_folds)  # Vector para almacenar las precisiones
matriz <- vector("list", k_folds)  # Vector para almacenar las precisiones

for (i in 1:k_folds) {
  # Separar los datos en conjunto de entrenamiento y prueba para el fold actual
  train_df <- datasetBayesPerfiles[folds != i, ]
  test_df <- datasetBayesPerfiles[folds == i, ]
  
  # Entrenar el clasificador bayesiano
  model <- naiveBayes(clasePerfiles ~ ., data = train_df)
  
  # Realizar predicciones en el conjunto de prueba
  predictions <- predict(model, test_df)
  matriz[[i]] <- confusionMatrix(predictions, test_df$clasePerfiles)
  cat("---------------------------------------------------------------\n")
  cat("Fold", i, ":\n")
  print(confusionMatrix(predictions,test_df$clasePerfiles))
  # Calcular la precisión para el fold actual
  accuracy[i] <- sum(predictions == test_df$clasePerfiles) / nrow(test_df)
  
  # Imprimir resultados del fold actual
  #cat("Fold", i, ":\n")
  #cat("Predicciones:", predictions, "\n")
  #cat("Clases verdaderas:", test_df$clasePerfiles, "\n")
  #cat("Precisión:", accuracy[i], "\n\n")
}

# Calcular la precisión promedio
mean_accuracy <- mean(accuracy)
cat("Precisión promedio:", mean_accuracy, "\n")





#APLICANDO LEAVE ONE OUT
# Crear el modelo de clasificador bayesiano

library(e1071)
precisions <- c()
confusion_matrices <- list()
modelo <- naive_bayes(x = X, y = y)
for (i in 1:nrow(datasetBayesPerfiles)) {
  # Divide los datos en conjunto de entrenamiento y prueba para leave-one-out
  train_data <- datasetBayesPerfiles[-i, ]
  test_data <- datasetBayesPerfiles[i, ]
  
  # Crea el modelo de clasificación Naive Bayes
  model <- naiveBayes(clasePerfiles ~ ., data = train_data)
  
  # Realiza las predicciones en el objeto de prueba
  predicted_labels <- predict(model, test_data)
  
  # Obtiene la etiqueta de clase verdadera del objeto de prueba
  true_label <- test_data$clasePerfiles
  
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
