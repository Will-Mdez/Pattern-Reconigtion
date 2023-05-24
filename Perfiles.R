#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"


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
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Preparatoria<(mean_features[2]+3*sd_features[2]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Cursadas.Primer.Semestre<(mean_features[3]+3*sd_features[3]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Primer.Semestre<(mean_features[4]+3*sd_features[4]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Aprobadas.Primer.Semestre<(mean_features[5]+3*sd_features[5]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Examenes<(mean_features[6]+3*sd_features[6]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Actividades.Escolares<(mean_features[7]+3*sd_features[7]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Semana.Divertirse.con.sus.amigos<(mean_features[8]+3*sd_features[8]))
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

Entropias<-c(E1,E2,E3,E4,E5,E6,E6,E7,E8,E9,E10,E11,E12,E13,E14,E15,E16,E17,E18,E19,E20,E21,E22,E23,E24,E25,E26,E27)
sort(Entropias)
#La entropía menor corresponde a E0 que es Edad

