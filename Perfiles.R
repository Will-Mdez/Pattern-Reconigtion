#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//ReconocimientoPerfiles_JD.csv"


dataPerfiles <- read.table(namefile, header = TRUE, sep =',')

#Descripción de los datos
##Nombre a las columnas de datos
names(dataPerfiles)
#dimensiones
dim(dataPerfiles)

summary(dataPerfiles)

#Columnas con datos Cualitativos
cualitativos <- c(2,3,5,6,7,8,9,10,11,13,14,15,16,20,21,22,25,26,27,28,30,31,32,33,34,35)
#Columnas con datos Cuantitativos
cuantitativos<- c(1,4,12,17,18,19,23,24,29)

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
sd_features <- sapply(FeatNames, function(x) sd(dataPerfiles_cuanti[[x]]))

#Identificación de valores extremos
dataPerfiles_SE<-filter(dataPerfiles_cuanti,dataPerfiles_cuanti$Edad<(mean_features[1]+3*sd_features[1]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Integrantes.Familia.num<(mean_features[2]+3*sd_features[2]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Preparatoria<(mean_features[3]+3*sd_features[3]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Cursadas.Primer.Semestre<(mean_features[4]+3*sd_features[4]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Promedio.Primer.Semestre<(mean_features[5]+3*sd_features[5]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Materias.Aprobadas.Primer.Semestre<(mean_features[6]+3*sd_features[6]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Examenes<(mean_features[7]+3*sd_features[7]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Promedio.Estudio.Actividades.Escolares<(mean_features[8]+3*sd_features[8]))
dataPerfiles_SE<-filter(dataPerfiles_SE,dataPerfiles_cuanti$Horas.Semana.Divertirse.con.sus.amigos<(mean_features[9]+3*sd_features[9]))

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
dataP_Ord$Promedio.Primer.Semestre <- dataP_Promedio
dataP_Ord$Integrantes.Familia.num <- dataFamilia
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
valores <- c('Cañada', 'Costa', 'Istmo', 'Mixteca', 'Región Cuenca del Papaloapan','Sierra Sur','Sierra Norte', 'Valles Centrales', 'Otro estado')
dataCopy$Region <- match(dataCopy$Region, valores)
valores <- c('Ambos','Mamá','Papá')
dataCopy$Proveedor.Familia <- match(dataCopy$Proveedor.Familia, valores)
valores <- c('Otra persona','Madre','Padre','Pariente')
dataCopy$Tutor <- match(dataCopy$Tutor, valores)
valores <- c('Ingeniería en Computación', 'Ingeniería en Electrónica',
                               'Ingeniería en Mecatrónica', 'Ingeniería en Diseño',
                               'Ingeniería en Alimentos', 'Ingeniería Industrial',
                               'Ingeniería en Física Aplicada', 'Ingeniería en Mecánica Automotriz',
                               'Ingeniería Civil', 'Licenciatura en Ciencias Empresariales',
                               'Licenciatura en Matemáticas Aplicadas')
dataCopy$Carrera <- match(dataCopy$Carrera, valores)
valores <- c('COBAO','CETIS', 'CBTIS', 'Preparatoria-UABJO', 'CONALEP',
                               'Preparatoria/Bachillerato particular', 'Otro')
dataCopy$Preparatoria <- match(dataCopy$Preparatoria, valores)
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

freqClass <- table(dataCopy$Carrera)
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

