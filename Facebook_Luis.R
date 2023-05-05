#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv";
#namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
dataFacebookL <- read.table(namefile, header = TRUE, sep = ",")

namesF <- c("Nro Caracteres del nombre	",
            "Foto de Perfil	",
            "Tiempo en FB al día",
            "Publicaciones al día	",
            "Amigos	",
            "Páginas que siguen	",
            "comenta_publicaciones"
              ,"Día de la semana con más actividad","clase")

##Nombre a las columnas de datos
names(dataFacebookL) <- namesF
names(dataFacebookL)
#dimensiones
dim(dataFacebookL)
#Descripcion de datos
summary(dataFacebookL)

#Convertir los datos a Categoricos
dataFacebookL$clase<-factor(dataFacebookL$clase)
dataFacebookL$`Foto de Perfil`<-factor(dataFacebookL$`Foto de Perfil	`)
dataFacebookL$`Tiempo en FB al día`<-factor(dataFacebookL$`Tiempo en FB al día`)
dataFacebookL$`Publicaciones al día	`<-factor(dataFacebookL$`Publicaciones al día	`)
dataFacebookL$comenta_publicaciones<-factor(dataFacebookL$comenta_publicaciones)
dataFacebookL$`Día de la semana con más actividad`<-factor(dataFacebookL$`Día de la semana con más actividad`)
summary(dataFacebookL)
