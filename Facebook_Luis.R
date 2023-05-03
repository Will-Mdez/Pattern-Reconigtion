#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//DatasetsProyecto//Facebook_LUIS.csv"
dataFacebookL <- read.table(namefile, header = TRUE, sep = ",")


##Nombre a las columnas de datos
names(dataFacebookL)
#dimensiones
dim(dataFacebookL)
#Descripcion de datos
summary(dataFacebookL)