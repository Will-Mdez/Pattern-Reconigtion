
tamaño <- c()
intensidad <- c(37.5926,
                48.5556,
                19.0741,14.6296, 11.8519,
                44.8889,
                22.1852,
                49.5926,
                14.1481,
                47.7037,
                25.1111,
                51.7778,
                55.3704,
                47.5926,66.7778)
saturacion <- c(0.319714,
                0.278822,
                0.381867,
                0.416705,
                0.405556,
                0.246605,
                0.411811,
                0.302925,
                0.421621,
                0.290278,
                0.335127,
                0.290148,
                0.268621,
                0.317542,
                0.32276)
hue <- c(-2.13876,
         -1.99604,
         2.39502,
         2.30688,
         2.12865,
         -1.97178,
         2.62471,
         -2.02227,
         2.39249,
         -2.0527,
         2.76904,
         -2.0141, -2.00619,
         -2.12287, -2.12446)
clase <- c("cement",
           "path", "grass",
           "grass",
           "grass","cement",
           "grass","path",
           "grass",
           "path",
           "grass", "path",
           "cement",
           "cement",
           "cement")
DatosImagenes <- cbind(intensidad,saturacion,hue,clase)

normalizeaDataL <- function (dataF, meanF, stdF){
  dataFN <- dataF
  dataFN <- (dataFN - meanF)/stdF
  return(dataFN)
}
DatosImagenes <- as.data.frame(DatosImagenes)
FeatNames<-names(DatosImagenes[1:3])
FeatNames
DatosImagenes$intensidad <- as.numeric(DatosImagenes$intensidad)
DatosImagenes$hue <- as.numeric(DatosImagenes$hue)
DatosImagenes$saturacion <- as.numeric(DatosImagenes$saturacion)
mean_features <- sapply(FeatNames, function(x) mean(DatosImagenes[[x]]))
sd_features <- sapply(FeatNames, function(x) sd(DatosImagenes[[x]]))


dataImageNorm <- lapply(FeatNames, function (x) normalizeaDataL(DatosImagenes[[x]], mean_features[x], sd_features[x]))

names(dataImageNorm)<- FeatNames  
dataImageNorm <- as.data.frame(dataImageNorm)
summary(dataImageNorm)
dataImageNorm$clase <- factor(DatosImagenes$clase)

filter(dataImageNorm$intensidad,dataImageNorm$clase=="cement")

tablaIntensidad <- table(dataImageNorm$intensidad,dataImageNorm$clase="cement")


index_cement <- which(clase == "cement" )

index_path <- which(clase == "path" )
index_grass <- which(clase == "grass" )
mean_cement_intensidad <- mean(dataImageNorm$intensidad[index_cement])
mean_path_intensidad <- mean(dataImageNorm$intensidad[index_path])
mean_grass_intensidad<- mean(dataImageNorm$intensidad[index_grass])

mean_cement_saturacion <- mean(dataImageNorm$saturacion[index_cement])
mean_path_saturacion <- mean(dataImageNorm$saturacion[index_path])
mean_grass_saturacion<- mean(dataImageNorm$saturacion[index_grass])

mean_cement_hue <- mean(dataImageNorm$hue[index_cement])
mean_path_hue <- mean(dataImageNorm$hue[index_path])
mean_grass_hue<- mean(dataImageNorm$hue[index_grass])


var_cement_intensidad <- var(dataImageNorm$intensidad[index_cement])
var_path_intensidad <- var(dataImageNorm$intensidad[index_path])
var_grass_intensidad<- var(dataImageNorm$intensidad[index_grass])

var_cement_saturacion <- var(dataImageNorm$saturacion[index_cement])
var_path_saturacion <- var(dataImageNorm$saturacion[index_path])
var_grass_saturacion<- var(dataImageNorm$saturacion[index_grass])

var_cement_hue <- var(dataImageNorm$hue[index_cement])
var_path_hue <- var(dataImageNorm$hue[index_path])
var_grass_hue<- var(dataImageNorm$hue[index_grass])

ProbClase <- table(dataImageNorm$clase)

#nuevo Objeto
#intensidad =0.1969
#saturacion=-1.43
#hue = 1.12

verosimIntensidad <- 1(sqrt(2*pi*var_cement_intensidad))*exp((0.1969-mean_cement_intensidad)**2/2*var_cement_intensidad)

#Precision, FScore, Recall se Calcula por Clase, 
#