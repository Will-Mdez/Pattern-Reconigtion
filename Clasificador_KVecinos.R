#nameFile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//wine.data";
#namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTraining.data";
#namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTest.data";
namefile <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTraining.data"
namefile2 <- "C://Users//willm//Downloads//1002-A//Metaheuristicas//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTest.data"

dataWineTraining <- read.table(namefile, header = FALSE, sep = ",");
dataWineTest <- read.table(namefile2, header = FALSE, sep = ",");

names(dataWineTraining) <- c("class","Alcohol","Malic_acid","Ash","Alcalinity_of_ash","Mag",
                     "Total phenols","Flavanoids","Nonflavanoid phenols","Proanthocyanins","Color intensity",            "Hue",
                     "OD280/OD315 of diluted wines","Proline")
names(dataWineTest) <- names(dataWineTraining)


summary(dataWineTraining)
summary(dataWineTest)

mean_features <- sapply(seq(1:14),function(x) mean(dataWineTraining[[x]]))
sd_features <- sapply(seq(1:14),function(x) sd(dataWineTraining[[x]]))
print(sd_features)
print(mean_features)


#NOrmalizacion
normalizeDataL <- function(dataF,meanF,stdF){
  dataFN<- dataF
  dataFN<- (dataFN-meanF)/stdF
  return(dataFN)
}
dataWineTrainingNorm <- lapply(seq(1:14),function(x) normalizeDataL(dataWineTraining[[x]],mean_features[x],sd_features[x]))
names(dataWineTrainingNorm)<-names(dataWineTraining)
dataWineTrainingNorm<-as.data.frame(dataWineTrainingNorm)
#head(dataWineNorm)

dataWineTest <- lapply(seq(1:14),function(x) normalizeDataL(dataWineTest[[x]],mean_features[x],sd_features[x]))
names(dataWineTest)<-names(dataWineTraining)
dataWineTest<-as.data.frame(dataWineTest)

summary(dataWineTest)
dim(dataWineTest)
summary(dataWineTrainingNorm)
dim(dataWineTrainingNorm)

euclidiana <- function(a,b) sqrt( sum((a-b) ^ 2))

dataWineTest$class <- factor(dataWineTest$class)
dataWineTraining$class <- factor(dataWineTraining$class)
summary(dataWineTraining)
summary(dataWineTest)
distancia_new<-sapply(seq(1:120),function(x) euclidiana(dataWineTest[1,1:14],dataWineTrainingNorm[x,1:14]))
distancia_new_index <- sort(distancia_new, index.return=TRUE)
distancia_new_index
distancia_new_index$ix[1]
dataWineTest[distancia_new_index$ix[1:3],1]
dataWineTest[distancia_new_index$ix[1:3],1]

dataWineTrainingNorm
mean_df <- colMeans(dataWineTrainingNorm)
sd_df <- apply(dataWineTrainingNorm, 2, sd)


mean_df <- colMeans(dataWineTrainingNorm)

N <- dim(dataWineTrainingNorm)[1]
indObj <- seq(1:N)

distancias <- sapply(indObj,function(x) sapply(indObj, function(y) euclidiana(dataWineTrainingNorm[y,],dataWineTest[x,])))

k <- 3
sort_with_indices <- function(x) {
  sorted <- sort(x)
  indices <- order(x)
  list(sorted = sorted, indices = indices)
}

distancias_index <- apply(distancias, MARGIN = 1, FUN = sort_with_indices)
valores_ordenados <- t(sapply(distancias_index, "[[", "sorted"))
index_ordenados <- t(sapply(distancias_index, "[[", "indices"))

index_ordenados
dim(index_ordenados)
k_cercanos <- t(apply(index_ordenados, 1, function(row) row[1:k]))
k_cercanos

for (i in 1:nrow(k_cercanos)) {
  row_indices <- k_cercanos[i, ]
  class_counts <- table(dataWineTraining$class[row_indices])
  most_frequent_class <- names(class_counts)[which.max(class_counts)]
  print(most_frequent_class)
}



#EJEMPLO
clase <- c(1,1,1,2,2,2,3,3,3)
alcohol <- c(14.23,13.2,13.16,12.33,12.64,13.67,12.93,13.36,13.52)
color <- c(5.64,4.38,5.68,3.27,5.75,3.8,4.6,5.6,4.35)
prolina <- c(1065,1050,1185,680,450,630,600,780,520)

nuevo <- c(11.65,2.6,562)


dataDiamantes <- cbind(clase,alcohol,color,prolina)
names(dataDiamantes) <- c("clase","alcohol","color","prolina")

dataDiamantes<-as.data.frame(dataDiamantes)
summary(dataDiamantes)
mean_features <- sapply(c(1:4),function(x) mean(dataDiamantes[[x]]))
mean_features
sd_features <- sapply(c(1:4),function(x) sd(dataDiamantes[[x]]))
sd_features

dataDiamantesNorm <- lapply(seq(1:4),function(x) normalizeDataL(dataDiamantes[[x]],mean_features[x],sd_features[x]))
nuevoNorm <- normalizeDataL(nuevo,mean_features[2:4],sd_features[2:4])
names(dataDiamantesNorm)<-names(dataDiamantes)
dataDiamantesNorm<-as.data.frame(dataDiamantesNorm)
summary(dataDiamantesNorm)



distancia_new<-sapply(seq(1:9),function(x) euclidiana(nuevoNorm,dataDiamantesNorm[x,2:4]))
distancia_new
distancia_new_index <- sort(distancia_new, index.return=TRUE)
distancia_new_index$ix[1:3]
frec <- table(dataDiamantes[distancia_new_index$ix[1:3],1])
claseN <- sort(frec[1])
nuevo <- c(claseN,nuevo)
nuevo
