#nameFile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//wine.data";
namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTraining.data";
namefile2 <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001//datasets//wineTest.data";

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

summary(dataWineTrainingNorm)
dim(dataWineTrainingNorm)

euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))
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

