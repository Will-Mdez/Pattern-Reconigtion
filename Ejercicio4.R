library(class)

# Cargar el conjunto de datos iris
data(iris)

# Normalizar el conjunto de datos iris
normalized_iris <- scale(iris[, -5])

# Normalizar el nuevo objeto de prueba
new_data <- data.frame(
  sepal_l= c(5.2),
  sepal_w= c(3.2),
  petal_l= c(2.2),
  petal_w= c(1.2)
)
new_data <- scale(new_data)

# Definir el número de vecinos (k)
k <- 5
# Realizar la clasificación k-NN
predicted_class <- knn(normalized_iris, new_data, iris$Species, k)

# Imprimir la clase predicha
print(predicted_class)



# Validacion
# knn 
# bayes ingenuo 
# arboles de decision

namefile <- "C://Users//Alumnos//Documents//GitHub//Pattern-Reconigtion//datasets-20230307T150614Z-001/ datasets//iris.data";
datairis <- read.table(namefile, header = FALSE, sep = ",");
names(datairis) <- c('sepal_length','sepal_width','petal_length','petal_width','class')
datairis$class <- factor(datairis$class)
summary(datairis)

mean_features <- sapply(seq(1:4),function(x) mean(datairis[[x]]))
sd_features <- sapply(seq(1:4),function(x) sd(datairis[[x]]))
print(sd_features)
print(mean_features)

k <- 5

normalizeDataL <- function(dataF,meanF,stdF){
  dataFN<- dataF
  dataFN<- (dataFN-meanF)/stdF
  return(dataFN)
}
datairisNorm <- lapply(seq(1:4),function(x) normalizeDataL(datairis[[x]],mean_features[x],sd_features[x]))
datairisNorm$class <- datairis$class
names(datairisNorm)<-names(datairis)
datairisNorm<-as.data.frame(datairisNorm)

euclidiana <- function(a,b) (sqrt ( sum ((a - b) ^ 2)))

objeto <- c(5.2,3.2,2.2,1.2)
objeto_normalizado <- (objeto-mean_features)/sd_features
objeto_normalizado

distancias<-sapply(seq(1:150),function(x) euclidiana(objeto_normalizado,datairisNorm[x,1:4]))
distancias
distancias_index <- sort(distancias, index.return=TRUE)
distancias_index
k_cercanos <- distancias_index$ix[1:k]
k_cercanos
print(datairisNorm$class[k_cercanos])

class_counts <- table(datairisNorm$class[k_cercanos])
most_frequent_class <- names(class_counts)[which.max(class_counts)]

print(most_frequent_class)