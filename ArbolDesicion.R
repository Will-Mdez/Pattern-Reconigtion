Color <- c("Amarillo","Amarillo","Verde","Verde","Amarillo","Amarillo","Amarillo","Amarillo","Verde","Amarillo","Amarillo","Amarillo","Amarillo","Amarillo","Amarillo","Amarillo")
Tamano <-c("Pequeña","Pequeña","Pequeña","Grande","Grande","Pequeña","Pequeña","Pequeña","Pequeña","Grande","Grande","Grande","Grande","Grande","Pequeña","Grande")
Forma <- c("Redonda","Redonda","Irregular","Irregular","Redonda","Redonda","Redonda","Redonda","Redonda","Redonda","Redonda","Redonda","Redonda","Redonda","Irregular","Irregular")
ClaseComestible <- c("Si","No","Si","No","Si","Si","Si","Si","No","No","Si","No","No","No","Si","Si")
length(ClaseComestible)

dataHongos <- data.frame(Color,Tamano,Forma,ClaseComestible)
ProbClaseComes <- table(dataHongos$ClaseComestible)/16

P00<-table(dataHongos$Color[dataHongos$ClaseComestible=="Si"])
P01<-table(dataHongos$Color[dataHongos$ClaseComestible=="No"])

P10<-table(dataHongos$Tamano[dataHongos$ClaseComestible=="Si"])
P11<-table(dataHongos$Tamano[dataHongos$ClaseComestible=="No"])

##
P20<-table(dataHongos$Forma[dataHongos$ClaseComestible=="Si"])
P21<-table(dataHongos$Forma[dataHongos$ClaseComestible=="No"])
N <- 16

EV00<--(P00[1]/N*log2(P00[1]/N)+P01[1]/N*log2(P01[1]/N))
EV01<--(P00[2]/N*log2(P00[2]/N)+P01[2]/N*log2(P01[2]/N))
E0<-((P00[1]+P01[1])*EV00+(P00[2]+P01[2])*EV01)/N

EV10<--(P10[1]/N*log2(P10[1]/N)+P11[1]/N*log2(P11[1]/N))
EV11<--(P10[2]/N*log2(P10[2]/N)+P11[2]/N*log2(P11[2]/N))
E1<-((P10[1]+P11[1])*EV10+(P10[2]+P11[2])*EV11)/N

EV20<--(P21[1]/N*log2(P21[1]/N)+P21[1]/N*log2(P21[1]/N))
EV21<--(P20[2]/N*log2(P20[2]/N)+P21[2]/N*log2(P21[2]/N))
E2<-((P20[1]+P21[1])*EV20+(P20[2]+P21[2])*EV21)/16

Ic01 <- -(ProbClaseComes[1]*log2(ProbClaseComes[1])+ProbClaseComes[2]*log2(ProbClaseComes[2]))


Gan01 <- Ic01-E0
Gan02 <- Ic01-E1
Gan03 <- Ic01-E2

indexPeque <- which(Tamano == "Pequeña")
indexGrande <- which(Tamano == "Grande")

P00<-table(dataHongos$Color[dataHongos$ClaseComestible[,indexPeque]=="Si"])
P01<-table(dataHongos$Color[dataHongos$ClaseComestible=="No"])

P10<-table(dataHongos$Tamano[dataHongos$ClaseComestible=="Si"])
P11<-table(dataHongos$Tamano[dataHongos$ClaseComestible=="No"])

##
P20<-table(dataHongos$Forma[dataHongos$ClaseComestible=="Si"])
P21<-table(dataHongos$Forma[dataHongos$ClaseComestible=="No"])
N <- 16

EV00<--(P00[1]/N*log2(P00[1]/N)+P01[1]/N*log2(P01[1]/N))
EV01<--(P00[2]/N*log2(P00[2]/N)+P01[2]/N*log2(P01[2]/N))
E0<-((P00[1]+P01[1])*EV00+(P00[2]+P01[2])*EV01)/N

EV10<--(P10[1]/N*log2(P10[1]/N)+P11[1]/N*log2(P11[1]/N))
EV11<--(P10[2]/N*log2(P10[2]/N)+P11[2]/N*log2(P11[2]/N))
E1<-((P10[1]+P11[1])*EV10+(P10[2]+P11[2])*EV11)/N




calc_entropy <- function(x) {
  prop <- table(x) / length(x)  # Calcula las proporciones de cada nivel
  entropy <- -sum(prop * log2(prop))  # Calcula la entropía utilizando la fórmula de la entropía de Shannon
  return(entropy)
}

entropy_A <- calc_entropy(dataHongos$Color)
entropy_B <- calc_entropy(dataHongos$Tamano)
entropy_C <- calc_entropy(dataHongos$Forma)
entropy_Clase <- calc_entropy(dataHongos$ClaseComestible)

library(entropy)
dataHongos$Color <- factor(dataHongos$Color)
dataHongos$Tamano <- factor(dataHongos$Tamano)
dataHongos$Forma <- factor(dataHongos$Forma)
dataHongos$ClaseComestible <- factor(dataHongos$ClaseComestible)
entropy_A <- entropy(dataHongos$Color)

