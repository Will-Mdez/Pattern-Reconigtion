namefile <- "C://Users//Alumnos//Downloads//datasets-20230323T150811Z-001//datasets//HIPOTIRO_CORTO.csv"
dataHipoTiro <- read.table(namefile, header = TRUE, sep = ",")

dataHipTir_Cuanti<-dataHipoTiro[,9:12]
summary(dataHipTir_Cuanti)

varianzas<-sapply(c(1,2,3,4), function(x) sd(dataHipTir_Cuanti[,x]))
print(varianzas)
c1<-c(varianzas[1],0,0,0)
c2<-c(0,varianzas[2],0,0)
c3<-c(0,0,varianzas[3],0)
c4<-c(0,0,0,varianzas[4])

matrizMahalanobis<-rbind(c1,c2,c3,c4)
matrizInversa<-solve(matrizMahalanobis)
matrizInversa
distanciaMahalanobis<-function(a,b){
  dist<-sqrt(t(a-b) %*% matrizInversa %*% (a-b))
  return(dist)
}

distprueba<-distanciaMahalanobis(as.vector(dataHipTir_Cuanti[1,]),as.vector(dataHipTir_Cuanti[1,]))
